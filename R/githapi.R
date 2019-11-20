#  FUNCTION: is_sha ---------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid SHA
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid SHA, FALSE otherwise
#'
#' @export
#'
is_sha <- function(x) {
  is_scalar_character(x) && (nchar(x) == 40) && grepl("[0-9a-f]", x)
}

#  FUNCTION: is_repo --------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid repo name
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid repo, FALSE otherwise
#'
#' @export
#'
is_repo <- function(x) {
  is_scalar_character(x) && identical(length(strsplit(x, "/")[[1]]), 2L)
}

#  FUNCTION: gh_token -------------------------------------------------------------------------
#
#' Retrieve the GitHub personal access token
#'
#' The token is retreived by looking in the following locations, in order:
#'
#' 1. `github.token` option
#' 2. `GITHUB_TOKEN` environment variable
#' 3. `GITHUB_PAT` environment variable
#'
#' @return The token as a string
#'
gh_token <- function() {
  token <- getOption("github.token")
  if (is_null(token)) {
    token <- Sys.getenv("GITHUB_TOKEN")
  }
  if (identical(token, "")) {
    token <- Sys.getenv("GITHUB_PAT")
  }
  if (identical(token, "")) {
    error("Cannot find GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")
  }
  token
}

#  FUNCTION: gh_url ----------------------------------------------------------------------------
#
#' Build the URL for the github API
#'
#' @param ... (strings, optional) unnamed strings are built up into a URL path and named
#'   parameters are added as queries.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#'
#' @return Valid URL (string)
#'
#' @export
#'
gh_url <- function(
  ...,
  api = getOption("github.api"))
{
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  url  <- api
  dots <- list(...) %>% remove_missing() %>% unlist()

  if (is_null(names(dots))) {
    path <- paste(dots, collapse = "/")
  } else {
    path <- paste(dots[names(dots) == ""], collapse = "/")
  }
  if (!identical(length(path), 0L))
    url <- file.path(url, path)

  query <- dots[names(dots) != "" & !sapply(dots, is_null)]
  if (!identical(length(query), 0L)) {
    url <- paste0(url, "?", paste(names(query), query, sep = "=", collapse = "&"))
  }

  url
}

#  FUNCTION: gh_get ---------------------------------------------------------------------------
#
#' Send a http GET request to the specified GitHub url
#'
#' @param url (string) URL to the GitHub API.
#' @param accept (string) The format of the returned result. Either "json", "raw" or other
#'   GitHub accepted format. Default: "json".
#' @param parse (boolean) Whether to parse the response.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @return A list of the parsed response
#'
#' @export
#'
gh_get <- function(
  url,
  accept = "json",
  parse  = TRUE,
  token  = gh_token())
{
  assert(is_url(url))
  assert(is_scalar_character(accept))
  assert(is_scalar_logical(parse))
  assert(is_scalar_character(token))

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> GET: ", url, level = 2)
  response <- curl::curl_fetch_memory(url, handle = curl::handle_setheaders(
    curl::new_handle(),
    Authorization = paste("token", token),
    Accept        = accept))

  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(curl::parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (parse) {
    info("> Parsing response", level = 4)
    response_content <- rawToChar(response$content)
    if (grepl("json$", tolower(accept))) {
      response_content <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
      message <- response_content$message
    }
  }

  if (response$status_code >= 400) {
    error(
      "\nGitHub GET request failed\n",
      "\n[Status]: ", header_values$Status,
      "\n[URL]: ", url,
      "\n[Message]: ", message)
  }

  attributes(response_content) <- c(attributes(response_content), list(header = header_values))

  info("> Done", level = 4)
  response_content
}

#  FUNCTION: gh_page --------------------------------------------------------------------------
#
#' Get and parse the contents of multiple pages from a github http request
#'
#' @param url (string) URL for GitHub API endpoint.
#' @param accept (string) The format of the returned result. Either "json", "raw" or other
#'   GitHub accepted format. Default: "json".
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list of the combined parsed responses
#'
#' @export
#'
gh_page <- function(
  url,
  accept = "json",
  n_max  = 1000L,
  token  = getOption("github.token"),
  ...)
{
  {
    (is_url(url)) ||
      error("'url' must be a valid URL:\n  '", paste(url, collapse = "'\n  '"), "'")
    (is_scalar_character(accept)) ||
      error("'accept' must be a string:\n  '", paste(accept, collapse = "'\n  '"), "'")
    (is_scalar_integerish(n_max) && isTRUE(n_max > 0)) ||
      error("'n_max' must be a positive integer:\n  '", paste(n_max, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  }

  per_page <- min(n_max, 100)
  if (grepl("\\?", url)) {
    url <- paste0(url, "&per_page=", per_page)
  } else {
    url <- paste0(url, "?per_page=", per_page)
  }

  response <- list()

  while (length(response) < n_max) {
    page <- gh_request("GET", url = url, accept = accept, token = token, ...)
    response <- c(response, page)
    if (length(response) >= n_max) {
      info("> Returned ", length(response), level = 4)
      return(response[1:n_max])
    }
    if (is_null(attributes(page)[["header"]][["Link"]])) {
      info("> Returned ", length(response), level = 4)
      return(response)
    }
    links <- strsplit(attributes(page)[["header"]][["Link"]], ", ")[[1]]
    if (!any(grepl("next", links))) {
      info("> Returned ", length(response), level = 4)
      return(response)
    }
    url <- sub("<", "", strsplit(links[grepl("next", links)], ">")[[1]][[1]])
  }
}

#  FUNCTION: gh_request -----------------------------------------------------------------------
#
#' Send a http request to the specified GitHub url
#'
#' @param type (string) The type of the request. Either "GET", "POST", "DELETE" or "PATCH".
#' @param url (string) URL to the GitHub API.
#' @param payload (list) The information to send, which is converted to JSON.
#' @param accept (string) The format of the returned result. Either "json", "raw" or other
#'   GitHub accepted format. Default: "json".
#' @param parse (boolean) Whether to parse the response.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @return A list of the parsed response
#'
#' @export
#'
gh_request <- function(
  type,
  url,
  payload = NULL,
  accept  = "json",
  parse   = TRUE,
  token   = getOption("github.token"))
{
  {
    (is_scalar_character(type) && type %in% c("GET", "POST", "DELETE", "PATCH", "PUT")) ||
      error("'type' must be either 'GET', 'POST', 'DELETE', 'PATCH' or 'PUT':\n  '", paste(type, collapse = "'\n  '"), "'")
    (is_url(url)) ||
      error("'url' must be a valid URL:\n  '", paste(url, collapse = "'\n  '"), "'")
    (is_scalar_character(accept)) ||
      error("'accept' must be a string:\n  '", paste(accept, collapse = "'\n  '"), "'")
    (is_scalar_logical(parse)) ||
      error("'parse' must be a boolean:\n  '", paste(parse, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  }

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> ", type, ": ", url, level = 2)
  h <- curl::new_handle()
  if (!identical(type, "GET")) {
    h <- curl::handle_setopt(h, customrequest = type)
  }

  if (!is_null(payload)) {
    (is_list(payload)) ||
      error("'payload' must be a list:\n  '", paste(payload, collapse = "'\n  '"), "'")

    h <- curl::handle_setopt(h, COPYPOSTFIELDS = jsonlite::toJSON(payload, auto_unbox = TRUE))
  }
  h <- curl::handle_setheaders(h, Authorization = paste("token", token), Accept = accept)

  response <- curl::curl_fetch_memory(url, handle = h)
  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(curl::parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (parse) {
    info("> Parsing response", level = 4)
    response_content <- rawToChar(response$content)
    if (!identical(response_content, "") && grepl("json$", tolower(accept))) {
      response_content <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
      message <- response_content$message
    } else {
      message <- "None"
    }
  }

  if (response$status_code >= 400) {
    error(
      "GitHub ", type, " request failed\n",
      "\n[Status]:  ", header_values$Status,
      "\n[URL]:     ", url,
      "\n[Message]: ", message)
  }

  attributes(response_content) <- c(attributes(response_content), list(header = header_values))

  info("> Done", level = 4)
  response_content
}

#  FUNCTION: gh_download_binary ---------------------------------------------------------------
#
#' Download a binary file from GitHub
#'
#' @param url (string) URL to the GitHub API.
#' @param path (string) The location to download the file to.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @return The path to the download location
#'
#' @export
#'
gh_download_binary <- function(
  url,
  path,
  token = getOption("github.token"))
{
  {
    (is_url(url)) ||
      error("'url' must be a valid URL:\n  '", paste(url, collapse = "'\n  '"), "'")
    (is_writeable(dirname(path))) ||
      error("'path' must be a file path within a writeable directory:\n  '", paste(path, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  info("> DOWNLOAD: ", url, level = 2)
  response <- curl::curl_fetch_disk(url, path, handle = curl::handle_setheaders(
    curl::new_handle(),
    Authorization = paste("token", token),
    Accept = "application/vnd.github.raw"))

  info("> Parsing response", level = 4)
  response_header  <- strsplit(curl::parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (!is_null(response$status) && response$status >= 400) {
    error(
      "\nGitHub GET request failed\n",
      "\n[Status]:  ", header_values[[1]],
      "\n[URL]:     ", url,
      "\n[Path]:    ", path)
  }

  attributes(path) <- c(attributes(path), list(header = header_values))

  info("> Done", level = 4)
  path
}
