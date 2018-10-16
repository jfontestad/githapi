#  PACKAGE: githapi ---------------------------------------------------------------------------
#
#' User-friendly access to the GitHub API for R, consistent with the tidyverse.
#'
#' Provides a suite of functions which simplify working with GitHub's API.
#'
#' @import curl jsonlite dplyr
#' @importFrom methods as is
#' @importFrom utils unzip
#'
#' @docType package
#' @name githapi
NULL

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
  is_string(x) && identical(nchar(x), 40L) && grepl("[0-9a-f]", x)
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
  is_string(x) && identical(length(strsplit(x, "/")[[1]]), 2L)
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
  assert(is_url(api))

  url  <- api
  dots <- unlist(list(...))

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
  assert(is_string(accept))
  assert(is_boolean(parse))
  assert(is_string(token))

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> GET: ", url, level = 2)
  response <- curl_fetch_memory(url, handle = handle_setheaders(
    new_handle(),
    Authorization = paste("token", token),
    Accept        = accept))

  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]]))
  names(header_values) <- sapply(response_header, function(h) h[[1]])

  if (parse) {
    info("> Parsing response", level = 2)
    response_content <- rawToChar(response$content)
    if (grepl("json$", tolower(accept))) {
      response_content <- fromJSON(response_content, simplifyVector = FALSE)
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

  info("> Done", level = 2)
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
  token  = gh_token(),
  ...)
{
  assert(is_url(url))
  assert(is_string(accept))
  assert(is_integer(n_max), n_max > 0)
  assert(is_string(token))

  per_page <- min(n_max, 100)
  if (grepl("\\?", url)) {
    url <- paste0(url, "&per_page=", per_page)
  } else {
    url <- paste0(url, "?per_page=", per_page)
  }

  response <- list()

  while (length(response) < n_max) {
    page <- gh_get(url = url, accept = accept, token = token, ...)
    response <- c(response, page)
    if (length(response) >= n_max) {
      info("> Returned ", length(response), level = 2)
      return(response[1:n_max])
    }
    if (is_null(attributes(page)[["header"]][["Link"]])) {
      info("> Returned ", length(response), level = 2)
      return(response)
    }
    links <- strsplit(attributes(page)[["header"]][["Link"]], ", ")[[1]]
    if (!any(grepl("next", links))) {
      info("> Returned ", length(response), level = 2)
      return(response)
    }
    url <- sub("<", "", strsplit(links[grepl("next", links)], ">")[[1]][[1]])
  }
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
  token = gh_token())
{
  assert(is_url(url))
  assert(is_writeable(dirname(path)))
  assert(is_string(token))

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  info("> DOWNLOAD: ", url, level = 2)
  response <- curl_fetch_disk(url, path, handle = handle_setheaders(
    new_handle(),
    Authorization = paste("token", token),
    Accept = "application/vnd.github.raw"))

  info("> Parsing response", level = 2)
  response_header  <- strsplit(parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]]))
  names(header_values) <- sapply(response_header, function(h) h[[1]])

  if (!is_null(response$status) && response$status >= 400) {
    error(
      "\nGitHub GET request failed\n",
      "\n[Status]:  ", header_values[[1]],
      "\n[URL]:     ", url,
      "\n[Path]:    ", path)
  }

  attributes(path) <- c(attributes(path), list(header = header_values))

  info("> Done", level = 2)
  path
}
