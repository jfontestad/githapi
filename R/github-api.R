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
  assert(is_url(api), "'api' must be a valid URL: '", api, "'")

  url  <- httr::parse_url(api)

  dots <- list(...) %>% compact() %>% unlist()

  if (is_null(names(dots)))
  {
    path <- str_c(dots, collapse = "/")
  }
  else
  {
    path <- str_c(dots[names(dots) == ""], collapse = "/")
  }

  if (!identical(length(path), 0L))
  {
    url$path <- path
  }

  query <- as.list(dots[names(dots) != ""])

  if (!identical(length(query), 0L))
  {
    url$query <- query
  }

  httr::build_url(url)
}

#  FUNCTION: gh_request --------------------------------------------------------------------
#
#' Send an HTTP request to an github compatible API
#'
#' This function can be used to make "GET", "POST", "PATCH", "PUT" or "DELETE" requests to
#' the specified URL.
#'
#' The response is parsed from either raw binary formation, JSON or plain text, depending
#' on the format received. When no reponse is received an empty list returned by the
#' function. Details of the response are recorded as attributes.
#'
#' For "POST", "PATCH", "PUT" and "DELETE" requests a payload can be supplied. It is parsed
#' into a JSON format before being sent to the URL.
#'
#' If an error is returned from the API then an error is thrown by this function, detailing
#' the URL, the HTTP status code and a message from the API, if there is one.
#'
#' Finally, an authorisation token can be supplied if it is required.
#'
#' @param url (string) The address of the API endpoint.
#' @param type (string) The type of HTTP request. Either "GET", "POST" or "PATCH".
#' @param payload (list, optional) The information to send to the API for "POST" and
#'   "PATCH" requests. Default: `NULL`.
#' @param accept (string, optional) The mime format to accept when making the call. Default:
#'   "json".
#' @param headers (character, optional) Headers to add to the request. Default: `NULL`.
#' @param token (string, optional) An authorisation token to include with the request.
#'   Default: `NULL`.
#' @param proxy (character, optional) The proxy server to use to connect to the github API.
#'   Default: Set in the option `github.proxy`.
#'
#' @return A `github` list object consisting of the response, parsed into a list, with the
#'   attributes:
#'   - **url**: The URL the request was sent to
#'   - **request**: The type of HTTP request made
#'   - **status**: The HTTP status code returned
#'   - **header**: The HTTP header returned
#'
#' @export
#'
gh_request <- function(
  url,
  type,
  payload = NULL,
  accept  = "json",
  headers = NULL,
  token   = getOption("github.token"),
  proxy   = getOption("github.proxy"))
{
  assert(is_url(url), "'url' must be a valid URL: '", url, "'")
  assert(is_scalar_character(type), "'type' must be a string: '", type, "'")
  assert(type %in% c("GET", "POST", "PATCH", "PUT", "DELETE"), "'type' must be either 'GET', 'POST', 'PATCH','PUT' or 'DELETE': '", type, "'")
  assert(is_scalar_character(accept), "'accept' must be a string: '", accept, "'")
  assert(is_null(headers) || is_character(headers), "'headers' must be a character vector: '", headers, "'")

  headers <- httr::add_headers(.headers = headers)

  if (!is_null(payload))
  {
    assert(is_list(payload), "'payload' must be a list: '", payload, "'")
    info("> Parsing payload", level = 6)
    payload <- jsonlite::toJSON(payload, auto_unbox = TRUE, null = "null", na = "null")
    headers <- c(headers, httr::content_type_json())
  }

  if (identical(accept, "raw"))
  {
    headers <- c(headers, httr::accept("application/vnd.github.raw"))
  }
  else if (identical(accept, "json"))
  {
    headers <- c(headers, httr::accept("application/vnd.github.v3+json"))
  }
  else
  {
    assert(str_detect(accept, "^application/"), "'accept' is not a valid mime: '", accept, "'")
    headers <- c(headers, httr::accept(accept))
  }

  if (is_sha(token))
  {
    headers <- c(headers, httr::add_headers(Authorization = str_c("token ", token)))
  }
  else
  {
    error("'token' specified is not valid")
  }

  if (!is_null(proxy))
  {
    assert(is_scalar_character(proxy), "'proxy' must be a string: '", proxy, "'")
    httr::set_config(httr::use_proxy(proxy))
    httr::set_config(httr::config(connecttimeout = 60))
    on.exit(httr::reset_config())
  }

  info("> ", type, ": ", url, level = 3)
  response <- switch(
    type,
    GET    = httr::GET(   url,                 headers),
    POST   = httr::POST(  url, body = payload, headers),
    PATCH  = httr::PATCH( url, body = payload, headers),
    PUT    = httr::PUT(   url, body = payload, headers),
    DELETE = httr::DELETE(url, body = payload, headers))

  info("> Parsing response", level = 6)
  if (identical(httr::status_code(response), 204L))
  {
    parsed_response <- list()
  }
  else if (identical(httr::http_type(response), "application/octet-stream"))
  {
    parsed_response <- httr::content(response, type = "raw")
  }
  else if (identical(httr::http_type(response), "application/json"))
  {
    parsed_response <- httr::content(response, as = "text", encoding = "UTF-8") %>%
      jsonlite::fromJSON(simplifyVector = FALSE)
  }
  else
  {
    parsed_response <- httr::content(response, as = "text", encoding = "UTF-8")
  }

  if (httr::http_error(response))
  {
    error(
      "GitHub ", type, " request failed:\n",
      "\n[Status]  ", httr::status_code(response),
      "\n[URL]     ", url,
      "\n[Message] ", str_replace_all(parsed_response$message, "\\n\\n", "\n  "),
      "\n  Further details: ", str_c(parsed_response$documentation_url, collapse = "\n  "))
  }

  structered_response <- structure(
    parsed_response,
    class   = c("github", class(parsed_response)),
    url     = url,
    request = type,
    status  = httr::status_code(response),
    header  = httr::headers(response))

  info("> Done", level = 9)
  structered_response
}

#  FUNCTION: print.github -----------------------------------------------------------------------
#
#  Print method for `github` class
#
#' @export
#'
print.github <- function(x, n_urls = 2, ...)
{
  urls <- attr(x, "url")
  if (length(urls) > n_urls)
  {
    urls <- c(urls[1:n_urls], "...")
  }

  cat("\033[34m", str_c("# ", attr(x, "request"), " \033[4m", str_replace_all(urls, "%20", " "), "\033[24m\n"), "\033[39m", sep = "")

  if (is.data.frame(x))
  {
    class(x) <- class(x)[class(x) != "github"]
    print(x)
  }
  else if (is_list(x))
  {
    utils::capture.output(utils::str(x, max.level = 2, give.attr = FALSE, strict.width = "cut")) %>%
      str_replace("\\.\\.\\$", " ")  %>% str_replace("\\$", "") %>% cat(sep = "\n")
  }
  else
  {
    added_attributes <- c("class", "url", "request", "status", "header", "github")
    attributes(x) <- attributes(x)[!names(attributes(x)) %in% added_attributes]
    print.default(x, ...)
  }

  invisible(x)
}
