#  FUNCTION: gh_token -------------------------------------------------------------------------
#
#' Get a token for accessing GitHub
#'
#' This function initiates the OAuth authentication code grant flow with GitHub. The user
#' will be redirected to GitHub's login page to enter a user name and password. Once complete
#' the user can close the window and this function returns a token which can be used for all
#' calls to the githapi API.
#'
#' For non-interactive processes a personal access token can be specified, either as an R
#' option (`"github.token"`) or as an environment variable (either `GITHUB_TOKEN` or
#' `GITHUB_PAT`).
#'
#' @param github_token (string, optional) A personal access token. Can be set in the
#'   `github.token` option or the `GITHUB_TOKEN` environment variable. Default: `NULL`.
#' @param github_oauth_url (string, optional) The base URL for for the OAuth endpoint. Can
#'   be set in the `github.oauth_url` option or the `GITHUB_OAUTH_URL` environment variable.
#'    Default: `"https://github.com/login/oauth"`.
#' @param githapi_key (string, optional) The application ID for accessing githapi. Default:
#'   Set in the `githapi.key` option or the `GITHAPI_KEY` environment variable.
#' @param githapi_secret (string, optional) The secret for the application to access githapi.
#'   Default: Set in the `githapi.secret` option or the `GITHAPI_SECRET` environment variable.
#' @param githapi_cache (boolean or string, optional) The location to store a cached token.
#'   If `TRUE` the cache uses the httr default; if `FALSE` it does not cache. Can be set
#'   in the `"githapi.cache"` option or the `GITHAPI_CACHE` environment variable. Default:
#'   `FALSE`.
#'
#' @return A token which is either a string, for a personal access token, or a [httr::Token]
#'   object for an OAuth token.
#'
#' @export
#'
gh_token <- function(
  github_token     = getOption("github.token"),
  github_oauth_url = getOption("github.oauth_url"),
  githapi_key      = getOption("githapi.key"),
  githapi_secret   = getOption("githapi.secret"),
  githapi_cache    = getOption("githapi.cache"))
{
  if (!is_null(github_token))
  {
    assert(is_sha(github_token), "'github_token' must be a 40 character string: '", github_token, "'")
    info("> Using personal access token", level = 6)
    return(github_token)
  }
  else if (!is_null(cache$token))
  {
    info("> Retrieving cached token", level = 6)
    return(cache$token)
  }

  assert(is_scalar_character(github_oauth_url), "'github_oauth_url' must be a string: '", github_oauth_url, "'")
  assert(is_scalar_character(githapi_key), "'githapi_key' must be a string: '", githapi_key, "'")
  assert(is_scalar_character(githapi_secret), "'githapi_secret' must be a string: '", githapi_secret, "'")
  assert(is_scalar_logical(githapi_cache) || is_scalar_character(githapi_cache), "'githapi_cache' must be a boolean or a string: '", githapi_cache, "'")

  github_endpoint <- httr::oauth_endpoint(
    authorize = file.path(github_oauth_url, "authorize"),
    access    = file.path(github_oauth_url, "access_token"))

  githapi_app <- httr::oauth_app(
    appname = "githapi",
    key     = githapi_key,
    secret  = githapi_secret)

  info("> Retrieving new token", level = 6)
  token <- try_catch(httr::oauth2.0_token(
    endpoint = github_endpoint,
    app      = githapi_app,
    cache    = githapi_cache))

  if (("error" %in% names(token$credentials)) && (nchar(token$credentials$error) > 0))
  {
    error(
      "GitHub token request failed:\n",
      "\n[Error]       ", str_replace_all(token$credentials$error, "_", " "),
      "\n[Description] ", str_replace_all(token$credentials$error_description, "\\+", " "),
      "\n[Details]     ", token$credentials$error_uri)
  }

  cache$token <- token
  token
}

#  FUNCTION: gh_url ----------------------------------------------------------------------------
#
#' Build the URL for the GitHub API
#'
#' Unnamed strings are used to build the path upon the API and named strings are added as
#' queries.
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
#' Send an HTTP request to the GitHub API
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
  token   = gh_token(),
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
  else if ("Token" %in% class(token))
  {
    headers <- c(headers, httr::config(token = token))
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
      "\n[Details] ", parsed_response$documentation_url)
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
#  Print method for the `github` class
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
