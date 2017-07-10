#' githapi: User-friendly access to the GitHub API for R, consistent with the tidyverse.
#'
#' Provides a suite of functions which simplify working with GitHub's API.
#'
#' @import assertthat httr jsonlite stringr tibble readr purrr dplyr
#' @docType package
#' @name githapi
NULL

#  FUNCTION: gh_url ----------------------------------------------------------------------------
#' Build the URL for the github API
#'
#' @param ... (strings, optional) unnamed strings are built up into a URL path and named
#'   parameters are added as queries.
#' @param api (string, optional) the base address of GitHub's API. Default:
#'   \code{getOption("github.api")}
#' @return Valid URL (string)
#' @export
gh_url <- function(
  ...,
  api = getOption("github.api"))
{
  address <- parse_url(api)
  dots <- flatten(list(...))

  if (is.null(names(dots))) {
    path <- str_c(dots, collapse = "/")
  } else {
    path <- str_c(dots[names(dots) == ""], collapse = "/")
  }
  if (!identical(length(path), 0L))
    address$path <- file.path(address$path, path)

  query <- dots[names(dots) != "" & !map_lgl(dots, is.null)]
  if (!identical(length(query), 0L))
    address$query <- c(address$query, query)

  build_url(address)
}

#  FUNCTION: gh_get ---------------------------------------------------------------------------
#' Get the contents of a github http request as a string.
#' @export
gh_get <- function(
  address,
  binary     = FALSE,
  github_url = getOption("github.url"),
  token      = gh_token())
{
  request <- GET(
    address,
    add_headers(
      Accept = "application/vnd.github.raw",
      Authorization = paste("token", token)))
  stop_for_status(request)

  if (binary) {
    content(request, "raw")
  } else {
    content(request, "text")
  }
}

#  FUNCTION: gh_page ---------------------------------------------------------------------------
#' Get and parse the contents of a github http request, including multiple pages.
#' @export
gh_page <- function(
  address,
  ...,
  page_size  = 100L,
  max_pages  = 100L,
  github_url = getOption("github.url"),
  token      = gh_token())
{
  page_result <- function(page_url) {
    page_url %>%
      build_url %>%
      gh_get(..., github_url = github_url, token = token) %>%
      fromJSON(simplifyDataFrame = FALSE)
  }

  page_url <- parse_url(address)
  page_url$query <- c(page_url$query, list(per_page = page_size, page = 1L))
  result <- page_result(page_url)

  if (identical(length(result), page_size)) {
    for (p in 2:max_pages) {
      page_url$query$page <- p
      result <- c(result, page_result(page_url))
      if (!identical(length(result), page_size * p))
        break
    }
  }

  result
}

#  FUNCTION: gh_post --------------------------------------------------------------------------
#' @export
gh_post <- function(
  address,
  ...,
  binary     = FALSE,
  github_url = getOption("github.url"),
  token      = gh_token())
{
  github_app <- oauth_app(app_name, key = app_key, secret = app_secret)

  github <- oauth_endpoint(
    NULL,
    authorize = "authorize",
    access = "access_token",
    base_url = file.path(github_url, "login/oauth"))

  github_token <- oauth2.0_token(endpoint = github, app = github_app, cache = token_cache)

  body <- list(...)
  print(body)

  response <- POST(
    address,
    body = list(...),
    encode = "json",
    add_headers(
      Accept = "application/vnd.github.v3+json",
      Authorization = paste("token", token)))
  stop_for_status(response)

  if (binary)
    content(response, as = NULL, type = "raw")
  else
    content(response, as = "text", type = NULL)
}
