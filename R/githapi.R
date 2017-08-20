#' User-friendly access to the GitHub API for R, consistent with the tidyverse.
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
#' Get the contents of a github http request.
#' @export
gh_get <- function(
  address,
  binary = FALSE,
  accept = "application/vnd.github.raw",
  token  = gh_token())
{
  request <- GET(
    address,
    add_headers(
      Accept = accept,
      Authorization = paste("token", token)))
  stop_for_status(request)

  if (binary) {
    content(request, "raw")
  } else {
    content(request, "text")
  }
}

#  FUNCTION: gh_json --------------------------------------------------------------------------
#' Get the JSON contents of a GitHub request and parse into a list.
#' @export
gh_json <- function(
  address,
  accept = "application/vnd.github.v3+json",
  token  = gh_token())
{
  gh_get(address, binary = FALSE, accept = accept, token = token) %>%
    fromJSON(simplifyDataFrame = FALSE)
}

#  FUNCTION: gh_page ---------------------------------------------------------------------------
#' Get and parse the contents of a github http request, including multiple pages.
#' @export
gh_page <- function(
  address,
  ...,
  n_max = 1000L,
  token = gh_token())
{
  pages <- page_size(n_max)
  page_url <- parse_url(address)

  result <- list()
  for (page in seq_along(pages)) {
    page_url$query <- c(page_url$query, list(per_page = pages[page], page = page))
    result <- c(result, build_url(page_url) %>% gh_json(token = token, ...))
  }

  result
}

#  FUNCTION: gh_tibble ------------------------------------------------------------------------
#' Get and parse the contents of a github http request, including multiple pages and return a
#' tibble.
#' @export
gh_tibble <- function(
  address,
  ...,
  n_max = 1000L,
  token = gh_token())
{
  gh_page(address, ..., n_max = n_max) %>%
    map(flatten_) %>%
    bind_rows()
}
