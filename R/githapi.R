#' User-friendly access to the GitHub API for R, consistent with the tidyverse.
#'
#' Provides a suite of functions which simplify working with GitHub's API.
#'
#' @import assertthat httr jsonlite stringr tibble readr purrr dplyr
#' @docType package
#' @name githapi
NULL

# FUNCTION: gh_token --------------------------------------------------------------------------
# Retrieve the GitHub personal access token from the following locations:
#  - github.token option
#  - GITHUB_TOKEN environment variable
#  - GITHUB_PAT environment variable
gh_token <- function() {
  token <- getOption("github.token")
  if (is.null(token)) {
    token <- Sys.getenv("GITHUB_TOKEN")
  }
  if (identical(token, "")) {
    token <- Sys.getenv("GITHUB_PAT")
  }
  if (identical(token, "")) {
    stop("Cannot find GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")
  }
  token
}

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
  sub_list = NULL,
  simplify = FALSE,
  binary   = FALSE,
  accept   = "json",
  token    = gh_token())
{
  if (identical(accept, "raw") || binary) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  request <- GET(
    address,
    add_headers(
      Accept = accept,
      Authorization = paste("token", token)))
  stop_for_status(request)

  if (binary) {
    result <- content(request, "raw")
  } else if (identical(accept %>% str_sub(., nchar(.) - 3, nchar(.)), "json")) {
    result <- content(request, "text") %>%
      fromJSON(simplifyDataFrame = simplify, flatten = simplify)

    if (!missing(sub_list)) {
      result <- result[[sub_list]]
    }

    if (simplify) {
      result <- result %>%
        as_tibble() %>%
        set_names(str_replace_all(names(.), "\\.", "_"))
    }
  } else {
    result <- content(request, "text")
  }

  result
}

#  FUNCTION: gh_page ---------------------------------------------------------------------------
#' Get and parse the contents of a github http request, including multiple pages.
#' @export
gh_page <- function(
  address,
  sub_list = NULL,
  simplify = FALSE,
  accept   = "json",
  n_max    = 1000L,
  token    = gh_token())
{
  if (n_max <= 100) {
    pages <- n_max
  } else {
    pages <- c(rep(100L, floor(n_max/100)), n_max %% 100L)
  }

  page_url <- parse_url(address)

  if (simplify) {
    result <- tibble()
  } else {
    result <- list()
  }

  for (page in seq_along(pages)) {
    page_url$query <- c(page_url$query, list(per_page = pages[page], page = page))
    if (simplify) {
      result <- bind_rows(
        result,
        build_url(page_url) %>% gh_get(token = token, simplify = simplify, accept = accept))
    } else {
      result <- c(
        result,
        build_url(page_url) %>% gh_get(token = token, simplify = simplify, accept = accept))
    }
  }

  result
}
