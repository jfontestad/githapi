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
#
# @return The token as a string
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

# FUNCTION: gh_rate_limit ---------------------------------------------------------------------
# Get your current rate limit status
#
# url{https://developer.github.com/v3/rate_limit/#get-your-current-rate-limit-status}
#
# @param warning_limit (numeric) The fraction of calls remaining below which a warning is made.
# @param token (string, optional) The personal access token for GitHub authorisation. Default:
#   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
# @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
# @return A tibble describing the rate limits (see GitHub's API documentation for details).
gh_rate_limit <- function(
  warning_limit = 0.1,
  token         = gh_token(),
  api           = getOption("github.api"))
{
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  request <- gh_url("rate_limit", api = api) %>%
    GET(add_headers(Accept = "application/vnd.github.v3+json", Authorization = paste("token", token)))
  stop_for_status(request)

  rate <- content(request, "text") %>%
    fromJSON(simplifyDataFrame = FALSE) %>%
    .[["resources"]]

  if (rate$core$remaining < 1) {
    stop(
      "GitHub rate limit reached: no more API calls until reset time: ",
      as.POSIXct(rate$core$reset, origin = "1970-01-01", tz = "GMT"))
  } else if (rate$core$remaining / rate$core$limit < warning_limit) {
    warning(
      "GitHub rate limit nearly reached: ", rate$core$remaining, " API calls remaining.\n  Reset time: ",
      as.POSIXct(rate$core$reset, origin = "1970-01-01", tz = "GMT"))
  }

  bind_rows(rate) %>%
    bind_cols(type = names(rate)) %>%
    mutate(reset = as.POSIXct(reset, origin = "1970-01-01", tz = "GMT")) %>%
    select(type, everything())
}

# FUNCTION: gh_url -----------------------------------------------------------------------------
# Build the URL for the github API
#
# @param ... (strings, optional) unnamed strings are built up into a URL path and named
#   parameters are added as queries.
# @param api (string, optional) the base address of GitHub's API. Default:
#   \code{getOption("github.api")}
# @return Valid URL (string)
# @export
gh_url <- function(
  ...,
  api = getOption("github.api"))
{
  assert_that(is.string(api))

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

# FUNCTION: gh_get ----------------------------------------------------------------------------
# Get the contents of a github http request.
#
# @param address (string) URL for GitHub API endpoint.
# @param sub_list (string) A sub-list of the returned list.
# @param simplify (boolean) Whether to simplify the parsed JSON into tibbles.
# @param binary (boolean) Whether to return binary format.
# @param accept (string) The format of the returned result. Either "json", "raw" or other
#   GitHub accepted format. Default: "json".
# @param token (string, optional) The personal access token for GitHub authorisation. Default:
#   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
# @return Either a binary, text string or a list / tibbles from a parsed JSON string.
# @export
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

  header <- headers(request)
  if (!is.null(header$`x-ratelimit-limit`) &&
    as.numeric(header$`x-ratelimit-remaining`) / as.numeric(header$`x-ratelimit-limit`) < 0.1) {
    warning(
      "GitHub rate limit nearly reached: ", header$`x-ratelimit-remaining`, " API calls remaining.\n  Reset time: ",
      as.POSIXct(as.numeric(header$`x-ratelimit-reset`), origin = "1970-01-01", tz = "GMT"))
  }

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
# Get and parse the contents of a github http request, including multiple pages.
#
# @param address (string) URL for GitHub API endpoint.
# @param sub_list (string) A sub-list of the returned list.
# @param simplify (boolean) Whether to simplify the parsed JSON into tibbles.
# @param accept (string) The format of the returned result. Either "json", "raw" or other
#   GitHub accepted format. Default: "json".
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
# @param token (string, optional) The personal access token for GitHub authorisation. Default:
#   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
# @return A list if simplify = FALSE, or a tibble if TRUE.
# @export
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
