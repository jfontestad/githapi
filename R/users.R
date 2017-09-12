#  FUNCTION: gh_user --------------------------------------------------------------------------
#' Get a single user
#'
#' \url{https://developer.github.com/v3/users/#get-a-single-user}
#'
#' @param user (string) The GitHub username of the user.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the user (see GitHub's API documentation for details).
#' @export
gh_user <- function(
  user,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_url("users", user, api = api) %>%
      gh_get(token = token, ...)
  }))

  if (is(response, "try-error") || response == "") {
    stop("Specified user does not exist in GitHub: '", user, "'")
  }

  response
}

#  FUNCTION: gh_users -------------------------------------------------------------------------
#' Get all users
#'
#' \url{https://developer.github.com/v3/users/#get-all-users}
#'
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the users (see GitHub's API documentation for more detail).
#' @export
gh_users <- function(
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("users", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select(login, type, html_url, url)
}

#  FUNCTION: gh_user_email --------------------------------------------------------------------
#' List email addresses for the authenticated user
#'
#' url{https://developer.github.com/v3/users/emails/#list-email-addresses-for-a-user}
#' url{https://developer.github.com/v3/users/emails/#list-public-email-addresses-for-a-user}
#'
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the user's email addresses (see GitHub's API documentation for
#'   more detail).
#' @export
gh_user_email <- function(
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("user/emails", api = api) %>%
    gh_get(simplify = TRUE, token = token, ...)
}
