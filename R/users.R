#  FUNCTION: gh_user --------------------------------------------------------------------------
#
#' Get a single user
#'
#' <https://developer.github.com/v3/users/#get-a-single-user>
#'
#' @param user (string) The GitHub username of the user.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the user (see GitHub's API documentation for details).
#'
#' @export
#'
gh_user <- function(
  user,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(user))
  assert(is_sha(token))
  assert(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(gh_url("users", user, api = api), token = token, ...)
  }))

  if (is(response, "try-error") || response == "") {
    error("Specified user does not exist in GitHub: '", user, "'")
  }

  response
}

#  FUNCTION: gh_users -------------------------------------------------------------------------
#
#' Get all users
#'
#' <https://developer.github.com/v3/users/#get-all-users>
#'
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the users (see GitHub's API documentation for more detail).
#'
#' @export
#'
gh_users <- function(
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  users <- gh_page(gh_url("users", api = api), n_max = n_max, token = token, ...)

  bind_fields(users, list(
    login    = c("login",    as = "character"),
    type     = c("type",     as = "character"),
    html_url = c("html_url", as = "character"),
    url      = c("url",      as = "character")))
}

#  FUNCTION: gh_user_email --------------------------------------------------------------------
#
#' List email addresses for the authenticated user
#'
#' <https://developer.github.com/v3/users/emails/#list-email-addresses-for-a-user>
#' <https://developer.github.com/v3/users/emails/#list-public-email-addresses-for-a-user>
#'
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the user's email addresses (see GitHub's API documentation for
#'   more detail).
#'
#' @export
#'
gh_user_email <- function(
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_sha(token))
  assert(is_url(api))

  emails <- gh_get(gh_url("user/emails", api = api), token = token, ...)

  bind_fields(emails, list(
    email      = c("email",      as = "character"),
    primary    = c("primary",    as = "logical"),
    verified   = c("verified",   as = "logical"),
    visibility = c("visibility", as = "character")))
}
