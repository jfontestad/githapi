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
#' @param ... Parameters passed to \code{\link{gh_page}}.
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

  response <- try(silent = TRUE, {
    gh_url("users", user, api = api) %>% gh_page(token = token, ...)
  })

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
#' @param limit (integer, optional) The maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the users (see GitHub's API documentation for more detail).
#' @export
gh_users <- function(
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("users") %>%
    gh_page(token = token, ...) %>%
    .[] %>%
    bind_rows %>%
    select(login, type, html_url, url)
}
