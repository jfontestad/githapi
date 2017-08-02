#  FUNCTION: gh_organization ------------------------------------------------------------------
#' Get an organization
#'
#' url{https://developer.github.com/v3/orgs/#get-an-organization}
#'
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the organization (see GitHub's API documentation for details).
#' @export
gh_organization <- function(
  org,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(org))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("orgs", org, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_organizations -----------------------------------------------------------------
#' List organizations
#'
#' url{https://developer.github.com/v3/orgs/#list-all-organizations}
#' url{https://developer.github.com/v3/orgs/#list-user-organizations}
#'
#' @param user (string, optional) The GitHub username of the user.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the user (see GitHub's API documentation for details).
#' @export
gh_organizations <- function(
  user,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(user)) {
    assert_that(is.string(user))
    url <- gh_url("users", user, "orgs", api = api)
  } else {
    url <- gh_url("organizations", api = api)
  }

  url %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    select(id, name = login, description, url)
}
