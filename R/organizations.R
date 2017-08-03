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

#  FUNCTION: gh_member ------------------------------------------------------------------------
#' Check membership
#'
#' url{https://developer.github.com/v3/orgs/members/#check-membership}
#'
#' @param user (string, optional) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return TRUE if the user is a member, FALSE otherwise (see GitHub's API documentation for
#'   details).
#' @export
gh_member <- function(
  user,
  org,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(org))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, {
    gh_url("orgs", org, "members", user, api = api) %>%
      gh_get(token = token, ...)
  })

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_members -----------------------------------------------------------------------
#' Members list
#' url{https://developer.github.com/v3/orgs/members/#members-list}
#'
#' @param org (string) The name of the organization.
#' @param filter (string) Filter members returned in the list. Can be one of:
#'   \itemize{
#'     \item 2fa_disabled: Members without two-factor authentication enabled. Available for
#'       organization owners.
#'     \item all: All members the authenticated user can see.
#'     \item Default: all.
#'   }
#' @param role (string) Filter members returned by their role. Can be one of:
#'   \itemize{
#'     \item all: All members of the organization, regardless of role.
#'     \item admin: Organization owners.
#'     \item member: Non-owner organization members.
#'     \item Default: all.
#'   }
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the members (see GitHub's API documentation for details).
#' @export
gh_members <- function(
  org,
  filter = NULL,
  role   = NULL,
  token  = gh_token(),
  api    = getOption("github.api"),
  ...)
{
  assert_that(is.string(org))
  assert_that(is.null(filter) | is.string(filter))
  assert_that(is.null(role) | is.string(role))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("orgs", org, "members", filter = filter, role = role, api = api) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    select(id, login, type, site_admin, url)
}

#  FUNCTION: gh_membership --------------------------------------------------------------------
#' Get organization membership
#'
#' url{https://developer.github.com/v3/orgs/members/#get-organization-membership}
#'
#' @param user (string, optional) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the users membership (see GitHub's API documentation for details).
#' @export
gh_membership <- function(
  user,
  org,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(org))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("orgs", org, "memberships", user, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_memberships -------------------------------------------------------------------
#' List your organization memberships
#'
#' url{https://developer.github.com/v3/orgs/members/#list-your-organization-memberships}
#' url{https://developer.github.com/v3/orgs/members/#get-your-organization-membership}
#'
#' @param org (string, optional) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing your memberships (see GitHub's API documentation for details).
#' @export
gh_memberships <- function(
  org,
  filter = NULL,
  role   = NULL,
  token  = gh_token(),
  api    = getOption("github.api"),
  ...)
{
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(org)) {
    url <- gh_url("user/memberships/orgs", api = api)
  } else {
    assert_that(is.string(org))
    url <- gh_url("user/memberships/orgs", org, api = api)
  }

  url %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows()
}
