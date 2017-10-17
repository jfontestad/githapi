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
#' @param ... Parameters passed to \code{\link{gh_get}}.
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
    gh_get(token = token, ...)
}

#  FUNCTION: gh_organizations -----------------------------------------------------------------
#' List organizations
#'
#' url{https://developer.github.com/v3/orgs/#list-all-organizations}
#' url{https://developer.github.com/v3/orgs/#list-user-organizations}
#'
#' @param user (string, optional) The GitHub username of the user. If not specified, all
#'   organizations are returned.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the organizations (see GitHub's API documentation for details).
#' @export
gh_organizations <- function(
  user,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(user)) {
    assert_that(is.string(user))
    url <- gh_url("users", user, "orgs", api = api)
  } else {
    url <- gh_url("organizations", api = api)
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(id, name = login, description, url)
}

#  FUNCTION: is_member ------------------------------------------------------------------------
#' Check membership
#'
#' url{https://developer.github.com/v3/orgs/members/#check-membership}
#'
#' @param user (string) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return TRUE if the user is a member, FALSE otherwise (see GitHub's API documentation for
#'   details).
#' @export
is_member <- function(
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

  response <- try(silent = TRUE, suppressMessages({
    gh_url("orgs", org, "members", user, api = api) %>%
      gh_get(accept = "raw", token = token, ...)
  }))

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_members -----------------------------------------------------------------------
#' Get Organization or team members list
#'
#' url{https://developer.github.com/v3/orgs/members/#members-list}
#' url{https://developer.github.com/v3/orgs/teams/#list-team-members}
#'
#' Note: Must specify either \code{org} or \code{team}, but not both.
#'
#' @param org (string) The name of the organization.
#' @param team (integer) The GitHub ID of the team.
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
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the members (see GitHub's API documentation for details).
#' @export
gh_members <- function(
  org,
  team,
  filter = NULL,
  role   = NULL,
  n_max = 1000L,
  token  = gh_token(),
  api    = getOption("github.api"),
  ...)
{
  assert_that(is.null(filter) | is.string(filter))
  assert_that(is.null(role) | is.string(role))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(org) && !missing(team))
    stop("Must specify either org or team, not both!")

  if (!missing(org)) {
    assert_that(is.string(org))
    url <- gh_url("orgs", org, "members", filter = filter, role = role, api = api)
  } else if (!missing(team)) {
    assert_that(is.count(team))
    url <- gh_url("teams", team, "members", org, role = role, api = api)
  } else {
    stop("Must specify either org or team!")
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(id, login, type, site_admin, url)
}

#  FUNCTION: gh_membership --------------------------------------------------------------------
#' Get organization and team membership
#'
#' url{https://developer.github.com/v3/orgs/members/#get-organization-membership}
#' url{https://developer.github.com/v3/orgs/teams/#get-team-membership}
#'
#' Note: Must specify either \code{org} or \code{team}, but not both.
#'
#' @param user (string, optional) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param team (integer) The GitHub ID of the team.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the membership (see GitHub's API documentation for details).
#' @export
gh_membership <- function(
  user,
  org,
  team,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(org))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(org) && !missing(team))
    stop("Must specify either org or team, not both!")

  if (!missing(org)) {
    assert_that(is.string(org))
    url <- gh_url("orgs", org, "memberships", user, api = api)
  } else if (!missing(team)) {
    assert_that(is.count(team))
    url <- gh_url("teams", team, "memberships", user, api = api)
  } else {
    stop("Must specify either org or team!")
  }

  url %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_memberships -------------------------------------------------------------------
#' List your organization memberships
#'
#' url{https://developer.github.com/v3/orgs/members/#list-your-organization-memberships}
#' url{https://developer.github.com/v3/orgs/members/#get-your-organization-membership}
#'
#' @param org (string, optional) The name of the organization.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing your memberships (see GitHub's API documentation for details).
#' @export
gh_memberships <- function(
  org,
  n_max = 1000L,
  token  = gh_token(),
  api    = getOption("github.api"),
  ...)
{
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(org)) {
    url <- gh_url("user/memberships/orgs", api = api)
  } else {
    assert_that(is.string(org))
    url <- gh_url("user/memberships/orgs", org, api = api)
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
}

#  FUNCTION: gh_team --------------------------------------------------------------------------
#' Get team
#'
#' url{https://developer.github.com/v3/orgs/teams/#get-team}
#'
#' @param team (integer) The GitHub ID of the team.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the team (see GitHub's API documentation for details).
#' @export
gh_team <- function(
  team,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(team))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("teams", team, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_teams -------------------------------------------------------------------------
#' List teams
#'
#' url{https://developer.github.com/v3/orgs/teams/#list-teams}
#' url{https://developer.github.com/v3/orgs/teams/#list-user-teams}
#'
#' @param org (string, optional) The name of the organization. If org and repo are not
#'   specified, the teams of the authenticated user are returned.
#' @param repo (string, optional) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the teams (see GitHub's API documentation for details).
#' @export
gh_teams <- function(
  org,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(org) && !missing(repo))
    stop("Must specify either org or repo, not both!")

  if (missing(org)) {
    if (missing(repo)) {
      url <- gh_url("user/teams", api = api)
    } else {
      assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
      url <- gh_url("repos", repo, "teams", api = api)
    }
  } else {
    assert_that(is.string(org))
    url <- gh_url("orgs", org, "teams", api = api)
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
}

#  FUNCTION: is_manager -----------------------------------------------------------------------
#' Check if a team manages a repository
#'
#' url{https://developer.github.com/v3/orgs/teams/#check-if-a-team-manages-a-repository}
#'
#' @param team (integer) The GitHub ID of the team.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return TRUE if the team is a manager, FALSE otherwise (see GitHub's API documentation for
#'   details).
#' @export
is_manager <- function(
  team,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(team))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_url("teams", team, "repos", repo, api = api) %>%
      gh_get(accept = "raw", token = token, ...)
  }))

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}
