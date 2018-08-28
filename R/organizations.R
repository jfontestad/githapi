#  FUNCTION: gh_organization ------------------------------------------------------------------
#
#' Get an organization
#'
#' <https://developer.github.com/v3/orgs/#get-an-organization>
#'
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the organization (see GitHub's API documentation for details).
#'
#' @export
#'
gh_organization <- function(
  org,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_string(org))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_get(gh_url("orgs", org, api = api), token = token, ...)
}

#  FUNCTION: gh_organizations -----------------------------------------------------------------
#
#' List organizations
#'
#' <https://developer.github.com/v3/orgs/#list-all-organizations>
#' <https://developer.github.com/v3/orgs/#list-user-organizations>
#'
#' @param user (string, optional) The GitHub username of the user. If not specified, all
#'   organizations are returned.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the organizations (see GitHub's API documentation for details).
#'
#' @export
#'
gh_organizations <- function(
  user,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (!missing(user)) {
    stopifnot(is_string(user))
    url <- gh_url("users", user, "orgs", api = api)
  } else {
    url <- gh_url("organizations", api = api)
  }

  organizations <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(organizations, list(
    id          = c("id",          as = "integer"),
    name        = c("login",       as = "character"),
    description = c("description", as = "character"),
    url         = c("url",         as = "character")))
}

#  FUNCTION: is_member ------------------------------------------------------------------------
#
#' Check membership
#'
#' <https://developer.github.com/v3/orgs/members/#check-membership>
#'
#' @param user (string) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE if the user is a member, FALSE otherwise (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
is_member <- function(
  user,
  org,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_string(user))
  stopifnot(is_string(org))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(
      gh_url("orgs", org, "members", user, api = api),
      accept = "raw", token = token, ...)
  }))

  attributes(response) <- NULL
  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_members -----------------------------------------------------------------------
#
#' Get Organization or team members list
#'
#' <https://developer.github.com/v3/orgs/members/#members-list>
#' <https://developer.github.com/v3/orgs/teams/#list-team-members>
#'
#' Note: Must specify either `org` or `team`, but not both.
#'
#' @param org (string) The name of the organization.
#' @param team (integer) The GitHub ID of the team.
#' @param filter (string) Filter members returned in the list. Can be one of:
#'  - `2fa_disabled`: Members without two-factor authentication enabled. Available for
#'    organization owners.
#'  - `all`: All members the authenticated user can see.
#'  - `Default`: all.
#' @param role (string) Filter members returned by their role. Can be one of:
#'  - `all`: All members of the organization, regardless of role.
#'  - `admin`: Organization owners.
#'  - `member`: Non-owner organization members.
#'  - `Default`: all.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the members (see GitHub's API documentation for details).
#'
#' @export
#'
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
  stopifnot(is.null(filter) | is_string(filter))
  stopifnot(is.null(role) | is_string(role))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (!missing(org) && !missing(team))
    stop("Must specify either org or team, not both!")

  if (!missing(org)) {
    stopifnot(is_string(org))
    url <- gh_url("orgs", org, "members", filter = filter, role = role, api = api)
  } else if (!missing(team)) {
    stopifnot(is_count(team))
    url <- gh_url("teams", team, "members", org, role = role, api = api)
  } else {
    stop("Must specify either org or team!")
  }

  members <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(members, list(
    id         = c("id",         as = "integer"),
    login      = c("login",      as = "character"),
    type       = c("type",       as = "character"),
    site_admin = c("site_admin", as = "logical"),
    url        = c("url",        as = "character")))
}

#  FUNCTION: gh_membership --------------------------------------------------------------------
#
#' Get organization and team membership
#'
#' <https://developer.github.com/v3/orgs/members/#get-organization-membership>
#' <https://developer.github.com/v3/orgs/teams/#get-team-membership>
#'
#' Note: Must specify either `org` or `team`, but not both.
#'
#' @param user (string, optional) The GitHub username of the user.
#' @param org (string) The name of the organization.
#' @param team (integer) The GitHub ID of the team.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the membership (see GitHub's API documentation for details).
#'
#' @export
#'
gh_membership <- function(
  user,
  org,
  team,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_string(user))
  stopifnot(is_string(org))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (!missing(org) && !missing(team))
    stop("Must specify either org or team, not both!")

  if (!missing(org)) {
    stopifnot(is_string(org))
    url <- gh_url("orgs", org, "memberships", user, api = api)
  } else if (!missing(team)) {
    stopifnot(is_count(team))
    url <- gh_url("teams", team, "memberships", user, api = api)
  } else {
    stop("Must specify either org or team!")
  }

  gh_get(url, token = token, ...)
}

#  FUNCTION: gh_memberships -------------------------------------------------------------------
#
#' List your organization memberships
#'
#' <https://developer.github.com/v3/orgs/members/#list-your-organization-memberships>
#' <https://developer.github.com/v3/orgs/members/#get-your-organization-membership>
#'
#' @param org (string, optional) The name of the organization.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing your memberships (see GitHub's API documentation for details).
#'
#' @export
#'
gh_memberships <- function(
  org,
  n_max = 1000L,
  token  = gh_token(),
  api    = getOption("github.api"),
  ...)
{
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (missing(org)) {
    url <- gh_url("user/memberships/orgs", api = api)
  } else {
    stopifnot(is_string(org))
    url <- gh_url("user/memberships/orgs", org, api = api)
  }

  gh_page(url, n_max = n_max, token = token, ...)
}

#  FUNCTION: gh_team --------------------------------------------------------------------------
#
#' Get team
#'
#' <https://developer.github.com/v3/orgs/teams/#get-team>
#'
#' @param team (integer) The GitHub ID of the team.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the team (see GitHub's API documentation for details).
#'
#' @export
#'
gh_team <- function(
  team,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(team))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_get(gh_url("teams", team, api = api), token = token, ...)
}

#  FUNCTION: gh_teams -------------------------------------------------------------------------
#
#' List teams
#'
#' <https://developer.github.com/v3/orgs/teams/#list-teams>
#' <https://developer.github.com/v3/orgs/teams/#list-user-teams>
#'
#' @param org (string, optional) The name of the organization. If org and repo are not
#'   specified, the teams of the authenticated user are returned.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the teams (see GitHub's API documentation for details).
#'
#' @export
#'
gh_teams <- function(
  org,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (!missing(org) && !missing(repo))
    stop("Must specify either org or repo, not both!")

  if (missing(org)) {
    if (missing(repo)) {
      url <- gh_url("user/teams", api = api)
    } else {
      stopifnot(is_repo(repo))
      url <- gh_url("repos", repo, "teams", api = api)
    }
  } else {
    stopifnot(is_string(org))
    url <- gh_url("orgs", org, "teams", api = api)
  }

  gh_page(url, n_max = n_max, token = token, ...)
}

#  FUNCTION: is_manager -----------------------------------------------------------------------
#
#' Check if a team manages a repository
#'
#' <https://developer.github.com/v3/orgs/teams/#check-if-a-team-manages-a-repository>
#'
#' @param team (integer) The GitHub ID of the team.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE if the team is a manager, FALSE otherwise (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
is_manager <- function(
  team,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(team))
  stopifnot(is_repo(repo))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(
      gh_url("teams", team, "repos", repo, api = api),
      accept = "raw", token = token, ...)
  }))

  attributes(response) <- NULL
  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}
