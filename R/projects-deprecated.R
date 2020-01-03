#  FUNCTION: gh_project -----------------------------------------------------------------------
#
#' Get a project
#'
#' NOTE: This function has been deprecated, please use [view_project()] instead.
#'
#' <https://developer.github.com/v3/projects/#get-a-project>
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the project (see GitHub's API documentation for details).
#'
#' @export
#'
gh_project <- function(
  project,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_project", package = "githapi")

  assert(is_scalar_integerish(project) && isTRUE(project > 0))
  assert(is_sha(token))
  assert(is_url(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_get(
    gh_url("projects", project, api = api),
    accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_projects ----------------------------------------------------------------------
#
#' List organization or repository projects
#'
#' NOTE: This function has been deprecated, please use [view_projects()] instead.
#'
#' <https://developer.github.com/v3/projects/#list-repository-projects>
#' <https://developer.github.com/v3/projects/#list-organization-projects>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param org (string) The name of the organization.
#' @param state (string, optional) Indicates the state of the projects to return. Can be either open,
#'   closed, or all. Default: open
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the projects (see GitHub's API documentation for details).
#'
#' @export
#'
gh_projects <- function(
  repo,
  org,
  state = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_projects", package = "githapi")

  assert(is_null(state) || is_scalar_character(state))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (!missing(repo) && !missing(org))
    error("Must specify either repo or org, not both!")

  if (!missing(repo)) {
    assert(is_repo(repo))
    url <- gh_url("repos", repo, "projects", state = state, api = api)
  } else if (!missing(org)) {
    assert(is_scalar_character(org))
    url <- gh_url("orgs", org, "projects", state = state, api = api)
  } else {
    error("Must specify either repo or org!")
  }

  # NOTE: Projects is currently in beta, so requires preview accept header
  projects <- gh_page(
    url, accept = "application/vnd.github.inertia-preview+json",
    n_max = n_max, token = token, ...)

  bind_fields(projects, list(
    id            = c("id",               as = "integer"),
    number        = c("number",           as = "integer"),
    name          = c("name",             as = "character"),
    body          = c("body",             as = "character"),
    state         = c("state",            as = "character"),
    creator_login = c("creator", "login", as = "character"),
    created_at    = c("created_at",       as = "datetime"),
    updated_at    = c("updated_at",       as = "datetime"),
    url           = c("url",              as = "character")))
}

#  FUNCTION: gh_column ------------------------------------------------------------------------
#
#' Get a project column
#'
#' <https://developer.github.com/v3/projects/columns/#get-a-project-column>
#'
#' @param column (integer) The ID of the column in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the column (see GitHub's API documentation for details).
#'
#' @export
#'
gh_column <- function(
  column,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_column", package = "githapi")

  assert(is_scalar_integerish(column) && isTRUE(column > 0))
  assert(is_sha(token))
  assert(is_url(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_get(
    gh_url("projects/columns", column, api = api),
    accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_columns -----------------------------------------------------------------------
#
#' List project columns
#'
#' <https://developer.github.com/v3/projects/columns/#list-project-columns>
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the columns (see GitHub's API documentation for details).
#'
#' @export
#'
gh_columns <- function(
  project,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_columns", package = "githapi")

  assert(is_scalar_integerish(project) && isTRUE(project > 0))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  columns <- gh_page(
    gh_url("projects", project, "columns", api = api),
    accept = "application/vnd.github.inertia-preview+json", n_max = n_max, token = token, ...)

  bind_fields(columns, list(
    id         = c("id",         as = "integer"),
    name       = c("name",       as = "character"),
    created_at = c("created_at", as = "datetime"),
    updated_at = c("updated_at", as = "datetime"),
    url        = c("url",        as = "character")))
}

#  FUNCTION: gh_card --------------------------------------------------------------------------
#
#' Get a project card
#'
#' <https://developer.github.com/v3/projects/cards/#get-a-project-card>
#'
#' @param card (integer) The ID of the card in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the card (see GitHub's API documentation for details).
#'
#' @export
#'
gh_card <- function(
  card,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_card", package = "githapi")

  assert(is_scalar_integerish(card) && isTRUE(card > 0))
  assert(is_sha(token))
  assert(is_url(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_get(
    gh_url("projects/columns/cards", card, api = api),
    accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_cards -------------------------------------------------------------------------
#
#' List project cards
#'
#' <https://developer.github.com/v3/projects/cards/#list-project-cards>
#'
#' @param column (integer) The ID of the column in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the cards (see GitHub's API documentation for details).
#'
#' @export
#'
gh_cards <- function(
  column,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_cards", package = "githapi")

  assert(is_scalar_integerish(column) && isTRUE(column > 0))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  cards <- gh_page(
    gh_url("projects/columns", column, "cards", api = api),
    accept = "application/vnd.github.inertia-preview+json",
    n_max = n_max, token = token, ...)

  bind_fields(cards, list(
    id            = c("id",               as = "integer"),
    creator_login = c("creator", "login", as = "character"),
    created_at    = c("created_at",       as = "datetime"),
    updated_at    = c("updated_at",       as = "datetime"),
    content_url   = c("content_url",      as = "character"),
    url           = c("url",              as = "character")))
}
