#  FUNCTION: gh_project -----------------------------------------------------------------------
#' Get a project
#'
#' url{https://developer.github.com/v3/projects/#get-a-project}
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the project (see GitHub's API documentation for details).
#' @export
gh_project <- function(
  project,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(project))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects", project, api = api) %>%
    gh_get(accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_projects ----------------------------------------------------------------------
#' List organisation or repository projects
#'
#' url{https://developer.github.com/v3/projects/#list-repository-projects}
#' url{https://developer.github.com/v3/projects/#list-organization-projects}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param org (string) The name of the organization.
#' @param state (string, optional) Indicates the state of the projects to return. Can be either open,
#'   closed, or all. Default: open
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the projects (see GitHub's API documentation for details).
#' @export
gh_projects <- function(
  repo,
  org,
  state = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.null(state) || is.string(state))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(repo) && !missing(org))
    stop("Must specify either repo or org, not both!")

  if (!missing(repo)) {
    assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
    url <- gh_url("repos", repo, "projects", state = state, api = api)
  } else if (!missing(org)) {
    assert_that(is.string(org))
    url <- gh_url("orgs", org, "projects", state = state, api = api)
  } else {
    stop("Must specify either repo or org!")
  }

  # NOTE: Projects is currently in beta, so requires preview accept header
  url %>%
    gh_page(
      simplify = TRUE, accept = "application/vnd.github.inertia-preview+json",
      n_max = n_max, token = token, ...) %>%
    select_safe(id, number, name, body, state, creator_login, created_at, updated_at, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_column ------------------------------------------------------------------------
#' Get a project column
#'
#' url{https://developer.github.com/v3/projects/columns/#get-a-project-column}
#'
#' @param column (integer) The ID of the column in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the column (see GitHub's API documentation for details).
#' @export
gh_column <- function(
  column,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(column))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects/columns", column, api = api) %>%
    gh_get(accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_columns -----------------------------------------------------------------------
#' List project columns
#'
#' url{https://developer.github.com/v3/projects/columns/#list-project-columns}
#'
#' @param project (integer) The ID of the project in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the columns (see GitHub's API documentation for details).
#' @export
gh_columns <- function(
  project,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(project))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects", project, "columns", api = api) %>%
    gh_page(
      simplify = TRUE, accept = "application/vnd.github.inertia-preview+json",
      n_max = n_max, token = token, ...) %>%
    select_safe(id, name, created_at, updated_at, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_card --------------------------------------------------------------------------
#' Get a project card
#'
#' url{https://developer.github.com/v3/projects/cards/#get-a-project-card}
#'
#' @param card (integer) The ID of the card in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the card (see GitHub's API documentation for details).
#' @export
gh_card <- function(
  card,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(card))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects/columns/cards", card, api = api) %>%
    gh_get(accept = "application/vnd.github.inertia-preview+json", token = token, ...)
}

#  FUNCTION: gh_cards -------------------------------------------------------------------------
#' List project cards
#'
#' url{https://developer.github.com/v3/projects/cards/#list-project-cards}
#'
#' @param column (integer) The ID of the column in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the cards (see GitHub's API documentation for details).
#' @export
gh_cards <- function(
  column,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(column))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # NOTE: Projects is currently in beta, so requires preview accept header
  gh_url("projects/columns", column, "cards", api = api) %>%
    gh_page(
      simplify = TRUE, accept = "application/vnd.github.inertia-preview+json",
      n_max = n_max, token = token, ...) %>%
    select_safe(id, creator_login, created_at, updated_at, content_url, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}
