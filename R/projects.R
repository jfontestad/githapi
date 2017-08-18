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
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.null(state) || is.string(state))
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
    gh_tibble(token = token, accept = "application/vnd.github.inertia-preview+json", ...) %>%
    select(id, number, name, body, state, creator_login, created_at, updated_at, url) %>%
    mutate(
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at))
}

