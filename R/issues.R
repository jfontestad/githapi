#  FUNCTION: gh_user_issues -------------------------------------------------------------------
#' List issues assigned to the authenticated user
#'
#' @param org (string, optional) Filter the list to repositories owned by an organisation.
#' @param filter (string) Indicates which sorts of issues to return. Can be one of:
#'   \itemize{
#'     \item assigned: Issues assigned to you
#'     \item created: Issues created by you
#'     \item mentioned: Issues mentioning you
#'     \item subscribed: Issues you're subscribed to updates for
#'     \item all: All issues the authenticated user can see, regardless of participation or creation
#'     \item Default: assigned.
#'   }
#' @param state (string) Indicates the state of the issues to return. Can be either open, closed,
#'   or all. Default: open.
#' @param labels (string) A list of comma separated label names. Example: bug,ui,@high.
#' @param sort (string) What to sort results by. Can be either created, updated, comments.
#'   Default: created.
#' @param direction (string) The direction of the sort. Can be either asc or desc. Default: desc.
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @export
gh_user_issues <- function(
  org,
  filter    = NULL,
  state     = NULL,
  labels    = NULL,
  sort      = NULL,
  direction = NULL,
  since     = NULL,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  if (missing(org)) {
    path <- "issues"
  } else {
    path <- file.path("orgs", org, "issues")
  }

  issues <- gh_url(
    path,
    filter    = filter,
    state     = state,
    labels    = labels,
    sort      = sort,
    direction = direction,
    since     = since) %>%
    gh_page(token = token, ...)

  labels <- map_chr(issues, function(i) {
    if (length(i$labels) > 0L) {
      map_chr(i$labels, "name") %>% str_c(collapse = ",")
    } else {
      ""
    }
  })

  issues %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(labels = labels) %>%
    select(number, state, title, body, user_login, labels, assignee_login,
           milestone_number, milestone_title, created_at, updated_at, repository_name) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_issues ------------------------------------------------------------------------
#' List issues for a repository
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param milestone	(string)	If an integer is passed, it should refer to a milestone by its number
#'   field. If the string * is passed, issues with any milestone are accepted. If the string none
#'   is passed, issues without milestones are returned.
#' @param state	(string) Indicates the state of the issues to return. Can be either open, closed,
#'   or all. Default: open
#' @param assignee	(string) Can be the name of a user. Pass in none for issues with no assigned
#'   user, and * for issues assigned to any user.
#' @param creator	(string) The user that created the issue.
#' @param mentioned	(string) A user that's mentioned in the issue.
#' @param labels	(string) A list of comma separated label names. Example: bug,ui,@high
#' @param sort	(string) What to sort results by. Can be either created, updated, comments. Default: created
#' @param direction	(string) The direction of the sort. Can be either asc or desc. Default: desc
#' @param since	(string) Only issues updated at or after this time are returned. This is a timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return
#' @export
gh_issues <- function(
  repo,
  milestone = NULL,
  state     = NULL,
  assignee  = NULL,
  creator   = NULL,
  mentioned = NULL,
  labels    = NULL,
  sort      = NULL,
  direction = NULL,
  since     = NULL,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  issues <- gh_url(
    "repos", repo, "issues", api = api,
    milestone = milestone,
    state     = state,
    assignee  = assignee,
    creator   = creator,
    mentioned = mentioned,
    labels    = labels,
    sort      = sort,
    direction = direction,
    since     = since) %>%
    gh_page(token = token, ...)

  labels <- map_chr(issues, function(i) {
    if (length(i$labels) > 0L) {
      map_chr(i$labels, "name") %>% str_c(collapse = ",")
    } else {
      ""
    }
  })

  issues %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(labels = labels) %>%
    select(number, state, title, body, user_login, labels, assignee_login,
           milestone_number, milestone_title, created_at, updated_at) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}
