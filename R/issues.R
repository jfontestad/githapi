#  FUNCTION: gh_issue -------------------------------------------------------------------------
#' Get a single issue
#'
#' \url{https://developer.github.com/v3/issues/#get-a-single-issue}
#'
#' @param issue_number (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the issue (see GitHub's API documentation for details).
#' @export
gh_issue <- function(
  issue_number,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(issue_number))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "issues", issue_number, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_issues ------------------------------------------------------------------------
#' List issues for a repository
#'
#' \url{https://developer.github.com/v3/issues/#list-issues-for-a-repository}
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
#' @param sort	(string) What to sort results by. Can be either created, updated, comments.
#'   Default: created
#' @param direction	(string) The direction of the sort. Can be either asc or desc. Default: desc
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing all the issues a repository has (see GitHub's API documentation for
#'   more detail)
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
  assert_that(is.string(repo))
  assert_that(is.null(milestone) || is.string(milestone))
  assert_that(is.null(state) || is.string(state))
  assert_that(is.null(assignee) || is.string(assignee))
  assert_that(is.null(creator) || is.string(creator))
  assert_that(is.null(mentioned) || is.string(mentioned))
  assert_that(is.null(labels) || is.string(labels))
  assert_that(is.null(sort) || is.string(sort))
  assert_that(is.null(direction) || is.string(direction))
  assert_that(is.null(since) || is.string(since))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

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
    map(function(i) {
      if (is.null(i$assignee$login)) i$assignee$login <- ""
      if (is.null(i$milestone$number)) i$milestone$number <- ""
      if (is.null(i$milestone$title)) i$milestone$title <- ""
      i
    }) %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(labels = labels) %>%
    select(
      number, title, body, state, user_login, labels, assignee_login,
      milestone_number, milestone_title, created_at, updated_at) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_user_issues -------------------------------------------------------------------
#' List issues assigned to the authenticated user
#'
#' \url{https://developer.github.com/v3/issues/#list-issues}
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
#' @return A tibble describing all the issues a user has assigned to them (see GitHub's API
#'   documentation for more detail)
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
  assert_that(is.null(filter) || is.string(filter))
  assert_that(is.null(state) || is.string(state))
  assert_that(is.null(labels) || is.string(labels))
  assert_that(is.null(sort) || is.string(sort))
  assert_that(is.null(direction) || is.string(direction))
  assert_that(is.null(since) || is.string(since))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(org)) {
    path <- "issues"
  } else {
    assert_that(is.string(org))
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
    select(number, title, body, state, user_login, labels, assignee_login,
           milestone_number, milestone_title, created_at, updated_at, repository_name) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_assignees ---------------------------------------------------------------------
#' List assignees for issues in a repository
#'
#' \url{https://developer.github.com/v3/issues/assignees/#list-assignees}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing all the issues a repository has (see GitHub's API documentation for
#'   more detail)
#' @export
gh_assignees <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "assignees", api = api) %>%
    gh_page(token = token, ...) %>%
    bind_rows() %>%
    select(login, type, site_admin)
}

#  FUNCTION: gh_issue_comments ----------------------------------------------------------------
#' List comments on an issue
#'
#' \url{https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue}
#'
#' @param issue_number (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the issue (see GitHub's API documentation for details).
#' @export
gh_issue_comments <- function(
  issue_number,
  repo,
  since = NULL,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.null(since) || is.string(since))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(issue_number)) {
    comments <- gh_url("repos", repo, "issues/comments", since = since, api = api) %>%
      gh_page(token = token, ...)
  } else {
    assert_that(is.count(issue_number))
    comments <- gh_url("repos", repo, "issues", issue_number, "comments", since = since, api = api) %>%
      gh_page(token = token, ...)
  }

  if (!identical(comments, list())) {
    comments %>%
      map(flatten_) %>%
      bind_rows() %>%
      select(id, body, user_login, created_at, updated_at, html_url) %>%
      mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
  } else {
    tibble(
      body = character(),
      user_login = character(),
      created_at = parse_datetime(character()),
      updated_at = parse_datetime(character()),
      html_url = character())
  }
}

#  FUNCTION: gh_issue_comment -----------------------------------------------------------------
#' Get a single issue comment
#'
#' \url{https://developer.github.com/v3/issues/comments/#get-a-single-comment}
#'
#' @param comment_id (integer) The number assigned to the comment.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the issue comment (see GitHub's API documentation for details).
#' @export
gh_issue_comment <- function(
  comment_id,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(comment_id))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "issues/comments", comment_id, api = api) %>%
    gh_page(token = token, ...)
}

