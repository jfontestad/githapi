#  FUNCTION: gh_issue -------------------------------------------------------------------------
#' Get a single issue
#'
#' \url{https://developer.github.com/v3/issues/#get-a-single-issue}
#'
#' @param issue (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the issue (see GitHub's API documentation for details).
#' @export
gh_issue <- function(
  issue,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(issue))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "issues", issue, api = api) %>%
    gh_get(token = token, ...)
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
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
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
  n_max     = 1000L,
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
  assert_that(is.count(n_max))
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
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      labels = map_chr(labels, ~str_c(.$name, collapse = ",")),
      assignees = map_chr(assignees, ~str_c(.$login, collapse = ",")),
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at),
      closed_at  = parse_datetime(closed_at)) %>%
    select(
      number, title, body, state, user_login, labels, assignees,
      milestone_number, milestone_title, created_at, updated_at, closed_at)
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
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
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
  n_max     = 1000L,
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
  assert_that(is.count(n_max))
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
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      labels = map_chr(labels, ~str_c(.$name, collapse = ",")),
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at)) %>%
    select(
      number, title, body, state, user_login, labels, assignee_login,
      milestone_number, milestone_title, created_at, updated_at, repository_name)
}

#  FUNCTION: gh_assignees ---------------------------------------------------------------------
#' List assignees for issues in a repository
#'
#' \url{https://developer.github.com/v3/issues/assignees/#list-assignees}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the assignees (see GitHub's API documentation for
#'   more detail)
#' @export
gh_assignees <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "assignees", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select(login, type, site_admin)
}

#  FUNCTION: gh_issue_comments ----------------------------------------------------------------
#' List comments on an issue
#'
#' \url{https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue}
#'
#' @param issue (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the issue comments (see GitHub's API documentation for details).
#' @export
gh_issue_comments <- function(
  issue,
  repo,
  since = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.null(since) || is.string(since))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(issue)) {
    comments <- gh_url("repos", repo, "issues/comments", since = since, api = api) %>%
      gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
  } else {
    assert_that(is.count(issue))
    comments <- gh_url("repos", repo, "issues", issue, "comments", since = since, api = api) %>%
      gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
  }

  if (identical(nrow(comments), 0L)) {
    tibble(
      body       = character(),
      user_login = character(),
      created_at = parse_datetime(character()),
      updated_at = parse_datetime(character()),
      html_url   = character())
  } else {
    comments %>%
      mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at)) %>%
      select(id, body, user_login, created_at, updated_at, html_url)
  }
}

#  FUNCTION: gh_issue_comment -----------------------------------------------------------------
#' Get a single issue comment
#'
#' \url{https://developer.github.com/v3/issues/comments/#get-a-single-comment}
#'
#' @param comment (integer) The number assigned to the comment.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the issue comment (see GitHub's API documentation for details).
#' @export
gh_issue_comment <- function(
  comment,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(comment))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "issues/comments", comment, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_label -------------------------------------------------------------------------
#' Get a single label
#'
#' url{https://developer.github.com/v3/issues/labels/#get-a-single-label}
#'
#' @param name (string) The name of the label.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the label (see GitHub's API documentation for details).
#' @export
gh_label <- function(
  name,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(name))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "labels", name, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_labels ------------------------------------------------------------------------
#' List all labels for an issue, milestone or repository
#'
#' url{https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue}
#' url{https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone}
#' url{https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository}
#'
#' Note: Must specify either \code{issue} or \code{milestone}, but not both.
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param issue (integer) The number assigned to the issue.
#' @param milestone (integer) The number assigned to the milestone.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the labels (see GitHub's API documentation for details).
#' @export
gh_labels <- function(
  repo,
  issue,
  milestone,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(issue) && !missing(milestone))
    stop("Must specify either issue or milestone, not both!")

  if (!missing(issue)) {
    assert_that(is.count(issue))
    url <- gh_url("repos", repo, "issues", issue, "labels", api = api)
  } else if (!missing(milestone)) {
    assert_that(is.count(milestone))
    url <- gh_url("repos", repo, "milestones", milestone, "labels", api = api)
  } else {
    url <- gh_url("repos", repo, "labels", api = api)
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select(id, name, color, default, url)
}

#  FUNCTION: gh_milestone ---------------------------------------------------------------------
#' Get a single milestone
#'
#' url{https://developer.github.com/v3/issues/milestones/#get-a-single-milestone}
#'
#' @param milestone (integer) The number assigned to the milestone.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the milestone (see GitHub's API documentation for details).
#' @export
gh_milestone <- function(
  milestone,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(milestone))
  assert_that(is.string(repo))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "milestones", milestone, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_milestones --------------------------------------------------------------------
#' List milestones for a repository
#'
#' url{https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param state (string) The state of the milestone. Either open, closed, or all. Default: open
#' @param sort (string) What to sort results by. Either due_on or completeness. Default: due_on
#' @param direction (string) The direction of the sort. Either asc or desc. Default: asc
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the milestones (see GitHub's API documentation for details).
#' @export
gh_milestones <- function(
  repo,
  state     = NULL,
  sort      = NULL,
  direction = NULL,
  n_max     = 1000L,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo))
  assert_that(is.null(state) || is.string(state))
  assert_that(is.null(sort) || is.string(sort))
  assert_that(is.null(direction) || is.string(direction))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url(
    "repos", repo, "milestones", api = api,
    state = state, sort = sort, direction = direction) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      number     = as.integer(number),
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at)) %>%
    select(
      id, number, title, description, creator_login, open_issues,
      closed_issues, state, created_at, updated_at, url)
}
