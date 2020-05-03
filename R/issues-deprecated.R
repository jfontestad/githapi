#  FUNCTION: gh_issue -------------------------------------------------------------------------
#
#' Get a single issue
#'
#' <https://developer.github.com/v3/issues/#get-a-single-issue>
#'
#' @param issue (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the issue (see GitHub's API documentation for details).
#'
#' @export
#'
gh_issue <- function(
  issue,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_issue", package = "githapi")

  assert(is_scalar_integerish(issue) && isTRUE(issue > 0))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(gh_url("repos", repo, "issues", issue, api = api), token = token, ...)
}

#  FUNCTION: gh_issues ------------------------------------------------------------------------
#
#' List issues for a repository
#'
#' <https://developer.github.com/v3/issues/#list-issues-for-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param milestone	(string)	If an integer is passed, it should refer to a milestone by its number
#'   field. If the string * is passed, issues with any milestone are accepted. If the string none
#'   is passed, issues without milestones are returned.
#' @param state	(string) Indicates the state of the issues to return. Can be either open, closed,
#'   or all. Default: open
#' @param assignee	(string) Can be the name of a user. Pass in none for issues with no assigned
#'   user, and * for issues assigned to any user.
#' @param creator	(string) The user that created the issue.
#' @param mentioned	(string) A user that's mentioned in the issue.
#' @param labels	(string) A list of comma separated label names. Example: bug,ui, high
#' @param sort	(string) What to sort results by. Can be either created, updated, comments.
#'   Default: created
#' @param direction	(string) The direction of the sort. Can be either asc or desc. Default: desc
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the issues a repository has (see GitHub's API documentation for
#'   more detail)
#'
#' @export
#'
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
  .Deprecated("view_issues", package = "githapi")

  assert(is_repo(repo))
  assert(is_null(milestone) || is_scalar_character(milestone))
  assert(is_null(state) || is_scalar_character(state))
  assert(is_null(assignee) || is_scalar_character(assignee))
  assert(is_null(creator) || is_scalar_character(creator))
  assert(is_null(mentioned) || is_scalar_character(mentioned))
  assert(is_null(labels) || is_scalar_character(labels))
  assert(is_null(sort) || is_scalar_character(sort))
  assert(is_null(direction) || is_scalar_character(direction))
  assert(is_null(since) || is_scalar_character(since))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

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
    gh_page(n_max = n_max, token = token, ...)

  bind_fields(issues, list(
    number           = c("number",              as = "integer"),
    title            = c("title",               as = "character"),
    body             = c("body",                as = "character"),
    state            = c("state",               as = "character"),
    user_login       = c("user", "login",       as = "character"),
    labels           = "",
    assignees        = "",
    milestone_number = c("milestone", "number", as = "integer"),
    milestone_title  = c("milestone", "title",  as = "character"),
    created_at       = c("created_at",          as = "datetime"),
    updated_at       = c("updated_at",          as = "datetime"),
    closed_at        = c("closed_at",           as = "datetime"))) %>%
    mutate(
      labels    = lapply(issues, function(i) sapply(i[["labels"]], getElement, "name")),
      assignees = lapply(issues, function(i) sapply(i[["assignees"]], getElement, "login")))
}

#  FUNCTION: gh_user_issues -------------------------------------------------------------------
#
#' List issues assigned to the authenticated user
#'
#' <https://developer.github.com/v3/issues/#list-issues>
#'
#' @param org (string, optional) Filter the list to repositories owned by an organization.
#' @param filter (string) Indicates which sorts of issues to return. Can be one of:
#'  - `assigned`: Issues assigned to you
#'  - `created`: Issues created by you
#'  - `mentioned`: Issues mentioning you
#'  - `subscribed`: Issues you're subscribed to updates for
#'  - `all`: All issues the authenticated user can see, regardless of participation or creation
#'  - `Default`: assigned.
#' @param state (string) Indicates the state of the issues to return. Can be either open, closed,
#'   or all. Default: open.
#' @param labels (string) A list of comma separated label names. Example: bug,ui, high.
#' @param sort (string) What to sort results by. Can be either created, updated, comments.
#'   Default: created.
#' @param direction (string) The direction of the sort. Can be either asc or desc. Default: desc.
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the issues a user has assigned to them (see GitHub's API
#'   documentation for more detail)
#'
#' @export
#'
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
  .Deprecated("view_issues", package = "githapi")

  assert(is_null(filter) || is_scalar_character(filter))
  assert(is_null(state) || is_scalar_character(state))
  assert(is_null(labels) || is_scalar_character(labels))
  assert(is_null(sort) || is_scalar_character(sort))
  assert(is_null(direction) || is_scalar_character(direction))
  assert(is_null(since) || is_scalar_character(since))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(org)) {
    path <- "issues"
  } else {
    assert(is_scalar_character(org))
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
    gh_page(n_max = n_max, token = token, ...)

  bind_fields(issues, list(
    number           = c("number",              as = "integer"),
    title            = c("title",               as = "character"),
    body             = c("body",                as = "character"),
    state            = c("state",               as = "character"),
    repository_name  = c("repository", "name",  as = "character"),
    labels           = "",
    user_login       = c("user", "login",       as = "character"),
    assignee_login   = c("assignee", "login",   as = "character"),
    milestone_number = c("milestone", "number", as = "integer"),
    milestone_title  = c("milestone", "title",  as = "character"),
    created_at       = c("created_at",          as = "datetime"),
    updated_at       = c("updated_at",          as = "datetime"))) %>%
    mutate(labels = lapply(issues, function(i) sapply(i[["labels"]], getElement, "name")))
}

#  FUNCTION: gh_assignees ---------------------------------------------------------------------
#
#' List assignees for issues in a repository
#'
#' <https://developer.github.com/v3/issues/assignees/#list-assignees>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the assignees (see GitHub's API documentation for
#'   more detail)
#'
#' @export
#'
gh_assignees <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(msg = "This function will be removed in a future version")

  assert(is_repo(repo))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  assignees <- gh_page(
    gh_url("repos", repo, "assignees", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(assignees, list(
    login      = c("login",      as = "character"),
    type       = c("type",       as = "character"),
    site_admin = c("site_admin", as = "logical")))
}

#  FUNCTION: gh_issue_comments ----------------------------------------------------------------
#
#' List comments on an issue
#'
#' <https://developer.github.com/v3/issues/comments/#list-comments-on-an-issue>
#'
#' @param issue (integer) The number assigned to the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param since (string) Only issues updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the issue comments (see GitHub's API documentation for details).
#'
#' @export
#'
gh_issue_comments <- function(
  issue,
  repo,
  since = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_comments", package = "githapi")

  assert(is_repo(repo))
  assert(is_null(since) || is_scalar_character(since))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(issue)) {
    comments <- gh_page(
      gh_url("repos", repo, "issues/comments", since = since, api = api),
      n_max = n_max, token = token, ...)
  } else {
    assert(is_scalar_integerish(issue) && isTRUE(issue > 0))
    comments <- gh_page(
      gh_url("repos", repo, "issues", issue, "comments", since = since, api = api),
      n_max = n_max, token = token, ...)
  }

  bind_fields(comments, list(
    id         = c("id",            as = "integer"),
    body       = c("body",          as = "character"),
    user_login = c("user", "login", as = "character"),
    created_at = c("created_at",    as = "datetime"),
    updated_at = c("updated_at",    as = "datetime"),
    html_url   = c("html_url",      as = "character")))
}

#  FUNCTION: gh_issue_comment -----------------------------------------------------------------
#
#' Get a single issue comment
#'
#' <https://developer.github.com/v3/issues/comments/#get-a-single-comment>
#'
#' @param comment (integer) The number assigned to the comment.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the issue comment (see GitHub's API documentation for details).
#'
#' @export
#'
gh_issue_comment <- function(
  comment,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_comment", package = "githapi")

  assert(is_scalar_integerish(comment) && isTRUE(comment > 0))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(gh_url("repos", repo, "issues/comments", comment, api = api), token = token, ...)
}

#  FUNCTION: gh_label -------------------------------------------------------------------------
#
#' Get a single label
#'
#' <https://developer.github.com/v3/issues/labels/#get-a-single-label>
#'
#' @param name (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the label (see GitHub's API documentation for details).
#'
#' @export
#'
gh_label <- function(
  name,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_label", package = "githapi")

  assert(is_scalar_character(name))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(gh_url("repos", repo, "labels", name, api = api), token = token, ...)
}

#  FUNCTION: gh_labels ------------------------------------------------------------------------
#
#' List all labels for an issue, milestone or repository
#'
#' <https://developer.github.com/v3/issues/labels/#list-labels-on-an-issue>
#' <https://developer.github.com/v3/issues/labels/#get-labels-for-every-issue-in-a-milestone>
#' <https://developer.github.com/v3/issues/labels/#list-all-labels-for-this-repository>
#'
#' Note: Must specify either `issue` or `milestone`, but not both.
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param issue (integer) The number assigned to the issue.
#' @param milestone (integer) The number assigned to the milestone.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the labels (see GitHub's API documentation for details).
#'
#' @export
#'
gh_labels <- function(
  repo,
  issue,
  milestone,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_labels", package = "githapi")

  assert(is_repo(repo))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (!missing(issue) && !missing(milestone))
    error("Must specify either issue or milestone, not both!")

  if (!missing(issue)) {
    assert(is_scalar_integerish(issue) && isTRUE(issue > 0))
    url <- gh_url("repos", repo, "issues", issue, "labels", api = api)
  } else if (!missing(milestone)) {
    assert(is_scalar_integerish(milestone) && isTRUE(milestone > 0))
    url <- gh_url("repos", repo, "milestones", milestone, "labels", api = api)
  } else {
    url <- gh_url("repos", repo, "labels", api = api)
  }

  labels <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(labels, list(
    id      = c("id",      as = "integer"),
    name    = c("name",    as = "character"),
    color   = c("color",   as = "character"),
    default = c("default", as = "logical"),
    url     = c("url",     as = "character")))
}

#  FUNCTION: gh_milestone ---------------------------------------------------------------------
#
#' Get a single milestone
#'
#' <https://developer.github.com/v3/issues/milestones/#get-a-single-milestone>
#'
#' @param milestone (integer) The number assigned to the milestone.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the milestone (see GitHub's API documentation for details).
#'
#' @export
#'
gh_milestone <- function(
  milestone,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_milestone", package = "githapi")

  assert(is_scalar_integerish(milestone) && isTRUE(milestone > 0))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(gh_url("repos", repo, "milestones", milestone, api = api), token = token, ...)
}

#  FUNCTION: gh_milestones --------------------------------------------------------------------
#
#' List milestones for a repository
#'
#' <https://developer.github.com/v3/issues/milestones/#list-milestones-for-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param state (string) The state of the milestone. Either open, closed, or all. Default: open
#' @param sort (string) What to sort results by. Either due_on or completeness. Default: due_on
#' @param direction (string) The direction of the sort. Either asc or desc. Default: asc
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the milestones (see GitHub's API documentation for details).
#'
#' @export
#'
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
  .Deprecated("view_milestones", package = "githapi")

  assert(is_repo(repo))
  assert(is_null(state) || is_scalar_character(state))
  assert(is_null(sort) || is_scalar_character(sort))
  assert(is_null(direction) || is_scalar_character(direction))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  milestones <- gh_url(
    "repos", repo, "milestones", api = api,
    state = state, sort = sort, direction = direction) %>%
    gh_page(n_max = n_max, token = token, ...)

  bind_fields(milestones, list(
    id            = c("id",               as = "integer"),
    number        = c("number",           as = "integer"),
    title         = c("title",            as = "character"),
    description   = c("description",      as = "character"),
    creator_login = c("creator", "login", as = "character"),
    open_issues   = c("open_issues",      as = "integer"),
    closed_issues = c("closed_issues",    as = "integer"),
    state         = c("state",            as = "character"),
    created_at    = c("created_at",       as = "datetime"),
    updated_at    = c("updated_at",       as = "datetime"),
    url           = c("url",              as = "character")))
}

#  FUNCTION: gh_event -------------------------------------------------------------------------
#
#' Get a single event
#'
#' <https://developer.github.com/v3/issues/events/#get-a-single-event>
#'
#' @param event (integer) The ID assigned to the event in GitHub.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the event (see GitHub's API documentation for details).
#'
#' @export
#'
gh_event <- function(
  event,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(msg = "This function will be removed in a future version")

  assert(is_scalar_integerish(event) && isTRUE(event > 0))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(gh_url("repos", repo, "issues/events", event, api = api), token = token, ...)
}

#  FUNCTION: gh_events ------------------------------------------------------------------------
#
#' List events for an issue or all issues in a repository
#'
#' <https://developer.github.com/v3/issues/events/#list-events-for-a-repository>
#' <https://developer.github.com/v3/issues/events/#list-events-for-an-issue>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param issue (integer, optional) The number assigned to the issue.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the issue events (see GitHub's API documentation for details).
#'
#' @export
#'
gh_events <- function(
  repo,
  issue,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(msg = "This function will be removed in a future version")

  assert(is_repo(repo))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(issue)) {
    url <- gh_url("repos", repo, "issues/events", api = api)
  } else {
    assert(is_scalar_integerish(issue) && isTRUE(issue > 0))
    url <- gh_url("repos", repo, "issues", issue, "events", api = api)
  }

  events <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(events, list(
    id           = c("id",              as = "numeric"),
    event        = c("event",           as = "character"),
    issue_number = c("issue", "number", as = "integer"),
    issue_title  = c("issue", "title",  as = "character"),
    created_at   = c("created_at",      as = "datetime"),
    actor_login  = c("actor", "login",  as = "character"),
    commit_id    = c("commit", "id",    as = "character"),
    url          = c("url",             as = "character")))
}
