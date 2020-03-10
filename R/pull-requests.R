#  FUNCTION: create_pull_request --------------------------------------------------------------
#
#' Create an pull request in a repository
#'
#' This function creates a new pull request for the specified repository in GitHub. It can
#' also be used to assign the pull request to a user, request reviewers and add labels or a
#' milestone.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/pulls/#create-a-pull-request>
#' - <https://developer.github.com/v3/issues/#edit-an-issue>
#' - <https://developer.github.com/v3/pulls/review_requests/#create-a-review-request>
#'
#' @param title (string) The title of the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param head (string) The name of the branch where your changes are implemented. For
#'   cross-repository pull requests, prefix `head` with an owner, e.g. `"username:branch"`.
#' @param base (string) The name of the branch you want the changes pulled into. This should
#'   be an existing branch in the specified repository.
#' @param body (string, optional) The contents of the pull request.
#' @param assignees (character, optional) Logins for Users to assign to this pull request.
#'   NOTE: Only users with push access can set assignees for new pull requests.
#' @param reviewers (character, optional) Logins for Users to review this pull request.
#'   NOTE: Only users with push access can set reviewers for new pull requests.
#' @param labels (character, optional) Labels to associate with this pull request. NOTE: Only
#'   users with push access can set labels for new pull requests.
#' @param milestone (character or integer, optional) The title or number of the milestone to
#'   associate this pull request with. NOTE: Only users with push access can set the
#'   milestone for new pull requests.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_pull_request()` returns a list of the pull request's properties.
#'
#' **Pull Request Properties:**
#'
#' - **number**: The number assigned to the pull request.
#' - **title**: The title of the pull request.
#' - **body**: The body contents of the pull request.
#' - **head_sha**: The SHA of the commit to merge in.
#' - **head_ref**: The reference, or branch, to merge in.
#' - **head_repo**: The repository containing the branch to merge in.
#' - **base_sha**: The SHA of the commit to merge onto.
#' - **base_ref**: The reference, or branch, to merge onto.
#' - **merge_sha**: The SHA of the merge commit, if the merge has been completed.
#' - **assignees**: The users assigned to the pull request.
#' - **reviewers**: The users reviewing the pull request.
#' - **labels**: The labels attached to the pull request.
#' - **milestone**: The milestone assigned to the pull request.
#' - **state**: The state of the pull request - either `"open"` or `"closed"`.
#' - **repository**: The repository the pull request is in.
#' - **html_url**: The URL of the pull request's web page in GitHub.
#' - **diff_url**: The URL of the pull request's diff web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the pull request was created.
#' - **updated_at**: When the pull request was last updated.
#' - **mergeable**: Whether the pull request can be merged.
#' - **rebaseable**: Whether the pull request can be rebased.
#' - **merged**: Whether the pull request has been merged
#' - **merged_by**: Who merged the pull request.
#' - **merged_at**: When the pull request was merged.
#' - **closed_at**: When the pull request was closed.
#'
#' @examples
#' \dontrun{
#'   create_pull_request(
#'     title = "test pull request",
#'     repo  = "ChadGoymer/test-githapi",
#'     head  = "test-pulls",
#'     base  = "master",
#'     body  = "This is a pull request to test create_pull_request()")
#'
#'   create_pull_request(
#'     title     = "test assigned pull request",
#'     repo      = "ChadGoymer/test-githapi",
#'     head      = "test-pulls-2",
#'     base      = "master",
#'     body      = "This is a pull request to test create_pull_request()",
#'     assignees = "ChadGoymer",
#'     reviewers = c("BobSmith", "JaneJones"),
#'     labels    = "enhancement",
#'     milestone = "Release-1.0")
#' }
#'
#' @export
#'
create_pull_request <- function(
  title,
  repo,
  head,
  base,
  body,
  assignees,
  reviewers,
  labels,
  milestone,
  ...)
{
  assert(is_scalar_character(title), "'title' must be a string:\n  ", title)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_character(head), "'head' must be a string:\n  ", head)
  assert(is_scalar_character(base), "'base' must be a string:\n  ", base)

  payload <- list(title = title, head = head, base = base)

  if (!missing(body)) {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

  info("Creating pull request '", title, "' for repository '", repo, "'")
  pull_lst <- gh_url("repos", repo, "pulls") %>%
    gh_request("POST", payload = payload, ...)

  payload <- list()

  if (!missing(assignees)) {
    assert(is_character(assignees), "'assignees' must be a character vector:\n  ", assignees)
    payload$assignees <- as.list(assignees)
  }

  if (!missing(labels)) {
    assert(is_character(labels), "'labels' must be a character vector:\n  ", labels)
    payload$labels <- as.list(labels)
  }

  if (!missing(milestone)) {
    if (is_scalar_character(milestone)) {
      milestone <- view_milestone(milestone, repo = repo)$number
    }

    assert(is_scalar_integerish(milestone), "'milestone' must be a string or integer:\n  ", milestone)
    payload$milestone <- milestone
  }

  issue_lst <- list(milestone = list(title = NA_character_))
  if (length(payload) > 0) {
    info("Adding assignees, labels or milestone to pull request '", pull_lst$title, "'")
    issue_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  reviewers_lst <- list()
  if (!missing(reviewers)) {
    assert(is_character(reviewers), "'reviewers' must be a character vector:\n  ", reviewers)
    info("Adding reviewers to pull request '", pull_lst$title, "'")
    reviewers_lst <- gh_url("repos", repo, "pulls", pull_lst$number, "requested_reviewers") %>%
      gh_request("POST", payload = list(reviewers = as.list(reviewers)), ...)
  }

  info("Transforming results", level = 4)
  pull_gh <- select_properties(pull_lst, properties$pull_request) %>%
    modify_list(
      assignees = collapse_property(list(issue_lst), "assignees", "login"),
      reviewers = collapse_property(list(reviewers_lst), "requested_reviewers", "login"),
      labels    = collapse_property(list(issue_lst), "labels", "name"),
      .before   = "milestone") %>%
    modify_list(repository = repo, milestone = issue_lst$milestone$title)

  info("Done", level = 7)
  structure(
    pull_gh,
    class   = class(pull_lst),
    url     = attr(pull_lst, "url"),
    request = attr(pull_lst, "request"),
    status  = attr(pull_lst, "status"),
    header  = attr(pull_lst, "header"))
}


#  FUNCTION: update_pull_request --------------------------------------------------------------
#
#' Update a pull request in a repository
#'
#' This function updates a pull request for the specified repository in GitHub. It can be used
#' to change the title or body, or used to close the pull request. It can also be used to
#' assign the pull request to a user, request reviewers and replace labels or a milestone.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/pulls/#update-a-pull-request>
#' - <https://developer.github.com/v3/issues/#edit-an-issue>
#' - <https://developer.github.com/v3/pulls/review_requests/#create-a-review-request>
#'
#' @param pull_request (string or character) The number or title of the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param title (string, optional) The new title of the pull request.
#' @param body (string, optional) The contents of the pull request.
#' @param assignees (character, optional) Logins for Users to assign to this pull request.
#'   NOTE: Only users with push access can set assignees for new pull requests.
#' @param reviewers (character, optional) Logins for Users to review this pull request.
#'   NOTE: Only users with push access can set reviewers for new pull requests.
#' @param labels (character, optional) Labels to associate with this pull request. NOTE: Only
#'   users with push access can set labels for new pull requests.
#' @param milestone (character or integer, optional) The title or number of the milestone to
#'   associate this pull request with. NOTE: Only users with push access can set the
#'   milestone for new pull requests.
#' @param state (string, optional) The state of the pull request. Either `"open"` or
#'   `"closed"`.
#' @param base (string, optional) The name of the branch you want the changes pulled into.
#'   This should be an existing branch on the current repository.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_pull_request()` returns a list of the pull request's properties.
#'
#' **Pull Request Properties:**
#'
#' - **number**: The number assigned to the pull request.
#' - **title**: The title of the pull request.
#' - **body**: The body contents of the pull request.
#' - **head_sha**: The SHA of the commit to merge in.
#' - **head_ref**: The reference, or branch, to merge in.
#' - **head_repo**: The repository containing the branch to merge in.
#' - **base_sha**: The SHA of the commit to merge onto.
#' - **base_ref**: The reference, or branch, to merge onto.
#' - **merge_sha**: The SHA of the merge commit, if the merge has been completed.
#' - **assignees**: The users assigned to the pull request.
#' - **reviewers**: The users reviewing the pull request.
#' - **labels**: The labels attached to the pull request.
#' - **milestone**: The milestone assigned to the pull request.
#' - **state**: The state of the pull request - either `"open"` or `"closed"`.
#' - **repository**: The repository the pull request is in.
#' - **html_url**: The URL of the pull request's web page in GitHub.
#' - **diff_url**: The URL of the pull request's diff web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the pull request was created.
#' - **updated_at**: When the pull request was last updated.
#' - **mergeable**: Whether the pull request can be merged.
#' - **rebaseable**: Whether the pull request can be rebased.
#' - **merged**: Whether the pull request has been merged
#' - **merged_by**: Who merged the pull request.
#' - **merged_at**: When the pull request was merged.
#' - **closed_at**: When the pull request was closed.
#'
#' @examples
#' \dontrun{
#'   # Update a pull request's properties
#'   update_pull_request(
#'     pull_request = "test pull request",
#'     repo         = "ChadGoymer/test-githapi",
#'     title        = "test updated pull request",
#'     body         = "This is an updated pull request to test create_pull_request()")
#'
#'   # Close a pull request
#'   update_pull_request(
#'     pull_request = "test updated pull request",
#'     repo         = "ChadGoymer/test-githapi",
#'     state        = "closed")
#' }
#'
#' @export
#'
update_pull_request <- function(
  pull_request,
  repo,
  title,
  body,
  assignees,
  reviewers,
  labels,
  milestone,
  base,
  state,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  payload <- list()

  if (!missing(title)) {
    assert(is_scalar_character(title), "'title' must be a string:\n  ", title)
    payload$title <- title
  }

  if (!missing(body)) {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

  if (!missing(state)) {
    assert(
      is_scalar_character(state) && state %in% values$pull_request$state,
      "'state' for milestones must be either '", str_c(values$pull_request$state, collapse = "', '"), "':\n  ", state)
    payload$state <- state
  }

  if (!missing(base)) {
    assert(is_scalar_character(base), "'base' must be a string:\n  ", base)
    payload$base <- base
  }

  if (is_scalar_integerish(pull_request))
  {
    info("Viewing pull request '", pull_request, "' for repository '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", pull_request) %>%
      gh_request("GET", ...)
  }
  else if (is_scalar_character(pull_request))
  {
    info("Viewing pull_request '", pull_request, "' for repository '", repo, "'")
    pull_lst <- gh_url("repos", repo, "pulls", state = "all") %>%
      gh_find(property  = "title", value = pull_request, ...)
  }
  else
  {
    error("'pull_request' must be either an integer or a string:\n  ", pull_request)
  }

  if (length(payload) > 0) {
    info("Updating pull request '", pull_lst$title, "' in repository '", repo, "'")
    pull_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  payload <- list()

  if (!missing(assignees)) {
    assert(is_character(assignees), "'assignees' must be a character vector:\n  ", assignees)
    payload$assignees <- as.list(assignees)
  }

  if (!missing(labels)) {
    assert(is_character(labels), "'labels' must be a character vector:\n  ", labels)
    payload$labels <- as.list(labels)
  }

  if (!missing(milestone)) {
    if (is_scalar_character(milestone)) {
      milestone <- view_milestone(milestone, repo = repo)$number
    }

    assert(is_scalar_integerish(milestone), "'milestone' must be a string or integer:\n  ", milestone)
    payload$milestone <- milestone
  }

  issue_lst <- list(milestone = list(title = NA_character_))
  if (length(payload) > 0) {
    info("Adding assignees, labels or milestone to pull request '", pull_lst$title, "'")
    issue_lst <- gh_url("repos", repo, "issues", pull_lst$number) %>%
      gh_request("PATCH", payload = payload, ...)
  }

  reviewers_lst <- list()
  if (!missing(reviewers)) {
    assert(is_character(reviewers), "'reviewers' must be a character vector:\n  ", reviewers)
    info("Adding reviewers to pull request '", pull_lst$title, "'")
    reviewers_lst <- gh_url("repos", repo, "pulls", pull_lst$number, "requested_reviewers") %>%
      gh_request("POST", payload = list(reviewers = as.list(reviewers)), ...)
  }

  info("Transforming results", level = 4)
  pull_gh <- select_properties(pull_lst, properties$pull_request) %>%
    modify_list(
      assignees = collapse_property(list(issue_lst), "assignees", "login"),
      reviewers = collapse_property(list(reviewers_lst), "requested_reviewers", "login"),
      labels    = collapse_property(list(issue_lst), "labels", "name"),
      .before   = "milestone") %>%
    modify_list(repository = repo, milestone = issue_lst$milestone$title)

  info("Done", level = 7)
  structure(
    pull_gh,
    class   = class(pull_lst),
    url     = attr(pull_lst, "url"),
    request = attr(pull_lst, "request"),
    status  = attr(pull_lst, "status"),
    header  = attr(pull_lst, "header"))
}
