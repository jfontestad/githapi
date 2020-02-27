#  FUNCTION: create_issue ------------------------------------------------------------------
#
#' Create an issue in a repository
#'
#' This function creates a new issue for the specified repository in GitHub. It can also be
#' used to assign the issue to a user and add labels or a milestone.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/issues/#create-an-issue>
#'
#' @param title (string) The title of the issue.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param body (string, optional) The contents of the issue.
#' @param assignees (character, optional) Logins for Users to assign to this issue. NOTE: Only
#'   users with push access can set assignees for new issues.
#' @param labels (character, optional) Labels to associate with this issue. NOTE: Only users
#'   with push access can set labels for new issues.
#' @param milestone (character or integer, optional) The title or number of the milestone to
#'   associate this issue with. NOTE: Only users with push access can set the milestone for
#'   new issues.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_issue()` returns a list of the issue's properties.
#'
#' **Issue Properties:**
#'
#' - **number**: The number assigned to the issue.
#' - **title**: The title of the issue.
#' - **body**: The body contents of the issue.
#' - **assignees**: The user assigned to the issue.
#' - **labels**: The labels attached to the issue.
#' - **milestone**: The milestone assigned to the issue.
#' - **state**: The state of the issue - either `"open"` or `"closed"`.
#' - **repository**: The repository the issue is in.
#' - **pull_request**: Whether the issue is a pull request.
#' - **html_url**: The URL of the issue's web page in GitHub.
#' - **creator**: The creator's login.
#' - **created_at**: When the issue was created.
#' - **updated_at**: When the issue was last updated.
#' - **closed_at**: When the issue was closed.
#'
#' @examples
#' \dontrun{
#'   create_issue(
#'     title     = "user issue",
#'     repo      = "ChadGoymer/test-issues",
#'     body      = "This is an issue to test create_issue()",
#'     assignees = "ChadGoymer",
#'     labels    = "feature",
#'     milestone = "release-1.0")
#'
#'   create_issue(
#'     title     = "organization issue",
#'     repo      = "HairyCoos/test-issues",
#'     body      = "This is an issue to test create_issue()",
#'     assignees = "ChadGoymer")
#' }
#'
#' @export
#'
create_issue <- function(
  title,
  repo,
  body,
  assignees,
  labels,
  milestone,
  ...)
{
  assert(is_scalar_character(title), "'title' must be a string:\n  ", title)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  payload <- list(title = title)

  if (!missing(body)) {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

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

  info("Creating issue '", title, "' for repository '", repo, "'")
  issue_lst <- gh_url("repos", repo, "issues") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  issue_gh <- select_properties(issue_lst, properties$issue) %>%
    modify_list(
      assignees = collapse_property(list(issue_lst), "assignees", "login"),
      labels = collapse_property(list(issue_lst), "labels", "name"),
      .before = "milestone") %>%
    modify_list(pull_request = !is_null(issue_lst$pull_request), .before = "html_url") %>%
    modify_list(repository = repo)

  info("Done", level = 7)
  structure(
    issue_gh,
    class   = class(issue_lst),
    url     = attr(issue_lst, "url"),
    request = attr(issue_lst, "request"),
    status  = attr(issue_lst, "status"),
    header  = attr(issue_lst, "header"))
}
