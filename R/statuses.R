#  FUNCTION: create_status --------------------------------------------------------------------
#
#' Create a status for a commit
#'
#' This function creates a new status for a commit in the specified repository in GitHub.
#' The status can also have a `description` and a target URL, allowing a user to link to more
#' information. The `context` allows you to differentiate the status from others. The status
#' can then be updated, from `pending` to `success` say, by using the same `context`.
#'
#' For more details see the GitHub API documentation:
#' - <https://docs.github.com/en/rest/reference/repos#create-a-commit-status>
#'
#' @param state (string) The state of the status. Can be one of `"error"`, `"failure"`,
#'   `"pending"`, or `"success"`.
#' @param ref (string) Either a SHA, branch or tag used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param description (string, optional) A short description of the status.
#' @param target_url (string, optional) The target URL to associate with this status. This URL
#'   will be linked from the GitHub UI to allow users to easily see the source of the status.
#' @param context (string, optional) A string label to differentiate this status from the
#'   status of other systems. Default: `"default"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_status()` returns a list of the status's properties.
#'
#' **Status Properties:**
#'
#' - **id**: The id of the release.
#' - **state**: The state of the status.
#' - **description**: The description of the status.
#' - **target_url**: The URL linked to the status.
#' - **context**: The context of the status.
#' - **creator**: The creator of the status.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a status on the master branch
#'   create_status(
#'     state       = "pending",
#'     ref         = "master",
#'     repo        = "ChadGoymer/test-githapi",
#'     description = "This is a pending status",
#'     target_url  = "https://www.goymer.me.uk/githapi",
#'     context     = "test/githapi")
#'
#'   # Update the status on the master branch
#'   create_status(
#'     state       = "success",
#'     ref         = "master",
#'     repo        = "ChadGoymer/test-githapi",
#'     description = "This is a success status",
#'     target_url  = "https://www.goymer.me.uk/githapi",
#'     context     = "test/githapi")
#'
#' }
#'
#' @export
#'
create_status <- function(
  state,
  ref,
  repo,
  description,
  target_url,
  context = "default",
  ...)
{
  assert(
    is_scalar_character(state) && state %in% values$status$state,
    "'state' must be one of '", str_c(values$status$state, collapse = "', '"), "':\n  ", state)
  assert(is_ref(ref), "'ref' must be a valid git reference - see help(is_ref):\n  ", ref)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_character(context), "'context' must be a string:\n  ", context)

  payload <- list(state = state, context = context)

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  if (!missing(target_url)) {
    assert(is_scalar_character(target_url), "'target_url' must be a string:\n  ", target_url)
    payload$target_url <- target_url
  }

  if (!is_sha(ref)) {
    ref <- view_sha(ref = ref, repo = repo, ...)
  }

  info("Creating status '", state, "' for ref '", ref, "' in repository '", repo, "'")
  status_lst <- gh_url("repos", repo, "statuses", ref) %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  status_gh <- select_properties(status_lst, properties$status)

  info("Done", level = 7)
  status_gh

}
