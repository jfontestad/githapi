#  FUNCTION: create_branch --------------------------------------------------------------------
#
#' Create a branch in a repository
#'
#' This function creates a new branch in the specified repository in GitHub. It must be
#' pointed at a commit by providing a Git reference, which can be either a SHA, branch or
#' tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param name (string) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) Either a SHA, branch or tag used to identify the commit
#'   the branch is pointing at. Default: `"master"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_branch()` returns a list of the branch's properties.
#'
#' **Branch Properties:**
#'
#' - **name**: The name of the branch.
#' - **ref**: The full Git reference of the branch.
#' - **sha**: The commit SHA the branch is pointing at.
#'
#' @examples
#' \dontrun{
#'   create_branch(
#'     name = "new-branch",
#'     repo = "ChadGoymer/test-githapi",
#'     ref  = "master")
#' }
#'
#' @export
#'
create_branch <- function(
  name,
  repo,
  ref = "master",
  ...)
{
  assert(is_ref(name), "'name' must be a valid git reference - see help(is_ref):\n  ", name)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  if (!is_sha(ref))
  {
    ref <- gh_url("repos", repo, "commits", ref) %>%
      gh_request("GET", accept = "application/vnd.github.VERSION.sha")
  }
  assert(is_sha(ref), "'ref' must be a 40 character string:\n  ", ref)

  payload <- list(ref = str_c("refs/heads/", name), sha = ref)

  info("Creating branch '", name, "' in repository '", repo, "'")
  branch_lst <- gh_url("repos", repo, "git/refs") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  branch_gh <- select_properties(branch_lst, properties$branch) %>%
    modify_list(name = basename(branch_lst$ref), .before = "ref")

  info("Done", level = 7)
  structure(
    branch_gh,
    class   = class(branch_lst),
    url     = attr(branch_lst, "url"),
    request = attr(branch_lst, "request"),
    status  = attr(branch_lst, "status"),
    header  = attr(branch_lst, "header"))
}
