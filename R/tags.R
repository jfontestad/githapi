#  FUNCTION: create_tag -----------------------------------------------------------------------
#
#' Create a tag in a repository
#'
#' This function creates a new tag in the specified repository in GitHub. It must be
#' pointed at a commit by providing a Git reference, which can be either a SHA, branch or
#' tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param name (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) Either a SHA, branch or tag used to identify the commit
#'   the tag is pointing at. Default: `"master"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_tag()` returns a list of the tag's properties.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **ref**: The full Git reference of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#'
#' @examples
#' \dontrun{
#'   create_tag(
#'     name = "new-tag",
#'     repo = "ChadGoymer/test-githapi",
#'     ref  = "master")
#' }
#'
#' @export
#'
create_tag <- function(
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

  payload <- list(ref = str_c("refs/tags/", name), sha = ref)

  info("Creating tag '", name, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "git/refs") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  tag_gh <- select_properties(tag_lst, properties$reference) %>%
    modify_list(name = basename(tag_lst$ref), .before = "ref")

  info("Done", level = 7)
  structure(
    tag_gh,
    class   = class(tag_lst),
    url     = attr(tag_lst, "url"),
    request = attr(tag_lst, "request"),
    status  = attr(tag_lst, "status"),
    header  = attr(tag_lst, "header"))
}
