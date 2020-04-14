#  FUNCTION: create_release -------------------------------------------------------------------
#
#' Create a release in a repository
#'
#' This function creates a new release in the specified repository in GitHub. It must be
#' pointed at a commit by providing an existing tag or a Git reference, which can be either a
#' SHA or branch. If the tag does not exist it is created pointing at the commit; if it does
#' exist then the reference is ignored.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/releases/#create-a-release>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the release.
#' @param body (string, optional) The description of the release.
#' @param ref (string, optional) Either a SHA or branch used to identify the commit. Ignored
#'   if the `tag` exists this `ref` is ignored. Default: "master".
#' @param draft (boolean, optional) Whether the release is a draft. Default: `FALSE`.
#' @param prerelease (boolean, optional) Whether the release is a pre-release.
#'   Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_release()` returns a list of the release's properties.
#'
#' **Release Properties:**
#'
#' - **id**: The id of the release.
#' - **tag**: The tags associated with the release.
#' - **name**: The name of the release.
#' - **body**: The description of the release.
#' - **commit**: The commit associated with the release.
#' - **draft**: Whether the release is draft.
#' - **prerelease**:  Whether it is a pre-release.
#' - **author_login**: The author's account login.
#' - **assets**: The name of the assets associated with the release.
#' - **html_url**: The address of the release's web page.
#' - **created_at**: The time and date the release was created.
#' - **published_at**: The time and date the release was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a release on the master branch
#'   create_release(
#'     tag  = "1.0.0",
#'     repo = "ChadGoymer/githapi",
#'     name = "Initial production release",
#'     body = "This is a release created by create_release()")
#'
#'   # Create a draft pre-release
#'   create_release(
#'     tag        = 1.0.9000,
#'     repo       = "ChadGoymer/githapi",
#'     name       = "Draft 1.1 release",
#'     body       = "This is a release created by create_release()",
#'     ref        = "dev-branch",
#'     draft      = TRUE,
#'     prerelease = TRUE)
#'
#' }
#'
#' @export
#'
create_release <- function(
  tag,
  repo,
  name,
  body,
  ref        = "master",
  draft      = FALSE,
  prerelease = FALSE,
  ...)
{
  assert(is_ref(tag), "'tag' must be a valid git reference - see help(is_ref):\n  ", tag)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_ref(ref), "'ref' must be a valid git reference - see help(is_ref):\n  ", ref)
  assert(is_scalar_logical(draft), "'draft' must be a string:\n  ", draft)
  assert(is_scalar_logical(prerelease), "'prerelease' must be a string:\n  ", prerelease)

  payload <- list(
    tag_name         = tag,
    target_commitish = ref,
    draft            = draft,
    prerelease       = prerelease)

  if (!missing(name))
  {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload$name <- name
  }

  if (!missing(body))
  {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

  info("Creating release '", tag, "' in repository '", repo, "'")
  release_lst <- gh_url("repos", repo, "releases") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  release_gh <- select_properties(release_lst, properties$release) %>%
    modify_list(assets = map_chr(release_lst$assets, "name"), .before = "html_url")

  info("Done", level = 7)
  structure(
    release_gh,
    class   = class(release_lst),
    url     = attr(release_lst, "url"),
    request = attr(release_lst, "request"),
    status  = attr(release_lst, "status"),
    header  = attr(release_lst, "header"))
}
