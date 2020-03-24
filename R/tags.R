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


#  FUNCTION: update_tag -----------------------------------------------------------------------
#
#' Update a tag in a repository
#'
#' This function updates a tag in the specified repository to point at a new commit. It must
#' be pointed at a commit by providing a Git reference, which can be either a SHA, branch or
#' tag. For a branch, the head commit is used.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#update-a-reference>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string) Either a SHA, branch or tag used to identify the new commit the tag
#'   is pointing at.
#' @param force (boolean, optional) Whether to force the update if it is not a simple
#'   fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_tag()` returns a list of the tag properties.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **ref**: The full Git reference of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#'
#' @examples
#' \dontrun{
#'   update_tag(
#'     tag  = "new-tag",
#'     repo = "ChadGoymer/test-githapi",
#'     ref  = "6b7b5a090d47fd3ef495620513a3f80da2487b1d")
#' }
#'
#' @export
#'
update_tag <- function(
  tag,
  repo,
  ref,
  force = FALSE,
  ...)
{
  assert(is_ref(tag), "'tag' must be a valid git reference - see help(is_ref):\n  ", tag)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_logical(force), "'force' must be boolean:\n  ", force)

  if (!is_sha(ref))
  {
    ref <- gh_url("repos", repo, "commits", ref) %>%
      gh_request("GET", accept = "application/vnd.github.VERSION.sha")
  }
  assert(is_sha(ref), "'ref' must be a 40 character string:\n  ", ref)

  info("Updating tag '", tag, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "git/refs/tags", tag) %>%
    gh_request("PATCH", payload = list(sha = ref, force = force), ...)

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


#  FUNCTION: view_tags ------------------------------------------------------------------------
#
#' View tags within a repository
#'
#' `view_tags()` summarises tags in a table with the properties as columns and a row for each
#' tag in the repository. `view_tag()` returns a list of all properties for a single tag.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/git/refs/#list-matching-references>
#' - <https://developer.github.com/v3/git/refs/#get-a-single-reference>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
# @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_tags()` returns a tibble of tag properties. `view_tag()` returns a list of
#'   properties for a single tag.
#'
#' **Tag Properties:**
#'
#' - **name**: The name of the tag.
#' - **ref**: The full Git reference of the tag.
#' - **sha**: The commit SHA the tag is pointing at.
#'
#' @examples
#' \dontrun{
#'   # View all tags in a repository
#'   view_tags("ChadGoymer/test-githapi")
#'
#'   # View a single tag
#'   view_tag("new-tag", "ChadGoymer/test-githapi")
#' }
#'
#' @export
#'
# TODO: Uncomment in version 1.0
# view_tags <- function(
#   repo,
#   n_max = 1000,
#   ...)
# {
#   assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
#
#   info("Viewing tags for repository '", repo, "'")
#   tags_lst <- gh_url("repos", repo, "git/refs/tags") %>%
#     gh_page(n_max = n_max, ...)
#
#   info("Transforming results", level = 4)
#   tags_gh <- bind_properties(tags_lst, properties$reference) %>%
#     add_column(name = basename(.$ref), .before = "ref")
#
#   info("Done", level = 7)
#   structure(
#     tags_gh,
#     class   = class(tags_gh),
#     url     = attr(tags_lst, "url"),
#     request = attr(tags_lst, "request"),
#     status  = attr(tags_lst, "status"),
#     header  = attr(tags_lst, "header"))
# }
#
#
#  FUNCTION: view_tag -------------------------------------------------------------------------
#
# @rdname view_tags
# @export
#
view_tag <- function(
  tag,
  repo,
  ...)
{
  assert(is_ref(tag), "'tag' must be a valid git reference - see help(is_ref):\n  ", tag)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  info("Viewing tag '", tag, "' in repository '", repo, "'")
  tag_lst <- gh_url("repos", repo, "git/ref/tags", tag) %>%
    gh_request("GET", ...)

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
