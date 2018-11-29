#  FUNCTION: view_trees -----------------------------------------------------------------------
#
#' View information about Git trees
#'
#' This function returns details about the trees of commits for the specified repository in
#' GitHub. A tree describes the the blobs, or files, usually in a commit.
#'
#' <https://developer.github.com/v3/git/trees/#get-a-tree>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param shas (character, optional) The SHAs of the trees.
#' @param recursive (logical) Whether to list files recursively. Default: TRUE.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the trees, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/blobs/) for more details):
#'   - **tree_sha**: The SHA of the tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **path**: The path to the object within the tree.
#'   - **type**: The type of the object. Either `"blob"`, `"tree"` or `"commit"`.
#'   - **sha**: The SHA of the object.
#'   - **size**: The size of the object, in bytes.
#'   - **url**: The URL to get the object's details from GitHub.
#'
#' @export
#'
view_trees <- function(
  repo,
  shas,
  recursive = TRUE,
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_boolean(recursive))
  assert(is_sha(token))
  assert(is_url(api))

  trees_list <- map(shas, function(sha) {
    info("Getting tree for sha '", sha, "' from repository '", repo, "'")
    tryCatch({
      tree <- gh_request(
        "GET", gh_url("repos", repo, "git/trees", sha, recursive = as.integer(recursive), api = api),
        token = token, ...)

      info("Transforming results")
      trees_tbl <- bind_fields(tree$tree, list(
        tree_sha = "",
        tree_url = "",
        path     = c("path", as = "character"),
        type     = c("type", as = "character"),
        sha      = c("sha",  as = "character"),
        size     = c("size", as = "integer"),
        url      = c("url",  as = "character"))) %>%
        mutate(tree_sha = tree$sha, tree_url = tree$url)

      info("Done")
      trees_tbl
    }, error = function(e) {
      info("sha '", sha, "' failed!")
      e
    })
  })

  if (any(map_vec(trees_list, is, "error"))) {
    collate_errors(trees_list, "view_tags() failed!")
  }

  info("Combining results")
  trees_tbl <- bind_rows(trees_list)

  info("Done")
  trees_tbl
}
