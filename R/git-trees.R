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

#  FUNCTION: create_tree ----------------------------------------------------------------------
#
#' Create a tree.
#'
#' This function creates the specified tree within a repository in GitHub. The tree is either
#' specified by supplying SHAs of existing blobs and trees, or by specifying the text contents
#' of files, which are uploaded as blobs.
#'
#' <https://developer.github.com/v3/git/trees/#create-a-tree>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (character) The paths to the files or directories.
#' @param modes (character) The file modes; one of `"100644"` for file (blob), `"100755"` for
#'   executable (blob), `"040000"` for subdirectory (tree), `"160000"` for submodule (commit), or
#'   `"120000"` for a blob that specifies the path of a symlink.
#' @param types (character) The object types. Either `"blob"`, `"tree"`, or `"commit"`.
#' @param shas (character) The SHAs of the objects in the tree.
#' @param contents (character) The contents of each file. GitHub will write this blob out and use
#'   that SHA for this entry. Use either this, or sha.
#' @param base_tree (string, optional) The SHAs of the tree you want to update with new data.
#'   Default: `NA`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the trees, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **sha**: The SHA of the tree.
#'   - **url**: The URL to get the tree details from GitHub.
#'
#' @export
#'
create_tree <- function(
  repo,
  paths,
  modes,
  types,
  shas      = NA,
  contents  = NA,
  base_tree = NA,
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(paths))
  assert(is_character(modes) && all(modes %in% c("100644", "100755", "040000", "160000", "120000")))
  assert(identical(length(modes), 1L) || identical(length(modes), length(paths)))
  assert(is_character(types) && all(types %in% c("blob", "tree", "commit")))
  assert(identical(length(types), 1L) || identical(length(types), length(paths)))
  assert(is_na(shas) || all(map_vec(shas, is_sha)))
  assert(is_na(shas) || (identical(length(shas), 1L) || identical(length(shas), length(paths))))
  assert(is_na(contents) || is_character(contents))
  assert(is_na(contents) || (identical(length(contents), 1L) || identical(length(contents), length(paths))))
  assert(is_na(base_tree) || all(map_vec(base_tree, is_sha)))
  assert(is_na(base_tree) || (identical(length(base_tree), 1L)) || identical(length(base_tree), length(paths)))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    path      = paths,
    mode      = modes,
    type      = types,
    sha       = shas,
    content   = contents,
    base_tree = base_tree)

  tree <- pmap(params, use_names = FALSE, function(...) remove_missing(list(...)))

  payload <- list(
    base_tree = base_tree,
    tree = tree) %>%
    remove_missing()

  info("Posting tree to repository '", repo, "'")
  tree_list <- tryCatch({
    gh_request(
      "POST", gh_url("repos", repo, "git/trees", api = api),
      payload = payload, token = token, ...)
  }, error = function(e) {
    error("Creation of tree failed: ", e$message)
  })

  info("Transforming results")
  tree_tbl <- bind_fields(tree_list$tree, list(
    tree_sha = "",
    tree_url = "",
    path     = c("path", as = "character"),
    type     = c("type", as = "character"),
    sha      = c("sha",  as = "character"),
    size     = c("size", as = "integer"),
    url      = c("url",  as = "character"))) %>%
    mutate(tree_sha = tree_list$sha, tree_url = tree_list$url)

  info("Done")
  tree_tbl
}

#  FUNCTION: upload_tree ----------------------------------------------------------------------
#
#' Upload a tree from a directory
#'
#' This function uploads the tree structure from a specified directory into a repository in
#' GitHub. Files are uploaded as blobs (see [upload_blobs()]) and sub-directories are
#' uploaded as nested trees [create_tree()].
#'
#' <https://developer.github.com/v3/git/trees/#create-a-tree>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string) The path to the directory, which is to be uploaded as a tree.
#' @param base_tree (string, optional) The SHAs of the tree you want to update with new data.
#'   Default: `NA`.
#' @param ignore (character) A character vector of regular expressions. If any of these are
#'   detected in a file name they are not uploaded. Default: `"\\.git"`, `"\\.Rproj\\.user"`,
#'   `"\\.Rhistory"`, `"\\.RData"` & `"\\.Ruserdata"`
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the tree, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **sha**: The SHA of the tree.
#'   - **url**: The URL to get the tree details from GitHub.
#'
#' @export
#'
upload_tree <- function(
  repo,
  path,
  base_tree = NA,
  ignore    = c("\\.git", "\\.Rproj\\.user", "\\.Rhistory", "\\.RData", "\\.Ruserdata"),
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_readable(path))
  assert(is_na(base_tree) || is_sha(base_tree))
  assert(is_character(ignore))
  assert(is_sha(token))
  assert(is_url(api))

  ignore <- unique(c("^\\.$", "^\\.\\.$", ignore))

  all_files <- list.files(path, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)
  file_path <- all_files[!map(ignore, grepl, basename(all_files)) %>% pmap_vec(any)]
  file_info <- file.info(file_path)

  tree <- file_info %>%
    mutate(path = file_path) %>%
    mutate(sha = pmap_vec(list(file_path, file_info$isdir), function(p, isdir) {
      if (isdir) {
        upload_tree(repo = repo, path = p, token = token, api = api)[["tree_sha"]][[1]]
      } else {
        upload_blobs(repo = repo, paths = p, token = token, api = api)[["sha"]]
      }
    })) %>%
    mutate(
      type = ifelse(file_info$isdir, "tree", "blob"),
      mode = ifelse(file_info$isdir, "040000", ifelse(file_info$exe == "no", "100644", "100755")),
      path = basename(file_path)) %>%
    select("path", "mode", "type", "sha")

  create_tree(
    repo      = repo,
    paths     = tree$path,
    modes     = tree$mode,
    types     = tree$type,
    shas      = tree$sha,
    base_tree = base_tree,
    token     = token,
    api       = api)
}
