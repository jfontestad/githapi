#  FUNCTION: view_trees -----------------------------------------------------------------------
#
#' View information about Git trees
#'
#' This function returns details about the trees of commits for the specified repository in
#' GitHub. A tree describes the the blobs, or files, usually in a commit.
#'
#' <https://developer.github.com/v3/git/trees/#get-a-tree>
#'
#' @param shas (character) The SHAs of the trees.
#' @param repo (string) The repository specified in the format: `owner/repo`.
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
  shas,
  repo,
  recursive = TRUE,
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  {
    (is_character(shas) && all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_scalar_logical(recursive)) ||
      error("'recursive' must be boolean:\n  '", paste(recursive, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  trees_list <- try_map(shas, function(sha) {
    info("Getting tree for sha '", sha, "' from repository '", repo, "'")

    tree <- gh_request(
      "GET", url = gh_url("repos", repo, "git/trees", sha, recursive = as.integer(recursive), api = api),
      token = token, ...)

    info("Transforming results", level = 3)
    trees_tbl <- bind_fields(tree$tree, list(
      tree_sha = "",
      tree_url = "",
      path     = c("path", as = "character"),
      type     = c("type", as = "character"),
      sha      = c("sha",  as = "character"),
      size     = c("size", as = "integer"),
      url      = c("url",  as = "character"))) %>%
      mutate(tree_sha = tree$sha, tree_url = tree$url)

    info("Done", level = 3)
    trees_tbl
  })

  info("Combining results", level = 3)
  trees_tbl <- bind_rows(trees_list)

  info("Done", level = 3)
  trees_tbl
}

#  FUNCTION: create_tree ----------------------------------------------------------------------
#
#' Create a tree.
#'
#' This function creates the specified tree within a repository in GitHub. The tree is
#' specified by supplying SHAs of existing blobs and trees. Therefore, new blobs have to be
#' created first using [create_blobs()] and directories have to be created by recursively
#' using this function to create trees.
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
  paths,
  modes,
  types,
  shas,
  repo,
  base_tree,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(base_tree)) {
      base_tree <- NA
    }

    (is_character(paths)) ||
      error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
    (is_character(modes) && all(modes %in% c("100644", "100755", "040000", "160000", "120000"))) ||
      error("'modes' must be a character vector of valid modes (see ?create_tree):\n  '", paste(modes, collapse = "'\n  '"), "'")
    (identical(length(modes), 1L) || identical(length(modes), length(paths))) ||
      error("'modes' must be a string or the same length as paths:\n  'modes': ", length(modes), "\n  'paths': ", length(paths))
    (is_character(types) && all(types %in% c("blob", "tree", "commit"))) ||
      error("'types' must be a character vector of valid types (see ?create_tree):\n  '", paste(types, collapse = "'\n  '"), "'")
    (identical(length(types), 1L) || identical(length(types), length(paths))) ||
      error("'types' must be a string or the same length as paths:\n  'types': ", length(types), "\n  'paths': ", length(paths))
    (all(is_na(shas)) || all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must be NA or a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (all(is_na(shas)) || (identical(length(shas), 1L) || identical(length(shas), length(paths)))) ||
      error("'shas' must be NA, a string or the same length as 'paths':\n  'shas':  ", length(shas), "\n  'paths': ", length(paths))
    (all(is_na(base_tree)) || all(gh_map(base_tree, is_sha, simplify = TRUE))) ||
      error("'base_tree' must be NA or a vector of 40 character strings:\n  '", paste(base_tree, collapse = "'\n  '"), "'")
    (all(is_na(base_tree)) || (identical(length(base_tree), 1L)) || identical(length(base_tree), length(paths))) ||
      error("'base_tree' must be NA, a string or the same length as 'paths':\n  'base_tree':", length(base_tree), "\n  'paths':     ", length(paths))
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  params <- tibble(
    path      = paths,
    mode      = modes,
    type      = types,
    sha       = shas,
    base_tree = base_tree)

  tree <- gh_pmap(params, use_names = FALSE, function(...) remove_missing(list(...)))

  payload <- list(
    base_tree = base_tree,
    tree = tree) %>%
    remove_missing()

  info("Posting tree to repository '", repo, "'")
  tree_list <- try_catch({
    gh_request(
      "POST", url = gh_url("repos", repo, "git/trees", api = api),
      payload = payload, token = token, ...)
  })

  info("Transforming results", level = 3)
  tree_tbl <- bind_fields(tree_list$tree, list(
    tree_sha = "",
    tree_url = "",
    path     = c("path", as = "character"),
    type     = c("type", as = "character"),
    sha      = c("sha",  as = "character"),
    size     = c("size", as = "integer"),
    url      = c("url",  as = "character"))) %>%
    mutate(tree_sha = tree_list$sha, tree_url = tree_list$url)

  info("Done", level = 3)
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
#' @param path (string) The path to the directory, which is to be uploaded as a tree.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param base_tree (string, optional) The SHAs of the tree you want to update with new data.
#'   If not specified, a new tree will be created.
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
  path,
  repo,
  base_tree,
  ignore = c("\\.git", "\\.Rproj\\.user", "\\.Rhistory", "\\.RData", "\\.Ruserdata"),
  token  = getOption("github.token"),
  api    = getOption("github.api"),
  ...)
{
  {
    if (missing(base_tree)) {
      base_tree <- NA
    }

    (is_dir(path) && is_readable(path)) ||
      error("'path' must be a file path to a readable directory:\n  '", paste(path, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (all(is_na(base_tree)) || is_sha(base_tree)) ||
      error("'base_tree' must be NA or a string of 40 characters:\n  '", paste(base_tree, collapse = "'\n  '"), "'")
    (is_character(ignore)) ||
      error("'ignore' must be a character vector:\n  '", paste(ignore, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  ignore <- unique(c("^\\.$", "^\\.\\.$", ignore))

  info("Getting file information from '", path, "'")
  all_files <- list.files(path, all.files = TRUE, include.dirs = TRUE, full.names = TRUE)
  file_path <- all_files[!gh_map(ignore, grepl, basename(all_files)) %>% gh_pmap(any, simplify = TRUE)]
  file_info <- file.info(file_path)

  tree <- file_info %>%
    mutate(path = file_path) %>%
    mutate(sha = gh_pmap(list(file_path, file_info$isdir), simplify = TRUE, function(p, isdir) {
      if (isdir) {
        upload_tree(path = p, repo = repo, token = token, api = api)[["tree_sha"]][[1]]
      } else {
        upload_blobs(paths = p, repo = repo, token = token, api = api)[["sha"]]
      }
    })) %>%
    mutate(
      type = ifelse(file_info$isdir, "tree", "blob"),
      mode = ifelse(file_info$isdir, "040000", "100644"),
      path = basename(file_path)) %>%
    select("path", "mode", "type", "sha")

  create_tree(
    paths     = tree$path,
    modes     = tree$mode,
    types     = tree$type,
    shas      = tree$sha,
    repo      = repo,
    base_tree = base_tree,
    token     = token,
    api       = api)
}

#  FUNCTION: trees_exist ----------------------------------------------------------------------
#
#' Determine whether a tree exists in the specified repository.
#'
#' This function returns `TRUE` if the tree exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param shas (character) The SHAs of the trees.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `TRUE` or `FALSE`
#'
#' @export
#'
trees_exist <- function(
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(shas) && all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  gh_map(shas, simplify = TRUE, function(sha) {
    info("Checking tree '", sha, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "git/trees", sha, api = api),
        token = token, ...)
      TRUE
    }, on_error = function(e) {
      FALSE
    })
  })
}
