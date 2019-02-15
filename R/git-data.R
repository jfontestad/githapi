#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
#
#' Get a blob
#'
#' NOTE: This function has been deprecated, please use [view_blobs()] instead.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param sha (string) SHA-1 of the blob
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A string containing the contents of the specified file (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
gh_git_blob <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_blobs", package = "githapi")

  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  blob <- gh_get(
    gh_url("repos", repo, "git/blobs", sha, api = api),
    accept = "raw", token = token, ...)

  attr(blob, "header") <- NULL
  blob
}

#  FUNCTION: gh_git_commit --------------------------------------------------------------------
#
#' Get a commit
#'
#' NOTE: This function has been deprecated, please use [view_commits()] instead.
#'
#' <https://developer.github.com/v3/git/commits/#get-a-commit>
#'
#' @param sha (string) The SHA-1 of the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the commit (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_commit <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_commits", package = "githapi")

  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/commits", sha, api = api),
    token = token, ...)
}

#  FUNCTION: gh_git_reference -----------------------------------------------------------------
#
#' Get a reference
#'
#' NOTE: This function has been deprecated, please use [view_tags()] or [view_branches()] instead.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param ref (string) tags, specified as `tags/<tag_name>`, or branches, specified as
#'   `heads/<branch_name>`
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the reference (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_reference <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(c("view_tags' or 'view_branches"), package = "githapi")

  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/refs", ref, api = api),
    token = token, ...)
}

#  FUNCTION: is_tag ---------------------------------------------------------------------------
#
#' Check whether the input is a valid tag
#'
#' NOTE: This function has been deprecated, please use [tags_exist()] instead.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param tag (string) tag name.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A boolean, with attributes describing the errors, if there are any.
#'
#' @export
#'
is_tag <- function(
  tag,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(c("tags_exist"), package = "githapi")

  if (!is_string(tag)) {
    return(FALSE)
  }

  result <- try(
    gh_git_reference(ref = paste0("tags/", tag), repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    return(FALSE)
  }

  TRUE
}

#  FUNCTION: gh_git_references ----------------------------------------------------------------
#
#' Get all references
#'
#' NOTE: This function has been deprecated, please use [view_tags()] or [view_branches()] instead.
#'
#' <https://developer.github.com/v3/git/refs/#get-all-references>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the references (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_references <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(c("view_tags' or 'view_branches"), package = "githapi")

  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  ref_map <- c(
    "refs/heads" = "branch",
    "refs/tags"  = "tag",
    "head"       = "pull request",
    "merge"      = "pull merge")

  references <- gh_page(
    gh_url("repos", repo, "git/refs", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(references, list(
    name        = "",
    type        = "",
    object_type = c("object", "type", as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"))) %>%
    mutate(
      name = ifelse(grepl("pull", .data$ref), basename(dirname(.data$ref)), basename(.data$ref)),
      type = ifelse(grepl("pull", .data$ref), ref_map[basename(.data$ref)], ref_map[dirname(.data$ref)]))
}

#  FUNCTION: gh_git_tag -----------------------------------------------------------------------
#
#' Get an annotated tag
#'
#' NOTE: This function has been deprecated, it will be removed in a future version.
#'
#' <https://developer.github.com/v3/git/tags/#get-a-tag>
#'
#' @param sha (string) The SHA-1 of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the tag (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_tag <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(package = "githapi")

  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/tags", sha, api = api),
    token = token, ...)
}

#  FUNCTION: gh_git_tree ----------------------------------------------------------------------
#
#' Get a tree
#'
#' <https://developer.github.com/v3/git/trees/#get-a-tree>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param recursive (logical) Whether to list files recursively. Default: TRUE.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the files in a commit (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_git_tree <- function(
  ref,
  repo,
  recursive = TRUE,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_boolean(recursive))
  assert(is_sha(token))
  assert(is_url(api))

  tree <- gh_get(
    gh_url("repos", repo, "git/trees", ref, recursive = as.integer(recursive), api = api),
    token = token, ...)

  bind_fields(tree$tree, list(
    path = c("path", as = "character"),
    type = c("type", as = "character"),
    sha  = c("sha",  as = "character"),
    size = c("size", as = "integer"),
    url  = c("url",  as = "character")))
}

#  FUNCTION: gh_save --------------------------------------------------------------------------
#
#' Download files and save them to a location
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param files (string) The paths to the files in the repository.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string) The location to save the files to.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_git_tree()].
#'
#' @return The file path of the saved file (invisibly).
#'
#' @export
#'
gh_save <- function(
  files,
  repo,
  path,
  ref   = "master",
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(files))
  assert(is_repo(repo))
  assert(is_string(path))
  assert(is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  repo_files <- gh_git_tree(ref = ref, repo = repo, recursive = TRUE, token = token, api = api, ...)

  if (all(files %in% repo_files$path)) {
    blob_paths <- repo_files[repo_files$path %in% files,][["path"]] %>%
      set_names(repo_files[repo_files$path %in% files,][["sha"]])
  } else {
    error(
      "Cannot find specified files in repo '", repo, "':\n\n  ",
      paste(files, collapse = "\n  "))
  }

  if (!dir.exists(path)) dir.create(path)

  for (blob in names(blob_paths)) {
    gh_download_binary(
      gh_url("repos", repo, "git/blobs", blob, api = api),
      path = file.path(path, basename(blob_paths[[blob]])), token = token)
  }

  invisible(path)
}

#  FUNCTION: gh_source ------------------------------------------------------------------------
#
#' Source an R file
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param file (string) The path to the file in the repository
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [base::source()].
#'
#' @return The result of sourcing the file.
#'
#' @export
#'
gh_source <- function(
  file,
  repo,
  ref   = "master",
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(file))
  assert(is_repo(repo))
  assert(is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  path <- file.path(tempdir(), "gh_source")
  on.exit(unlink(path, recursive = TRUE))

  gh_save(files = file, repo = repo, path = path, ref = ref, token = token, api = api)
  source(file.path(path, basename(file)), ...)
}
