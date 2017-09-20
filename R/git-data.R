#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
#' Get a blob
#'
#' url{https://developer.github.com/v3/git/blobs/#get-a-blob}
#'
#' @param sha (string) SHA-1 of the blob
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A string containing the contents of the specified file (see GitHub's API
#'   documentation for details).
#' @export
gh_git_blob <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(sha) && identical(str_length(sha), 40L))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "git/blobs", sha, api = api) %>%
    gh_get(accept = "raw", token = token, ...)
}

#  FUNCTION: gh_git_commit --------------------------------------------------------------------
#' Get a commit
#'
#' url{https://developer.github.com/v3/git/commits/#get-a-commit}
#'
#' @param sha (string) The SHA-1 of the commit.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the commit (see GitHub's API documentation for details).
#' @export
gh_git_commit <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(sha) && identical(str_length(sha), 40L))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "git/commits", sha, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_git_reference -----------------------------------------------------------------
#' Get a reference
#'
#' url{https://developer.github.com/v3/git/refs/#get-a-reference}
#'
#' @param ref (string) tags, specified as \code{tags/<tag_name>}, or branches, specified as
#'   \code{heads/<branch_name>}
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the reference (see GitHub's API documentation for details).
#' @export
gh_git_reference <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "git/refs", ref, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_git_references ----------------------------------------------------------------
#' Get all references
#'
#' url{https://developer.github.com/v3/git/refs/#get-all-references}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the references (see GitHub's API documentation for details).
#' @export
gh_git_references <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  ref_map <- c(
    "refs/heads" = "branch",
    "refs/tags"  = "tag",
    "head"       = "pull request",
    "merge"      = "pull merge")

  gh_url("repos", repo, "git/refs", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      name = ifelse(str_detect(ref, "pull"), basename(dirname(ref)), basename(ref)),
      type = ifelse(str_detect(ref, "pull"), ref_map[basename(ref)], ref_map[dirname(ref)])) %>%
    select(name, type, object_type, object_sha, ref, url)
}

#  FUNCTION: gh_git_tag -----------------------------------------------------------------------
#' Get an annotated tag
#'
#' url{https://developer.github.com/v3/git/tags/#get-a-tag}
#'
#' @param sha (string) The SHA-1 of the tag.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the tag (see GitHub's API documentation for details).
#' @export
gh_git_tag <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(sha) && identical(str_length(sha), 40L))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "git/tags", sha, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_git_tree ----------------------------------------------------------------------
#' Get a tree
#'
#' url{https://developer.github.com/v3/git/trees/#get-a-tree}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the files in a commit (see GitHub's API documentation for
#'   details).
#' @export
gh_git_tree <- function(
  ref,
  repo,
  recursive = TRUE,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "git/trees", ref, recursive = as.integer(recursive), api = api) %>%
    gh_get(sub_list = "tree", simplify = TRUE, token = token, ...) %>%
    select(path, type, sha, size, url)
}

#  FUNCTION: gh_save --------------------------------------------------------------------------
#' Download file and save it
#'
#' url{https://developer.github.com/v3/git/blobs/#get-a-blob}
#'
#' @param path (string) The path to the file in the repository
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param save_to (string, optional) The file path to save the file to. Default: file name
#'   specified in \code{path} in the current working directory.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return The file path of the saved file (invisibly).
#' @export
gh_save <- function(
  path,
  repo,
  save_to = basename(path),
  ref     = "master",
  token   = gh_token(),
  api     = getOption("github.api"),
  ...)
{
  assert_that(is.string(path))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(save_to))
  assert_that(is.string(ref))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  repo_files <- gh_git_tree(ref = ref, repo = repo, recursive = TRUE, token = token, api = api)

  if (path %in% repo_files$path) {
    blob_sha <- repo_files[repo_files$path == path,][["sha"]]
  } else {
    stop("Cannot find specified file path '", path, "' in repo '", repo)
  }

  gh_git_blob(sha = blob_sha, repo = repo, binary = TRUE, token = token, api = api, ...) %>%
    writeBin(save_to)

  invisible(save_to)
}

#  FUNCTION: gh_source ------------------------------------------------------------------------
#' Source an R file
#'
#' url{https://developer.github.com/v3/git/blobs/#get-a-blob}
#'
#' @param path (string) The path to the file in the repository
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{source}}.
#' @return The result of sourcing the file.
#' @export
gh_source <- function(
  path,
  repo,
  ref   = "master",
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(path))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(ref))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  save_to <- file.path(tempdir(), basename(path))
  on.exit(unlink(save_to))

  repo_files <- gh_git_tree(ref = ref, repo = repo, recursive = TRUE, token = token, api = api)

  if (path %in% repo_files$path) {
    blob_sha <- repo_files[repo_files$path == path,][["sha"]]
  } else {
    stop("Cannot find specified file path '", path, "' in repo '", repo)
  }

  gh_git_blob(sha = blob_sha, repo = repo, binary = TRUE, token = token, api = api) %>%
    writeBin(save_to)

  source(save_to, ...)
}
