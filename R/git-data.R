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
#' @return A string containing the contents of the specified file.
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
    gh_get(token = token, ...)
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
#' @param ... Parameters passed to \code{\link{gh_page}}.
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
    gh_page(token = token, ...)
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
#' @param ... Parameters passed to \code{\link{gh_page}}.
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
    gh_page(token = token, ...)
}

#  FUNCTION: gh_git_references ----------------------------------------------------------------
#' Get all references
#'
#' url{https://developer.github.com/v3/git/refs/#get-all-references}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return a tibble describing the references
#' @export
gh_git_references <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  ref_map <- c("refs/heads" = "branch", "refs/tags" = "tag")

  gh_url("repos", repo, "git/refs", api = api) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(name = basename(ref), type = ref_map[dirname(ref)]) %>%
    select(name, type, object_type, object_sha, ref, url)
}
