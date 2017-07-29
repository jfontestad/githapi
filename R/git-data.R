#  FUNCTION: gh_blob --------------------------------------------------------------------------
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
gh_blob <- function(
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
#' @param ... Parameters passed to \code{\link{gh}}.
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
