#  FUNCTION: gh_branch ------------------------------------------------------------------------
#' Get branch
#'
#' \url{https://developer.github.com/v3/repos/branches/#get-branch}
#'
#' @param branch (string) The branch name.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{gh::gh())}.
#' @return A list describing the branch (see GitHub's API documentation for details).
#' @export
gh_branch <- function(
  branch,
  repo,
  token   = gh_token(),
  api_url = gh_api_url(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api_url))

  path <- file.path("/repos", repo, "branches")
  if (!missing(branch)) {
    assert_that(is.string(branch))
    path <- file.path(path, branch)
  }

  gh(path, .token = token, .api_url = api_url, ...)
}

#  FUNCTION: gh_branches ----------------------------------------------------------------------
#' Get information about all the head commits in each branch.
#'
#' This function returns useful information about the head commits of each branch in the
#' repository. If \code{extended = FALSE} a tibble with the columns "branch", "sha" and "url"
#' is returned with a row for each branch. If \code{extended = TRUE} the the tibble contains
#' the columns "branch", "sha", date", "author_name", "author_email", "committer_name",
#' "committer_email", "message" and "url".
#'
#' Note: Extended results requires an separate API call for every required branch. This may
#' impact performance of the function considerably depending on the number of branches.
#'
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{gh::gh())}.
#' @return A tibble with columns "branch", "sha" and "url". When \code{extended = TRUE} the columns
#'   "date", "author_name", "author_email", "committer_name", "committer_email" and "message" are
#'   added.
#' @export
gh_branches <- function(
  repo,
  extended = FALSE,
  token   = gh_token(),
  api_url = gh_api_url(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.flag(extended))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api_url))

  branch_tbl <- gh_branch(repo = repo, token = token, api_url = api_url, ...) %>%
    map(flatten) %>%
    bind_rows

  if (extended) {
    branch_tbl <- map(branch_tbl$name, gh_branch, repo = repo, token = token, api_url = api_url, ...) %>%
      map("commit") %>%
      map("commit") %>%
      map(flatten_) %>%
      bind_rows %>%
      mutate(name = branch_tbl$name, sha = basename(url), date = parse_datetime(author_date)) %>%
      select(name, sha, date, everything(), -author_date, -committer_date, -comment_count)
  }

  branch_tbl
}
