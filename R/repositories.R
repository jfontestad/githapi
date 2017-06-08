#  FUNCTION: gh_repo --------------------------------------------------------------------------
#' Get repository
#'
#' \url{https://developer.github.com/v3/repos/#get}
#'
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{gh::gh())}.
#' @return A list describing the repository (see GitHub's API documentation for details).
#' @export
gh_repo <- function(
  repo,
  token = gh_token(),
  api   = gh_api(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo", repo = repo, .token = token, .api_url = api)
}

#  FUNCTION: gh_repos -------------------------------------------------------------------------
#' List user repositories
#'
#' \url{https://developer.github.com/v3/repos/#list-user-repositories}
#'
#' @param owner (string) The user or organisation owning the repository.
#' @param type (string, optional) Can be one of all, owner, member. Default: owner
#' @param sort (string, optional) Can be one of created, updated, pushed, full_name.
#'   Default: full_name.
#' @param direction (string, optional) Can be one of asc or desc. Default: when using full_name:
#'   asc, otherwise desc.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   string stored in the environment variable \code{GITHUB_TOKEN} or \code{GITHUB_PAT}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{GITHUB_API_URL} or \code{https://api.github.com}.
#' @param ... Parameters passed to \code{gh::gh())}.
#' @return A list where each element describes a repository (see GitHub's API documentation for
#'   more detail).
#' @export
gh_repos <- function(
  owner,
  type      = "owner",
  sort      = "full_name",
  direction = ifelse(sort == "full_name", "asc", "desc"),
  token     = gh_token(),
  api       = gh_api(),
  ...)
{
  assert_that(is.string(owner) && identical(str_count(owner, "/"), 0L))
  assert_that(is.string(type) && type %in% c("owner", "member"))
  assert_that(is.string(sort) && sort %in% c("created", "updated", "pushed", "full_name"))
  assert_that(is.string(direction) && direction %in% c("asc", "desc"))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api))

  repos <- try(silent = TRUE, {
    gh("/users/:user/repos", user = owner,
       type = type, sort = sort, direction = direction,
       .token = token, .api_url = api)
  })

  if (is(repos, "try-error")) {
    repos <- try(silent = TRUE, {
      gh("/orgs/:org/repos", org = owner,
         type = type, sort = sort, direction = direction,
         .token = token, .api_url = api)
    })
  }

  if (is(repos, "try-error")) stop(repos)

  repos %>%
    map(flatten_) %>%
    bind_rows %>%
    select(name, owner_login, owner_type, description, html_url, url, created_at,updated_at, size, open_issues, default_branch) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

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
  token = gh_token(),
  api   = gh_api(),
  ...)
{
  assert_that(is.string(branch))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo/branches/:branch", repo = repo, branch = branch,
     .token = token, .api_url = api, ...)
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
  token    = gh_token(),
  api      = gh_api(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.flag(extended))
  assert_that(is.string(token) && identical(str_length(githapi:::gh_token()), 40L))
  assert_that(is.string(api))

  branch_tbl <- gh("/repos/:repo/branches", repo = repo, .token = token, .api_url = api, ...) %>%
    map(flatten) %>%
    bind_rows

  if (extended) {
    branch_tbl <- map(branch_tbl$name, gh_branch, repo = repo, token = token, api = api, ...) %>%
      map("commit") %>%
      map("commit") %>%
      map(flatten_) %>%
      bind_rows %>%
      mutate(name = branch_tbl$name, sha = basename(url), date = parse_datetime(author_date)) %>%
      select(name, sha, date, everything(), -author_date, -committer_date, -comment_count)
  }

  branch_tbl
}
