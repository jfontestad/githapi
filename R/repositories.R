#  FUNCTION: gh_repo --------------------------------------------------------------------------
#' Get information about a repository.
#'
#' \url{https://developer.github.com/v3/repos/#get}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A list describing the repository (see GitHub's API documentation for details).
#' @export
gh_repo <- function(
  repo,
  token = gh_token(),
  api   = gh_api(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo", repo = repo, .token = token, .api_url = api)
}

#  FUNCTION: gh_repos -------------------------------------------------------------------------
#' Get information about a user or organisation's repositories.
#'
#' \url{https://developer.github.com/v3/repos/#list-user-repositories}
#' \url{https://developer.github.com/v3/repos/#list-organization-repositories}
#'
#' @param owner (string) The user or organisation owning the repository.
#' @param type (string, optional) Can be one of \code{"all"}, \code{"owner"}, \code{"member"}.
#'   Default: \code{"owner"}.
#' @param sort (string, optional) Can be one of \code{"created"}, \code{"updated"},
#'   \code{"pushed"}, \code{"full_name"}. Default: \code{"full_name"}.
#' @param direction (string, optional) Can be one of \code{"asc"} or \code{"desc"}. Default:
#'   when using \code{"full_name"}: \code{"asc"}, otherwise \code{"desc"}.
#' @param limit (integer, optional) The maximum number to return. Default: \code{Inf}, which
#'   returns all.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A tibble describing all the repositories a user or organisation has (see GitHub's
#'   API documentation for more detail).
#' @export
gh_repos <- function(
  owner,
  type      = "owner",
  sort      = "full_name",
  direction = ifelse(sort == "full_name", "asc", "desc"),
  limit     = 1000,
  token     = gh_token(),
  api       = gh_api(),
  ...)
{
  assert_that(is.string(owner) && identical(str_count(owner, "/"), 0L))
  assert_that(is.string(type) && type %in% c("owner", "member"))
  assert_that(is.string(sort) && sort %in% c("created", "updated", "pushed", "full_name"))
  assert_that(is.string(direction) && direction %in% c("asc", "desc"))
  assert_that(is.infinite(limit) || is.count(limit))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  repos <- try(silent = TRUE, {
    gh("/orgs/:org/repos", org = owner,
       type = type, sort = sort, direction = direction,
       .token = token, .api_url = api, .limit = limit, per_page = 100, ...)
  })

  if (is(repos, "try-error")) {
    repos <- gh("/users/:user/repos", user = owner,
       type = type, sort = sort, direction = direction,
       .token = token, .api_url = api, .limit = limit, per_page = 100, ...)
  }

  repos %>%
    map(flatten_) %>%
    bind_rows %>%
    select(name, owner_login, owner_type, description, html_url, url, created_at, updated_at, size, open_issues, default_branch) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_tags --------------------------------------------------------------------------
#' Get information about all the tags in a repository.
#'
#' \url{https://developer.github.com/v3/repos/#list-tags}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param limit (integer, optional) The maximum number to return. Default: \code{Inf}, which
#'   returns all.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A tibble describing all the tags (see GitHub's API documentation for details).
#' @export
gh_tags <- function(
  repo,
  limit    = 1000,
  token    = gh_token(),
  api      = gh_api(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.infinite(limit) || is.count(limit))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo/tags", repo = repo,
     .token = token, .api_url = api, .limit = limit, per_page = 100, ...) %>%
    map(flatten) %>%
    bind_rows %>%
    select(name, sha, url, zipball_url, tarball_url)
}

#  FUNCTION: gh_branch ------------------------------------------------------------------------
#' Get information about a branch.
#'
#' \url{https://developer.github.com/v3/repos/branches/#get-branch}
#'
#' @param branch (string) The branch name.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
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
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo/branches/:branch", repo = repo, branch = branch,
     .token = token, .api_url = api, ...)
}

#  FUNCTION: gh_branches ----------------------------------------------------------------------
#' Get information about all the head commits in each branch.
#'
#' \url{https://developer.github.com/v3/repos/branches/#list-branches}
#'
#' Note: Extended results requires an separate API call for every required branch. This may
#' impact performance of the function considerably depending on the number of branches.
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param extended (logical, optional) Whether to return extended information for each branch.
#'   Warning: this may take some time for a lot of branches. Default: \code{FALSE}.
#' @param limit (integer, optional) The maximum number to return. Default: \code{Inf}, which
#'   returns all.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#' @export
gh_branches <- function(
  repo,
  extended = FALSE,
  limit    = 1000,
  token    = gh_token(),
  api      = gh_api(),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.flag(extended))
  assert_that(is.infinite(limit) || is.count(limit))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  branches <- gh("/repos/:repo/branches", repo = repo,
     .token = token, .api_url = api, .limit = limit, per_page = 100, ...) %>%
    map(flatten) %>%
    bind_rows

  if (extended) {
    branches <- map(branches$name, gh_branch, repo = repo, token = token, api = api, ...) %>%
      map("commit") %>%
      map("commit") %>%
      map(flatten_) %>%
      bind_rows %>%
      mutate(name = branches$name, sha = basename(url), date = parse_datetime(author_date)) %>%
      select(name, sha, date, everything(), -author_date, -committer_date, -comment_count)
  }

  branches
}

#  FUNCTION: gh_commit ------------------------------------------------------------------------
#' Get information about a commit.
#'
#' \url{https://developer.github.com/v3/repos/commits/#get-a-single-commit}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A list describing the branch (see GitHub's API documentation for details).
#' @export
gh_commit <- function(
  ref,
  repo,
  token = gh_token(),
  api   = gh_api(),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh("/repos/:repo/commits/:ref", repo = repo, ref = ref,
     .token = token, .api_url = api, ...)
}

#  FUNCTION: gh_commits -----------------------------------------------------------------------
#' Get information about all the history of commits.
#'
#' \url{https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param extended (logical, optional) Whether to return extended information for each branch.
#'   Warning: this may take some time for a lot of branches. Default: \code{FALSE}.
#' @param limit (integer, optional) The maximum number to return. Default: \code{Inf}, which
#'   returns all.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#' @export
gh_commits <- function(
  ref,
  repo,
  extended = FALSE,
  limit    = 1000,
  token    = gh_token(),
  api      = gh_api(),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.flag(extended))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.infinite(limit) || is.count(limit))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  commits <- gh("/repos/:repo/commits", repo = repo, sha = ref,
     .token = token, .api_url = api, .limit = limit, per_page = 100, ...) %>%
    map("commit") %>%
    map(flatten_) %>%
    bind_rows %>%
    mutate(sha = basename(url), date = parse_datetime(author_date)) %>%
    select(sha, date, everything(), -author_date, -committer_date, -comment_count)

  if (extended) {
    commits <- mutate(commits, files = map(sha, function(.sha) {
      gh_commit(ref = .sha, repo = repo, token = token, api = api, ...) %>%
        .[["files"]] %>%
        bind_rows %>%
        select(filename, status, additions, deletions, changes, contents_url)
    }))
  }

  commits
}
