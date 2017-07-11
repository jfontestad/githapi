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
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, api = api) %>% gh_page(token = token, ...)
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
  type      = NULL,
  sort      = NULL,
  direction = NULL,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.string(owner) && identical(str_count(owner, "/"), 0L))
  assert_that(is.null(type) || is.string(type) && type %in% c("owner", "member"))
  assert_that(is.null(sort) || is.string(sort) && sort %in% c("created", "updated", "pushed", "full_name"))
  assert_that(is.null(direction) || is.string(direction) && direction %in% c("asc", "desc"))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  repos <- try(silent = TRUE, {
    gh_url("orgs", owner, "repos", type = type, sort = sort, direction = direction, api = api) %>%
      gh_page(token = token, ...)
  })

  if (is(repos, "try-error")) {
    repos <- gh_url("users", owner, "repos", type = type, sort = sort, direction = direction, api = api) %>%
      gh_page(token = token, ...)
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
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "tags", api = api) %>%
    gh_page(...) %>%
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
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(branch))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "branches", branch, api = api) %>%
    gh_page(token = token, ...)
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
  token    = gh_token(),
  api      = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.flag(extended))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  branches <- gh_url("repos", repo, "branches", api = api) %>%
    gh_page(token = token, ...) %>%
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
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "commits", ref, api = api) %>%
    gh_page(token = token, ...)
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
  token    = gh_token(),
  api      = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.flag(extended))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  commits <- gh_url("repos", repo, "commits", sha = ref, api = api) %>%
    gh_page(token = token, ...) %>%
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


#  FUNCTION: gh_compare_commits ---------------------------------------------------------------
#' Compare two commits
#'
#' \url{https://developer.github.com/v3/repos/commits/#compare-two-commits}
#'
#' @param base (string) The base branch name.
#' @param head (string) The branch to compare to the base.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A list describing the commits and the differences (see GitHub's API documentation for
#'   details).
#' @export
gh_compare_commits <- function(
  base,
  head,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(base))
  assert_that(is.string(head))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "compare", str_c(base, "...", head), api = api) %>%
    gh_page(token = token, ...) %>%
    .[["commits"]] %>%
    map("commit") %>%
    map(flatten_) %>%
    bind_rows %>%
    mutate(sha = basename(url), date = parse_datetime(author_date)) %>%
    select(sha, date, everything(), -author_date, -tree_sha, -tree_url, -comment_count, -committer_date)
}

#  FUNCTION: gh_compare_files -----------------------------------------------------------------
#' Compare the files of two commits
#'
#' \url{https://developer.github.com/v3/repos/commits/#compare-two-commits}
#'
#' @param base (string) The base branch name.
#' @param head (string) The branch to compare to the base.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation.
#'   Default: value stored in the environment variable \code{"GITHUB_TOKEN"} or
#'   \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A list describing the files and the differences (see GitHub's API documentation
#'   for details).
#' @export
gh_compare_files <- function(
  base,
  head,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(base))
  assert_that(is.string(head))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "compare", str_c(base, "...", head), api = api) %>%
    gh_page(token = token, ...) %>%
    .[["files"]] %>%
    bind_rows %>%
    select(filename, status, additions, deletions, changes, contents_url)
}

#  FUNCTION: gh_contents ----------------------------------------------------------------------
#' Get contents of a file.
#'
#' url{https://developer.github.com/v3/repos/contents/#get-contents}
#'
#' Note: This function can only get the contents of files less than 1MB. For larger files use
#' \code{gh_git_blob}.
#'
#' @param path (string) The path to the file in the repository
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return A string containing the contents of the specified file.
#' @export
gh_contents <- function(
  path,
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(path))
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "contents", path, ref = ref, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_download ----------------------------------------------------------------------
#' Download the entire tree from a commit.
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param path (string) The path to save to.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh}}.
#' @return Full tree of files in specified location.
#' @export
gh_download <- function(
  ref,
  repo,
  path,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!file.exists(path)) dir.create(path)
  archive_path <- file.path(path, str_c(str_replace(repo, "/", "-"), "-", ref, ".zip"))
  on.exit(unlink(archive_path), add = TRUE)

  gh_url("repos", repo, "zipball", ref, api = api) %>%
    gh_get(token = token, binary = TRUE, ...) %>%
    writeBin(archive_path)

  unzip(archive_path, exdir = path)

  # Tidy up by moving files up a directory level
  archive_folder <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  on.exit(unlink(archive_folder, recursive = TRUE), add = TRUE)

  subfolders <- list.dirs(archive_folder, recursive = TRUE, full.names = FALSE)
  map(file.path(path, subfolders[subfolders != ""]), dir.create)

  files <- list.files(archive_folder, recursive = TRUE)
  file.rename(file.path(archive_folder, files), file.path(path, files))

  invisible(path)
}
