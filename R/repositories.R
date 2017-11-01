#  FUNCTION: gh_repository --------------------------------------------------------------------
#' Get information about a repository.
#'
#' \url{https://developer.github.com/v3/repos/#get}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the repository (see GitHub's API documentation for details).
#' @export
gh_repository <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: is_repository --------------------------------------------------------------------
#' Check whether the input is a valid repository
#'
#' \url{https://developer.github.com/v3/repos/#get}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A boolean, with attributes describing the errors, if there are any.
#' @export
is_repository <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  if (!is.string(repo) || !identical(str_count(repo, "/"), 1L)) {
    return(
      FALSE %>%
        set_attrs(error = str_c("Specified 'repo', '", repo, "', is not a string in the format 'owner/repo'")))
  }

  result <- try(
    gh_repository(repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    if (is(attr(result, "condition"), "http_404")) {
      return(
        FALSE %>%
          set_attrs(error = str_c("Specified 'repo', '", repo, "', does not exist in GitHub")))
    } else {
      stop(result)
    }
  }

  TRUE
}

#  FUNCTION: gh_repositories ------------------------------------------------------------------
#' Get information about a user's, organisation's or team's repositories.
#'
#' \url{https://developer.github.com/v3/repos/#list-user-repositories}
#' \url{https://developer.github.com/v3/repos/#list-organization-repositories}
#' \url{https://developer.github.com/v3/orgs/teams/#list-team-repos}
#'
#' Note: Must specify either \code{owner} or \code{team}, but not both.
#'
#' @param owner (string) The user or organisation owning the repository.
#' @param team (string) The team owning the repository.
#' @param type (string, optional) Can be one of \code{"all"}, \code{"owner"}, \code{"member"}.
#'   Default: \code{"owner"}.
#' @param sort (string, optional) Can be one of \code{"created"}, \code{"updated"},
#'   \code{"pushed"}, \code{"full_name"}. Default: \code{"full_name"}.
#' @param direction (string, optional) Can be one of \code{"asc"} or \code{"desc"}. Default:
#'   when using \code{"full_name"}: \code{"asc"}, otherwise \code{"desc"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the repositories a user or organisation has (see GitHub's
#'   API documentation for more detail).
#' @export
gh_repositories <- function(
  owner,
  team,
  type      = NULL,
  sort      = NULL,
  direction = NULL,
  n_max     = 1000L,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.null(type) || is.string(type) && type %in% c("owner", "member"))
  assert_that(is.null(sort) || is.string(sort) && sort %in% c("created", "updated", "pushed", "full_name"))
  assert_that(is.null(direction) || is.string(direction) && direction %in% c("asc", "desc"))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (!missing(owner) && !missing(team))
    stop("Must specify either owner or team, not both!")

  if (!missing(owner)) {
    assert_that(is.string(owner) && identical(str_count(owner, "/"), 0L))

    repos <- try(silent = TRUE, suppressMessages({
      gh_url("orgs", owner, "repos", type = type, sort = sort, direction = direction, api = api) %>%
        gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
    }))

    if (is(repos, "try-error")) {
      repos <- gh_url("users", owner, "repos", type = type, sort = sort, direction = direction, api = api) %>%
        gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
    }
  } else if (!missing(team)) {
    assert_that(is.string(team))
    repos <- gh_url("teams", team, "repos", api = api) %>%
      gh_page(simplify = TRUE, n_max = n_max, token = token, ...)
  }

  repos %>%
    select_safe(
      name, owner_login, owner_type, description, html_url, url,
      created_at, updated_at, size, open_issues, default_branch) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_tags --------------------------------------------------------------------------
#' Get information about all the tags in a repository.
#'
#' \url{https://developer.github.com/v3/repos/#list-tags}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the tags (see GitHub's API documentation for details).
#' @export
gh_tags <- function(
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

  gh_url("repos", repo, "tags", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(name, commit_sha, commit_url, zipball_url, tarball_url)
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
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
    gh_get(token = token, ...)
}

#  FUNCTION: is_branch ------------------------------------------------------------------------
#' Checks whether the input is a valid branch.
#'
#' \url{https://developer.github.com/v3/repos/branches/#get-branch}
#'
#' @param branch (string) The branch name.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A boolean, with attributes describing the errors, if there are any.
#' @export
is_branch <- function(
  branch,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  if (!is.string(branch)) {
    return(
      FALSE %>%
        set_attrs(error = str_c("Specified 'branch', '", branch, "', is not a string")))
  }

  result <- try(
    gh_branch(branch = branch, repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is (result, "try-error")) {
    if (is(attr(result, "condition"), "http_404")) {
      return(
        FALSE %>%
          set_attrs(error = str_c("Specified 'branch', '", branch, "', does not exist in the 'repo' '", repo, "'")))
    } else {
      stop(result)
    }
  }

  TRUE
}

#  FUNCTION: gh_branches ----------------------------------------------------------------------
#' Get information about all the head commits in each branch.
#'
#' \url{https://developer.github.com/v3/repos/branches/#list-branches}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#' @export
gh_branches <- function(
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

  gh_url("repos", repo, "branches", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(name, commit_sha, commit_url)
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the commit (see GitHub's API documentation for details).
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
    gh_get(token = token, ...)
}

#  FUNCTION: is_sha ---------------------------------------------------------------------------
#' Check whether the input is a valid SHA-1
#'
#' \url{https://developer.github.com/v3/repos/commits/#get-a-single-commit}
#'
#' @param sha (string) A commit SHA-1.
#' @param repo (string, optional) The repository specified in the format: \code{"owner/repo"}.
#'   If not supplied existence of the commit is not checked, only the format of the string.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A boolean, with attributes describing the errors, if there are any.
#' @export
is_sha <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  if (!is.string(sha) || !identical(str_length(sha), 40L) || !identical(str_replace(sha, "[0-9a-fA-F]+", ""), "")) {
    return(
      FALSE %>%
        set_attrs(error = str_c("Specified 'sha', '", sha, "', is not a valid 40 character string")))
  }

  result <- try(
    gh_commit(ref = sha, repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    if (is(attr(result, "condition"), "http_404")) {
      return(
        FALSE %>%
          set_attrs(error = str_c("Specified 'sha', '", sha, "', does not exist in the supplied repository, '", repo, "'")))
    } else {
      stop(result)
    }
  }

  TRUE
}

#  FUNCTION: gh_commit_sha --------------------------------------------------------------------
#' Get the SHA-1 of a commit reference
#'
#' \url{https://developer.github.com/v3/repos/commits/#get-the-sha-1-of-a-commit-reference}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the branch (see GitHub's API documentation for details).
#' @export
gh_commit_sha <- function(
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
    gh_get(accept = "application/vnd.github.VERSION.sha", token = token, ...)
}

#  FUNCTION: gh_commits -----------------------------------------------------------------------
#' Get information about all the history of commits.
#'
#' \url{https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#' @export
gh_commits <- function(
  ref,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(ref))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "commits", sha = ref, api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select(starts_with("commit_")) %>%
    set_names(str_replace(names(.), "^commit_", "")) %>%
    select_safe(
      message, url, author_name, author_email, committer_name,
      committer_email, tree_sha, tree_url) %>%
    mutate(sha = basename(url), date = parse_datetime(author_date)) %>%
    select(sha, date, everything())
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the commits and the differences (see GitHub's API documentation for
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
    gh_get(sub_list = "commits", simplify = TRUE, token = token, ...) %>%
    select(starts_with("commit_")) %>%
    set_names(str_replace(names(.), "^commit_", "")) %>%
    select_safe(
      message, url, author_name,author_email,
      committer_name, committer_email) %>%
    mutate(sha = basename(url), date = parse_datetime(author_date)) %>%
    select(sha, date, everything())
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the files and the differences (see GitHub's API documentation
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
    gh_get(sub_list = "files", simplify = TRUE, token = token, ...) %>%
    select_safe(filename, status, additions, deletions, changes, contents_url)
}

#  FUNCTION: gh_readme ------------------------------------------------------------------------
#' Get the README
#'
#' url{https://developer.github.com/v3/repos/contents/#get-the-readme}
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A string containing the contents of the README.
#' @export
gh_readme <- function(
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

  gh_url("repos", repo, "readme", ref = ref, api = api) %>%
    gh_get(accept = "raw", token = token, ...)
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
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
    gh_get(accept = "raw", token = token, ...)
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
#' @param ... Parameters passed to \code{\link{gh_get}}.
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
  on.exit(unlink(archive_path, recursive = TRUE), add = TRUE)

  gh_url("repos", repo, "zipball", ref, api = api) %>%
    gh_get(binary = TRUE, token = token, ...) %>%
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

#  FUNCTION: is_collaborator ------------------------------------------------------------------
#' Check if a user is a collaborator
#'
#' url{https://developer.github.com/v3/repos/collaborators/#check-if-a-user-is-a-collaborator}
#'
#' @param user (string) The GitHub username of the user.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return TRUE if the user is a collaborator, FALSE otherwise (see GitHub's API
#'   documentation for details).
#' @export
is_collaborator <- function(
  user,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_url("repos", repo, "collaborators", user, api = api) %>%
      gh_get(accept = "raw", token = token, ...)
  }))

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_collaborators -----------------------------------------------------------------
#' List collaborators
#'
#' url{https://developer.github.com/v3/repos/collaborators/#list-collaborators}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param affiliation (string) Filter collaborators returned by their affiliation.
#'   Can be one of:
#'   \itemize{
#'     \item outside: All outside collaborators of an organization-owned repository.
#'     \item direct: All collaborators with permissions to an organization-owned repository,
#'       regardless of organization membership status.
#'     \item all: All collaborators the authenticated user can see.
#'     \item Default: all.
#'   }
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the collaborators (see GitHub's API documentation for details).
#' @export
gh_collaborators <- function(
  repo,
  affiliation = NULL,
  n_max       = 1000L,
  token       = gh_token(),
  api         = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "collaborators", affiliation = affiliation, api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(id, login, type, site_admin, permissions_admin, permissions_push, permissions_pull, url)
}

#  FUNCTION: gh_permissions -------------------------------------------------------------------
#' Review a user's permission level
#'
#' url{https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level}
#'
#' @param user (string) The GitHub username of the user.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the user's permissions (see GitHub's API documentation for
#'   details).
#' @export
gh_permissions <- function(
  user,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(user))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "collaborators", user, "permission", api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_commit_comment ----------------------------------------------------------------
#' Get a single commit comment
#'
#' url{https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment}
#'
#' @param comment (integer) The ID assigned to the comment in GitHub.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the commit comment (see GitHub's API documentation for details).
#' @export
gh_commit_comment <- function(
  comment,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(comment))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "comments", comment, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_commit_comments ---------------------------------------------------------------
#' List comments for a single commit or entire repository
#'
#' url{https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit}
#' url{https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing all the commit comments (see GitHub's API documentation for details).
#' @export
gh_commit_comments <- function(
  repo,
  ref,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(ref)) {
    url <- gh_url("repos", repo, "comments", api = api)
  } else {
    assert_that(is.string(ref))
    url <- gh_url("repos", repo, "commits", ref, "comments", api = api)
  }

  url %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(id, commit_id, body, user_login, created_at, updated_at, position, line, path, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}

#  FUNCTION: gh_contributers ------------------------------------------------------------------
#' List contributors
#'
#' url{https://developer.github.com/v3/repos/#list-contributors}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param anon (boolean) Set to TRUE to include anonymous contributors in results.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the contributers (see GitHub's API documentation for details).
#' @export
gh_contributers <- function(
  repo,
  anon  = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.null(anon) || is.flag(anon))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "contributors", anon = as.integer(anon), api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(id, login, contributions, type, site_admin, url)
}

#  FUNCTION: gh_languages ---------------------------------------------------------------------
#' List languages
#'
#' url{https://developer.github.com/v3/repos/#list-languages}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the number of bytes written in each language (see GitHub's API
#'   documentation for details).
#' @export
gh_languages <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "languages", api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_release -----------------------------------------------------------------------
#' Get a single release
#'
#' url{https://developer.github.com/v3/repos/releases/#get-a-single-release}
#' url{https://developer.github.com/v3/repos/releases/#get-the-latest-release}
#' url{https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name}
#'
#' @param tag (string, optional) The tag associated with a release. If not supplied the latest
#'   release is returned.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the release (see GitHub's API documentation for details).
#' @export
gh_release <- function(
  tag,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(tag)) {
    url <- gh_url("repos", repo, "releases/latest", api = api)
  } else {
    assert_that(is.string(tag) || is.count(tag))
    if (is.string(tag)) {
      url <- gh_url("repos", repo, "releases/tags", tag, api = api)
    } else {
      url <- gh_url("repos", repo, "releases", tag, api = api)
    }
  }

  url %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_releases ----------------------------------------------------------------------
#' List releases for a repository
#'
#' url{https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the releases (see GitHub's API documentation for details).
#' @export
gh_releases <- function(
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

  gh_url("repos", repo, "releases", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(assets = collapse_list(assets, "name")) %>%
    select_safe(
      id, tag_name, name, body, author_login, draft, prerelease, target_commitish,
      created_at, published_at, assets, zipball_url, url) %>%
    mutate(created_at = parse_datetime(created_at), published_at = parse_datetime(published_at))
}

#  FUNCTION: gh_asset -------------------------------------------------------------------------
#' Get a single release asset
#'
#' url{https://developer.github.com/v3/repos/releases/#get-a-single-release-asset}
#'
#' @param asset (integer) The ID assigned to the asset by GitHub
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the release asset (see GitHub's API documentation for details).
#' @export
gh_asset <- function(
  asset,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(asset))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  # GET /repos/:owner/:repo/releases/assets/:id
  gh_url("repos", repo, "releases/assets", asset, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_assets ------------------------------------------------------------------------
#' List assets for a release
#'
#' url{https://developer.github.com/v3/repos/releases/#list-assets-for-a-release}
#'
#' @param release (integer) The ID assigned to the release by GitHub.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the assets for a release (see GitHub's API documentation for details).
#' @export
gh_assets <- function(
  release,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(release))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "releases", release, "assets", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    select_safe(
      id, name, label, content_type, state, size, download_count,
      created_at, updated_at, uploader_login, url) %>%
    mutate(created_at = parse_datetime(created_at), updated_at = parse_datetime(updated_at))
}
