#  FUNCTION: gh_repository --------------------------------------------------------------------
#
#' Get information about a repository
#'
#' <https://developer.github.com/v3/repos/#get>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the repository (see GitHub's API documentation for details).
#'
#' @export
#'
gh_repository <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, api = api),
    token = token, ...)
}

#  FUNCTION: is_repository --------------------------------------------------------------------
#
#' Check whether the input is a valid repository
#'
#' <https://developer.github.com/v3/repos/#get>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE or FALSE.
#'
#' @export
#'
is_repository <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  if (!is_repo(repo)) {
    return(FALSE)
  }

  result <- try(
    gh_repository(repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    return(FALSE)
  }

  TRUE
}

#  FUNCTION: gh_repositories ------------------------------------------------------------------
#
#' Get information about a user's, organisation's or team's repositories.
#'
#' <https://developer.github.com/v3/repos/#list-user-repositories>
#' <https://developer.github.com/v3/repos/#list-organization-repositories>
#' <https://developer.github.com/v3/orgs/teams/#list-team-repos>
#'
#' Note: Must specify either `owner` or `team`, but not both.
#'
#' @param owner (string) The user or organisation owning the repository.
#' @param team (string) The team owning the repository.
#' @param type (string, optional) Can be one of `all`, `owner`, `member`.
#'   Default: `owner`.
#' @param sort (string, optional) Can be one of `created`, `updated`, `pushed`, `full_name`.
#'   Default: `full_name`.
#' @param direction (string, optional) Can be one of `asc` or `desc`. Default: when using
#'   `full_name`: `asc`, otherwise `desc`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the repositories a user or organisation has (see GitHub's
#'   API documentation for more detail).
#'
#' @export
#'
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
  assert(is_null(type) || is_string(type), type %in% c("owner", "member"))
  assert(is_null(sort) || is_string(sort), sort %in% c("created", "updated", "pushed", "full_name"))
  assert(is_null(direction) || is_string(direction), direction %in% c("asc", "desc"))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  if (!missing(owner) && !missing(team))
    error("Must specify either owner or team, not both!")

  if (!missing(owner)) {
    assert(is_string(owner))

    repos <- try(silent = TRUE, suppressMessages({
      gh_page(
        gh_url("orgs", owner, "repos", type = type, sort = sort, direction = direction, api = api),
        n_max = n_max, token = token, ...)
    }))

    if (is(repos, "try-error")) {
      repos <- gh_page(
        gh_url("users", owner, "repos", type = type, sort = sort, direction = direction, api = api),
        n_max = n_max, token = token, ...)
    }
  } else if (!missing(team)) {
    assert(is_string(team))

    repos <- gh_page(
      gh_url("teams", team, "repos", api = api),
      n_max = n_max, token = token, ...)
  }

  bind_fields(repos, list(
    name           = c("name",           as = "character"),
    description    = c("description",    as = "character"),
    owner_login    = c("owner", "login", as = "character"),
    owner_type     = c("owner", "type",  as = "character"),
    default_branch = c("default_branch", as = "character"),
    open_issues    = c("open_issues",    as = "integer"),
    size           = c("size",           as = "integer"),
    url            = c("url",            as = "character"),
    html_url       = c("html_url",       as = "character"),
    created_at     = c("created_at",     as = "datetime"),
    updated_at     = c("updated_at",     as = "datetime")))
}

#  FUNCTION: gh_tags --------------------------------------------------------------------------
#
#' Get information about all the tags in a repository
#'
#' NOTE: This function has been deprecated, please use [view_tags()] instead.
#'
#' <https://developer.github.com/v3/repos/#list-tags>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the tags (see GitHub's API documentation for details).
#'
#' @export
#'
gh_tags <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_tags", package = "githapi")

  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  tags <- gh_page(
    gh_url("repos", repo, "tags", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(tags, list(
    name        = c("name",          as = "character"),
    commit_sha  = c("commit", "sha", as = "character"),
    commit_url  = c("commit", "url", as = "character"),
    zipball_url = c("zipball_url",   as = "character"),
    tarball_url = c("tarball_url",   as = "character")))
}

#  FUNCTION: gh_branch ------------------------------------------------------------------------
#
#' Get information about a branch
#'
#' NOTE: This function has been deprecated, please use [view_branches()] instead.
#'
#' <https://developer.github.com/v3/repos/branches/#get-branch>
#'
#' @param branch (string) The branch name.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the branch (see GitHub's API documentation for details).
#'
#' @export
#'
gh_branch <- function(
  branch,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_branches", package = "githapi")

  assert(is_string(branch))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "branches", branch, api = api),
    token = token, ...)
}

#  FUNCTION: is_branch ------------------------------------------------------------------------
#
#' Checks whether the input is a valid branch
#'
#' NOTE: This function has been deprecated, please use [branches_exist()] instead.
#'
#' <https://developer.github.com/v3/repos/branches/#get-branch>
#'
#' @param branch (string) The branch name.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE or FALSE.
#'
#' @export
#'
is_branch <- function(
  branch,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("branches_exist", package = "githapi")

  if (!is_string(branch)) {
    return(FALSE)
  }

  result <- try(
    gh_branch(branch = branch, repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    return(FALSE)
  }

  TRUE
}

#  FUNCTION: gh_branches ----------------------------------------------------------------------
#
#' Get information about all the head commits in each branch
#'
#' NOTE: This function has been deprecated, please use [view_branches()] instead.
#'
#' <https://developer.github.com/v3/repos/branches/#list-branches>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#'
#' @export
#'
gh_branches <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_branches", package = "githapi")

  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  branches <- gh_page(
    gh_url("repos", repo, "branches", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(branches, list(
    name       = c("name",          as = "character"),
    commit_sha = c("commit", "sha", as = "character"),
    commit_url = c("commit", "url", as = "character")))
}

#  FUNCTION: gh_commit ------------------------------------------------------------------------
#
#' Get information about a commit
#'
#' NOTE: This function has been deprecated, please use [view_commits()] instead.
#'
#' <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
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
gh_commit <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_commits", package = "githapi")

  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "commits", ref, api = api),
    token = token, ...)
}

#  FUNCTION: is_valid_sha ---------------------------------------------------------------------
#
#' Check whether the input is a valid SHA-1
#'
#' NOTE: This function has been deprecated, please use [shas_exist()] instead.
#'
#' <https://developer.github.com/v3/repos/commits/#get-a-single-commit>
#'
#' @param sha (string) A commit SHA-1.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#'   If not supplied existence of the commit is not checked, only the format of the string.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE or FALSE.
#'
#' @export
#'
is_valid_sha <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("shas_exist", package = "githapi")

  if (!is_sha(sha)) {
    return(FALSE)
  }

  result <- try(
    gh_commit(ref = sha, repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    return(FALSE)
  }

  TRUE
}

#  FUNCTION: gh_commit_sha --------------------------------------------------------------------
#
#' Get the SHA-1 of a commit reference
#'
#' NOTE: This function has been deprecated, please use [view_shas()] instead.
#'
#' <https://developer.github.com/v3/repos/commits/#get-the-sha-1-of-a-commit-reference>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the branch (see GitHub's API documentation for details).
#'
#' @export
#'
gh_commit_sha <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("view_shas", package = "githapi")

  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  sha <- gh_get(
    gh_url("repos", repo, "commits", ref, api = api),
    accept = "application/vnd.github.VERSION.sha", token = token, ...)

  attr(sha, "header") <- NULL
  sha
}

#  FUNCTION: gh_commits -----------------------------------------------------------------------
#
#' Get information about all the history of commits
#'
#' <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the branches (see GitHub's API documentation for details).
#'
#' @export
#'
gh_commits <- function(
  ref,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  commits <- gh_page(
    gh_url("repos", repo, "commits", sha = ref, api = api),
    n_max = n_max, token = token, ...)

  bind_fields(commits, list(
    sha             = c("sha",                          as = "character"),
    message         = c("commit", "message",            as = "character"),
    author_name     = c("commit", "author", "name",     as = "character"),
    author_email    = c("commit", "author", "email",    as = "character"),
    committer_name  = c("commit", "committer", "name",  as = "character"),
    committer_email = c("commit", "committer", "email", as = "character"),
    date            = c("commit", "author", "date",     as = "datetime"),
    url             = c("commit", "url",                as = "character"),
    tree_sha        = c("commit", "tree", "sha",        as = "character"),
    tree_url        = c("commit", "tree", "url",        as = "character")))
}

#  FUNCTION: gh_compare_commits ---------------------------------------------------------------
#
#' Compare two commits
#'
#' <https://developer.github.com/v3/repos/commits/#compare-two-commits>
#'
#' @param base (string) The base branch name.
#' @param head (string) The branch to compare to the base.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the commits and the differences (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_compare_commits <- function(
  base,
  head,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(base))
  assert(is_string(head))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  comparison <- gh_get(
    gh_url("repos", repo, "compare", paste0(base, "...", head), api = api),
    token = token, ...)

  bind_fields(comparison$commits, list(
    sha             = c("sha",                          as = "character"),
    message         = c("commit", "message",            as = "character"),
    author_name     = c("commit", "author", "name",     as = "character"),
    author_email    = c("commit", "author", "email",    as = "character"),
    committer_name  = c("commit", "committer", "name",  as = "character"),
    committer_email = c("commit", "committer", "email", as = "character"),
    date            = c("commit", "author", "date",     as = "datetime"),
    url             = c("commit", "url",                as = "character"),
    tree_sha        = c("commit", "tree", "sha",        as = "character"),
    tree_url        = c("commit", "tree", "url",        as = "character")))
}

#  FUNCTION: gh_compare_files -----------------------------------------------------------------
#
#' Compare the files of two commits
#'
#' <https://developer.github.com/v3/repos/commits/#compare-two-commits>
#'
#' @param base (string) The base branch name.
#' @param head (string) The branch to compare to the base.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation.
#'   Default: value stored in the environment variable `GITHUB_TOKEN` or
#'   `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the files and the differences (see GitHub's API documentation
#'   for details).
#'
#' @export
#'
gh_compare_files <- function(
  base,
  head,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(base))
  assert(is_string(head))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  comparison <- gh_get(
    gh_url("repos", repo, "compare", paste0(base, "...", head), api = api),
    token = token, ...)

  bind_fields(comparison$files, list(
    filename     = c("filename",     as = "character"),
    status       = c("status",       as = "character"),
    additions    = c("additions",    as = "integer"),
    deletions    = c("deletions",    as = "integer"),
    changes      = c("changes",      as = "integer"),
    contents_url = c("contents_url", as = "character")))
}

#  FUNCTION: gh_readme ------------------------------------------------------------------------
#
#' Get the README
#'
#' <https://developer.github.com/v3/repos/contents/#get-the-readme>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A string containing the contents of the README.
#'
#' @export
#'
gh_readme <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  readme <- gh_get(
    gh_url("repos", repo, "readme", ref = ref, api = api),
    accept = "raw", token = token, ...)

  attr(readme, "header") <- NULL
  readme
}

#  FUNCTION: gh_contents ----------------------------------------------------------------------
#
#' Get contents of a file
#'
#' <https://developer.github.com/v3/repos/contents/#get-contents>
#'
#' Note: This function can only get the contents of files less than 1MB. For larger files use
#' [gh_git_blob()].
#'
#' @param path (string) The path to the file in the repository
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A string containing the contents of the specified file.
#'
#' @export
#'
gh_contents <- function(
  path,
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(path))
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  content <- gh_get(
    gh_url("repos", repo, "contents", path, ref = ref, api = api),
    accept = "raw", token = token, ...)

  attr(content, "header") <- NULL
  content
}

#  FUNCTION: gh_download ----------------------------------------------------------------------
#
#' Download the entire tree from a commit
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string) The path to save to.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_download_binary()].
#'
#' @return Full tree of files in specified location.
#'
#' @export
#'
gh_download <- function(
  ref,
  repo,
  path,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_string(path))
  assert(is_sha(token))
  assert(is_url(api))

  if (!file.exists(path)) dir.create(path, recursive = TRUE)

  archive_path <- file.path(path, paste0(sub("/", "-", repo), "-", ref, ".zip"))
  on.exit(unlink(archive_path, recursive = TRUE), add = TRUE)

  gh_download_binary(
    gh_url("repos", repo, "zipball", ref, api = api),
    path = archive_path, token = token, ...)

  unzip(archive_path, exdir = path)

  # Tidy up by moving files up a directory level
  archive_folder <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  on.exit(unlink(archive_folder, recursive = TRUE), add = TRUE)

  subfolders <- list.dirs(archive_folder, recursive = TRUE, full.names = FALSE)
  lapply(file.path(path, subfolders[subfolders != ""]), dir.create)

  files <- list.files(archive_folder, recursive = TRUE)
  file.rename(file.path(archive_folder, files), file.path(path, files))

  invisible(path)
}

#  FUNCTION: is_collaborator ------------------------------------------------------------------
#
#' Check if a user is a collaborator
#'
#' <https://developer.github.com/v3/repos/collaborators/#check-if-a-user-is-a-collaborator>
#'
#' @param user (string) The GitHub username of the user.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE if the user is a collaborator, FALSE otherwise (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
is_collaborator <- function(
  user,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(user))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(
      gh_url("repos", repo, "collaborators", user, api = api),
      accept = "raw", token = token, ...)
  }))

  attributes(response) <- NULL
  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_collaborators -----------------------------------------------------------------
#
#' List collaborators
#'
#' <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param affiliation (string) Filter collaborators returned by their affiliation.
#'   Can be one of:
#'   - `outside`: All outside collaborators of an organization-owned repository.
#'   - `direct`: All collaborators with permissions to an organization-owned repository,
#'       regardless of organization membership status.
#'   - `all`: All collaborators the authenticated user can see.
#'   - `Default`: all.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the collaborators (see GitHub's API documentation for details).
#'
#' @export
#'
gh_collaborators <- function(
  repo,
  affiliation = NULL,
  n_max       = 1000L,
  token       = gh_token(),
  api         = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  collaborators <- gh_page(
    gh_url("repos", repo, "collaborators", affiliation = affiliation, api = api),
    n_max = n_max, token = token, ...)

  bind_fields(collaborators, list(
    id                = c("id",                   as = "integer"),
    login             = c("login",                as = "character"),
    type              = c("type",                 as = "character"),
    site_admin        = c("site_admin",           as = "logical"),
    permissions_admin = c("permissions", "admin", as = "logical"),
    permissions_push  = c("permissions", "push",  as = "logical"),
    permissions_pull  = c("permissions", "pull",  as = "logical"),
    url               = c("url",                  as = "character")))
}

#  FUNCTION: gh_permissions -------------------------------------------------------------------
#
#' Review a user's permission level
#'
#' <https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level>
#'
#' @param user (string) The GitHub username of the user.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the user's permissions (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_permissions <- function(
  user,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(user))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "collaborators", user, "permission", api = api),
    token = token, ...)
}

#  FUNCTION: gh_commit_comment ----------------------------------------------------------------
#
#' Get a single commit comment
#'
#' <https://developer.github.com/v3/repos/comments/#get-a-single-commit-comment>
#'
#' @param comment (integer) The ID assigned to the comment in GitHub.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the commit comment (see GitHub's API documentation for details).
#'
#' @export
#'
gh_commit_comment <- function(
  comment,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_count(comment))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "comments", comment, api = api),
    token = token, ...)
}

#  FUNCTION: gh_commit_comments ---------------------------------------------------------------
#
#' List comments for a single commit or entire repository
#'
#' <https://developer.github.com/v3/repos/comments/#list-comments-for-a-single-commit>
#' <https://developer.github.com/v3/repos/comments/#list-commit-comments-for-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing all the commit comments (see GitHub's API documentation for details).
#'
#' @export
#'
gh_commit_comments <- function(
  repo,
  ref,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(ref)) {
    url <- gh_url("repos", repo, "comments", api = api)
  } else {
    assert(is_string(ref))
    url <- gh_url("repos", repo, "commits", ref, "comments", api = api)
  }

  comments <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(comments, list(
    id         = c("id",            as = "integer"),
    commit_id  = c("commit_id",     as = "character"),
    body       = c("body",          as = "character"),
    user_login = c("user", "login", as = "character"),
    created_at = c("created_at",    as = "datetime"),
    updated_at = c("updated_at",    as = "datetime"),
    position   = c("position",      as = "character"),
    line       = c("line",          as = "character"),
    path       = c("path",          as = "character"),
    url        = c("url",           as = "character")))
}

#  FUNCTION: gh_contributers ------------------------------------------------------------------
#
#' List contributors
#'
#' <https://developer.github.com/v3/repos/#list-contributors>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param anon (boolean) Set to TRUE to include anonymous contributors in results.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the contributers (see GitHub's API documentation for details).
#'
#' @export
#'
gh_contributers <- function(
  repo,
  anon  = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_null(anon) || is_boolean(anon))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  contributers <- gh_page(
    gh_url("repos", repo, "contributors", anon = as.integer(anon), api = api),
    n_max = n_max, token = token, ...)

  bind_fields(contributers, list(
    id            = c("id",            as = "integer"),
    login         = c("login",         as = "character"),
    contributions = c("contributions", as = "integer"),
    type          = c("type",          as = "character"),
    site_admin    = c("site_admin",    as = "logical"),
    url           = c("url",           as = "character")))
}

#  FUNCTION: gh_languages ---------------------------------------------------------------------
#
#' List languages
#'
#' <https://developer.github.com/v3/repos/#list-languages>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the number of bytes written in each language (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
gh_languages <- function(
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "languages", api = api),
    token = token, ...)
}

#  FUNCTION: gh_release -----------------------------------------------------------------------
#
#' Get a single release
#'
#' <https://developer.github.com/v3/repos/releases/#get-a-single-release>
#' <https://developer.github.com/v3/repos/releases/#get-the-latest-release>
#' <https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name>
#'
#' @param tag (string, optional) The tag associated with a release. If not supplied the latest
#'   release is returned.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the release (see GitHub's API documentation for details).
#'
#' @export
#'
gh_release <- function(
  tag,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(tag)) {
    url <- gh_url("repos", repo, "releases/latest", api = api)
  } else {
    assert(is_string(tag) || is_count(tag))
    if (is_string(tag)) {
      url <- gh_url("repos", repo, "releases/tags", tag, api = api)
    } else {
      url <- gh_url("repos", repo, "releases", tag, api = api)
    }
  }

  release <- gh_get(url, token = token, ...)
  attr(release, "header") <- NULL
  release
}

#  FUNCTION: gh_releases ----------------------------------------------------------------------
#
#' List releases for a repository
#'
#' <https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the releases (see GitHub's API documentation for details).
#'
#' @export
#'
gh_releases <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  releases <- gh_page(
    gh_url("repos", repo, "releases", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(releases, list(
    id               = c("id",               as = "integer"),
    tag_name         = c("tag_name",         as = "character"),
    name             = c("name",             as = "character"),
    body             = c("body",             as = "character"),
    author_login     = c("author", "login",  as = "character"),
    draft            = c("draft",            as = "logical"),
    prerelease       = c("prerelease",       as = "logical"),
    target_commitish = c("target_commitish", as = "character"),
    created_at       = c("created_at",       as = "datetime"),
    published_at     = c("published_at",     as = "datetime"),
    assets           = "",
    zipball_url      = c("zipball_url",      as = "character"),
    url              = c("url",              as = "character"))) %>%
    mutate(assets = lapply(releases, function(r) {
      sapply(r$assets, getElement, "name")
    }))
}

#  FUNCTION: gh_asset -------------------------------------------------------------------------
#
#' Get a single release asset
#'
#' <https://developer.github.com/v3/repos/releases/#get-a-single-release-asset>
#'
#' @param asset (integer) The ID assigned to the asset by GitHub
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the release asset (see GitHub's API documentation for details).
#'
#' @export
#'
gh_asset <- function(
  asset,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_count(asset))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "releases/assets", asset, api = api),
    token = token, ...)
}

#  FUNCTION: gh_assets ------------------------------------------------------------------------
#
#' List assets for a release
#'
#' <https://developer.github.com/v3/repos/releases/#list-assets-for-a-release>
#'
#' @param release (integer) The ID assigned to the release by GitHub.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the assets for a release (see GitHub's API documentation for details).
#'
#' @export
#'
gh_assets <- function(
  release,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_count(release))
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  assets <- gh_page(
    gh_url("repos", repo, "releases", release, "assets", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(assets, list(
    id             = c("id",                as = "integer"),
    name           = c("name",              as = "character"),
    label          = c("label",             as = "character"),
    content_type   = c("content_type",      as = "character"),
    state          = c("state",             as = "character"),
    size           = c("size",              as = "integer"),
    download_count = c("download_count",    as = "integer"),
    created_at     = c("created_at",        as = "datetime"),
    updated_at     = c("updated_at",        as = "datetime"),
    uploader_login = c("uploader", "login", as = "character"),
    url            = c("url",               as = "character")))
}
