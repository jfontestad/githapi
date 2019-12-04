#  FUNCTION: view_history ---------------------------------------------------------------------
#
#' Get information about the history of commits
#'
#' This function returns information about all the commits in the history of a specified
#' commit. The commit can be specified by a branch, tag or SHA.
#'
#' <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
#'
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used. If not specified the default branch is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the commits, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **sha**: The SHA of the commit.
#'   - **message**: The commit message recorded in GitHub.
#'   - **author_name**: The author of the commit.
#'   - **author_email**: The author's email address.
#'   - **committer_name**: The committer of the commit.
#'   - **committer_email**: The committer's email address.
#'   - **date**: The date and time of the commit.
#'   - **url**: The URL to get the commit details from GitHub.
#'   - **tree_sha**: The SHA of the git tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **parent_sha**: The SHA(s) of the parent commit(s).
#'   - **parent_url**: The URL(s) to get the parent's details from GitHub.
#'
#' @export
#'
view_history <- function(
  ref,
  repo,
  n_max = 1000L,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(repo)) {
      info("'repo' is missing, so using 'ref' argument: ", ref, level = 2)
      repo <- ref
      ref <- NULL
    }
    if (missing(ref) || is_null(ref)) {
      ref <- NULL
    }

    (is_null(ref) || is_scalar_character(ref)) ||
      error("'ref' must be NULL or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_scalar_integerish(n_max) && isTRUE(n_max > 0)) ||
      error("'n_max' must be a positive integer:\n  '", paste(n_max, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  info("Getting up to ", n_max, " commits from repository '", repo, "'")
  commits_list <- try_catch({
    gh_page(
      gh_url("repos", repo, "commits", sha = ref, api = api),
      n_max = n_max, token = token, ...)
  })

  info("Transforming results", level = 3)
  commits_tbl <- bind_fields(commits_list, list(
    sha             = c("sha",                          as = "character"),
    message         = c("commit", "message",            as = "character"),
    author_name     = c("commit", "author", "name",     as = "character"),
    author_email    = c("commit", "author", "email",    as = "character"),
    committer_name  = c("commit", "committer", "name",  as = "character"),
    committer_email = c("commit", "committer", "email", as = "character"),
    date            = c("commit", "author", "date",     as = "datetime"),
    url             = c("commit", "url",                as = "character"),
    tree_sha        = c("commit", "tree", "sha",        as = "character"),
    tree_url        = c("commit", "tree", "url",        as = "character"),
    parent_sha      = "",
    parent_url      = "")) %>%
    mutate(parent_sha = gh_map(commits_list, use_names = FALSE, list_fields, "parents", "sha")) %>%
    mutate(parent_url = gh_map(commits_list, use_names = FALSE, list_fields, "parents", "url"))

  info("Done", level = 3)
  commits_tbl
}

#  FUNCTION: view_shas ------------------------------------------------------------------------
#
#' Look up the SHA-1 of commit references
#'
#' This function looks up the SHA for the commit associated with the specified git references.
#' A reference can be either a branch or a tag (or a SHA, but the function will return the
#' same SHA). If a branch is specified the function returns the SHA of the head commit.
#'
#' <https://developer.github.com/v3/repos/commits/#get-the-sha-1-of-a-commit-reference>
#'
#' @param refs (character) Git references: either tags or branches. If a branch is specified
#'   the SHA of the head commit is returned.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named character vector with the supplied refs as names and the SHA as the value.
#'
#' @export
#'
view_shas <- function(
  refs,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(refs)) ||
      error("'refs' must be a character vector:\n  '", paste(refs, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  shas <- try_map(refs, simplify = TRUE, function(ref) {
    info("Getting SHA for ref '", ref, "' from repository '", repo, "'")

    sha <- gh_request(
      "GET", url = gh_url("repos", repo, "commits", ref, api = api),
      accept = "application/vnd.github.VERSION.sha", token = token, ...)

    attr(sha, "header") <- NULL
    sha
  })

  info("Done", level = 3)
  shas
}

#  FUNCTION: shas_exist -----------------------------------------------------------------------
#
#' Determine whether commit SHAs exist in the specified repository.
#'
#' This function returns `TRUE` if the commit SHA exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/repos/commits/#get-the-sha-1-of-a-commit-reference>
#'
#' @param shas (character) The SHAs of the commits to check.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each SHA specified.
#'
#' @export
#'
shas_exist <- function(
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(shas) && all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  gh_map(shas, simplify = TRUE, function(sha) {
    info("Checking commit SHA '", sha, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "commits", sha, api = api),
        token = token, ...)
      TRUE
    }, on_error = function(e) {
      FALSE
    })
  })
}

#  FUNCTION: compare_commits ------------------------------------------------------------------
#
#' Compare two commits
#'
#' This function returns information about all the commits made between the two specified
#' commits. The commits can be specified by any git reference, i.e. branch, tag or SHA. Note:
#' This function will only return a maximum of 250 commits. If more are required, use
#' [view_commits()].
#'
#' <https://developer.github.com/v3/repos/commits/#compare-two-commits>
#'
#' @param base (string) The reference of the base commit.
#' @param head (string) The reference of the commit to compare.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the commits, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **sha**: The SHA of the commit.
#'   - **message**: The commit message recorded in GitHub.
#'   - **author_name**: The author of the commit.
#'   - **author_email**: The author's email address.
#'   - **committer_name**: The committer of the commit.
#'   - **committer_email**: The committer's email address.
#'   - **date**: The date and time of the commit.
#'   - **url**: The URL to get the commit details from GitHub.
#'   - **tree_sha**: The SHA of the git tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **parent_sha**: The SHA(s) of the parent commit(s).
#'   - **parent_url**: The URL(s) to get the parent's details from GitHub.
#'
#' @export
#'
compare_commits <- function(
  base,
  head,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_scalar_character(base)) ||
      error("'base' must be a string:\n  '", paste(base, collapse = "'\n  '"), "'")
    (is_scalar_character(head)) ||
      error("'head' must be a string:\n  '", paste(head, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  info("Getting comparison of '", head, "' with '", base, "' from repository '", repo, "'")
  comparison_list <- try_catch({
    gh_request(
      "GET", url = gh_url("repos", repo, "compare", paste0(base, "...", head), api = api),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  comparison_tbl <- bind_fields(comparison_list$commits, list(
    sha             = c("sha",                          as = "character"),
    message         = c("commit", "message",            as = "character"),
    author_name     = c("commit", "author", "name",     as = "character"),
    author_email    = c("commit", "author", "email",    as = "character"),
    committer_name  = c("commit", "committer", "name",  as = "character"),
    committer_email = c("commit", "committer", "email", as = "character"),
    date            = c("commit", "author", "date",     as = "datetime"),
    url             = c("commit", "url",                as = "character"),
    tree_sha        = c("commit", "tree", "sha",        as = "character"),
    tree_url        = c("commit", "tree", "url",        as = "character"),
    parent_sha      = "",
    parent_url      = "")) %>%
    mutate(parent_sha = gh_map(comparison_list$commits, use_names = FALSE, list_fields, "parents", "sha")) %>%
    mutate(parent_url = gh_map(comparison_list$commits, use_names = FALSE, list_fields, "parents", "url"))

  info("Done", level = 3)
  comparison_tbl
}

#  FUNCTION: compare_files --------------------------------------------------------------------
#
#' Compare the files of two commits
#'
#' This function returns information on all the files that have been changed between two
#' commits. The commits can be specified by any git reference, i.e. branch, tag or SHA.
#'
#' <https://developer.github.com/v3/repos/commits/#compare-two-commits>
#'
#' @param base (string) The reference of the base commit.
#' @param head (string) The reference of the commit to compare.
#' @param repo (string) The repository specified as "owner/repo".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the commits, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **filename**: The name of the file.
#'   - **sha**: The SHA of the file blob.
#'   - **status**: Whether the file was "added", "deleted" or "changed".
#'   - **additions**: The number of lines added.
#'   - **deletions**: The number of lines deleted.
#'   - **changes**: The number of lines changed.
#'   - **raw_url**: The URL to the raw file.
#'   - **contents_url**: The URL to retrieve the contents
#'   - **patch**: The patch to apply to change the file.
#'
#' @export
#'
compare_files <- function(
  base,
  head,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_scalar_character(base)) ||
      error("'base' must be a string:\n  '", paste(base, collapse = "'\n  '"), "'")
    (is_scalar_character(head)) ||
      error("'head' must be a string:\n  '", paste(head, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  info("Getting comparison of '", head, "' with '", base, "' from repository '", repo, "'")
  comparison_list <- try_catch({
    gh_request(
      "GET", url = gh_url("repos", repo, "compare", paste0(base, "...", head), api = api),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  comparison_tbl <- bind_fields(comparison_list$files, list(
    filename     = c("filename",     as = "character"),
    sha          = c("sha",          as = "character"),
    status       = c("status",       as = "character"),
    additions    = c("additions",    as = "integer"),
    deletions    = c("deletions",    as = "integer"),
    changes      = c("changes",      as = "integer"),
    raw_url      = c("raw_url",      as = "character"),
    contents_url = c("contents_url", as = "character"),
    patch        = c("patch",        as = "character")))

  info("Done", level = 3)
  comparison_tbl
}
