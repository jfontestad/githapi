#  FUNCTION: view_commits ---------------------------------------------------------------------
#
#' Get information about the history of commits
#'
#' This function returns information about all the commits in the history of a specified
#' commit. The commit can be specifed by a branch, tag or SHA.
#'
#' <https://developer.github.com/v3/repos/commits/#list-commits-on-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used. If `NA` the default branch is used, Default: `NA`.
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
view_commits <- function(
  repo,
  ref   = NA,
  n_max = 1000L,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_na(ref) || is_string(ref))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  info("Getting up to ", n_max, " commits from repository '", repo, "'")
  commits_list <- gh_page(
    gh_url("repos", repo, "commits", sha = ref, api = api),
    n_max = n_max, token = token, ...)

  info("Transforming results")
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
    mutate(parent_sha = sapply(commits_list, function(cm) {
      sapply(cm$parents, getElement, "sha")
    })) %>%
    mutate(parent_url = sapply(commits_list, function(cm) {
      sapply(cm$parents, getElement, "url")
    }))

  info("Done")
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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param refs (character) Git references: either tags or branches. If a branch is specified
#'   the SHA of the head commit is returned.
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
  repo,
  refs,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(refs))
  assert(is_sha(token))
  assert(is_url(api))

  shas <- sapply(refs, simplify = TRUE, USE.NAMES = TRUE, function(ref) {
    info("Getting SHA for ref '", ref, "' from repository '", repo, "'")
    tryCatch({
      sha <- gh_request(
        "GET", gh_url("repos", repo, "commits", ref, api = api),
        accept = "application/vnd.github.VERSION.sha", token = token, ...)

      attr(sha, "header") <- NULL
      sha
    }, error = function(e) {
      info("Ref '", ref, "' failed!")
      e
    })
  })

  info("Done")
  shas
}

#  FUNCTION: compare_commits ------------------------------------------------------------------
#
#' Compare two commits
#'
#' This function returns information about all the commits made between the two specified
#' commits. The commits can be specifed by any git reference, i.e. branch, tag or SHA. Note:
#' This function will only return a maximum of 250 commits. If more are required, use
#' [view_commits()].
#'
#' <https://developer.github.com/v3/repos/commits/#compare-two-commits>
#'
#' @param repo (string) The repository specified as "owner/repo".
#' @param base (string) The reference of the base commit.
#' @param head (string) The reference of the commit to compare.
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
  repo,
  base,
  head,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_string(base))
  assert(is_string(head))
  assert(is_sha(token))
  assert(is_url(api))

  info("Getting comparion of commits from repository '", repo, "'")
  comparison_list <- gh_request(
    "GET", gh_url("repos", repo, "compare", paste0(base, "...", head), api = api),
    token = token, ...)

  info("Transforming results")
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
    mutate(parent_sha = sapply(comparison_list$commits, function(cm) {
      sapply(cm$parents, getElement, "sha")
    })) %>%
    mutate(parent_url = sapply(comparison_list$commits, function(cm) {
      sapply(cm$parents, getElement, "url")
    }))

  info("Done")
  comparison_tbl
}
