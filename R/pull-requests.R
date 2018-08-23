#  FUNCTION: gh_pull_request ------------------------------------------------------------------
#
#' Get a single pull request
#'
#' <https://developer.github.com/v3/pulls/#get-a-single-pull-request>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the pull request (see GitHub's API documentation for details).
#'
#' @export
#'
gh_pull_request <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_get(gh_url("repos", repo, "pulls", pull_request, api = api), token = token, ...)
}

#  FUNCTION: gh_pull_requests -----------------------------------------------------------------
#
#' List pull requests
#'
#' <https://developer.github.com/v3/pulls/#list-pull-requests>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param state (string) Either open, closed, or all to filter by state. Default: open
#' @param head (string) Filter pulls by head user and branch name in the format of
#'   `user:ref-name`. Example: `github:new-script-format`.
#' @param base (string) Filter pulls by base branch name. Example: gh-pages.
#' @param sort (string) What to sort results by. Can be either created, updated, popularity
#'   (comment count) or long-running (age, filtering by pulls updated in the last month).
#'   Default: created
#' @param direction (string) The direction of the sort. Can be either asc or desc.
#'   Default: desc when sort is created or sort is not specified, otherwise asc."
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the pull requests (see GitHub's API documentation for details).
#'
#' @export
#'
gh_pull_requests <- function(
  repo,
  state     = NULL,
  head      = NULL,
  base      = NULL,
  sort      = NULL,
  direction = NULL,
  n_max     = 1000L,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  stopifnot(is_repo(repo))
  stopifnot(is.null(state) || is_string(state))
  stopifnot(is.null(head) || is_string(head))
  stopifnot(is.null(base) || is_string(base))
  stopifnot(is.null(sort) || is_string(sort))
  stopifnot(is.null(direction) || is_string(direction))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  pull_requests <- gh_url(
    "repos", repo, "pulls", api = api,
    state = state, head = head, base = base, sort = sort, direction = direction) %>%
    gh_page(n_max = n_max, token = token, ...)

  bind_fields(pull_requests, list(
    id                  = c("id",                        as = "integer"),
    number              = c("number",                    as = "integer"),
    title               = c("title",                     as = "character"),
    body                = c("body",                      as = "character"),
    user_login          = c("user", "login",             as = "character"),
    state               = c("state",                     as = "character"),
    created_at          = c("created_at",                as = "datetime"),
    updated_at          = c("updated_at",                as = "datetime"),
    closed_at           = c("closed_at",                 as = "datetime"),
    merged_at           = c("merged_at",                 as = "datetime"),
    merge_commit_sha    = c("merge_commit_sha",          as = "character"),
    head_ref            = c("head", "ref",               as = "character"),
    head_sha            = c("head", "sha",               as = "character"),
    head_user_login     = c("head", "user", "login",     as = "character"),
    head_repo_full_name = c("head", "repo", "full_name", as = "character"),
    locked              = c("locked",                    as = "logical"),
    url                 = c("url",                       as = "character")))
}

#  FUNCTION: gh_pull_commits ------------------------------------------------------------------
#
#' List commits on a pull request
#'
#' <https://developer.github.com/v3/pulls/#list-commits-on-a-pull-request>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the commits on a pull request (see GitHub's API documentation
#'   for details).
#'
#' @export
#'
gh_pull_commits <- function(
  pull_request,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  commits <- gh_page(
    gh_url("repos", repo, "pulls", pull_request, "commits", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(commits, list(
    sha            = c("sha",                      as = "character"),
    author_login   = c("author", "login",          as = "character"),
    commit_date    = c("commit", "author", "date", as = "datetime"),
    commit_message = c("commit", "message",        as = "character"),
    url            = c("url",                      as = "character"),
    parents_sha    = "")) %>%
    mutate(parents_sha = sapply(commits, function(commit) {
      sapply(commit[["parents"]], getElement, "sha")
    }))
}

#  FUNCTION: gh_pull_files --------------------------------------------------------------------
#
#' List pull requests files
#'
#' <https://developer.github.com/v3/pulls/#list-pull-requests-files>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the files changed on a pull request (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
gh_pull_files <- function(
  pull_request,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  files <- gh_page(
    gh_url("repos", repo, "pulls", pull_request, "files", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(files, list(
    sha          = c("sha",          as = "character"),
    filename     = c("filename",     as = "character"),
    status       = c("status",       as = "character"),
    additions    = c("additions",    as = "integer"),
    deletions    = c("deletions",    as = "integer"),
    changes      = c("changes",      as = "integer"),
    blob_url     = c("blob_url",     as = "character"),
    contents_url = c("contents_url", as = "character"),
    patch        = c("patch",        as = "character")))
}

#  FUNCTION: is_pull_merged -------------------------------------------------------------------
#
#' Get if a pull request has been merged
#'
#' <https://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE if the pull request has been merged, FALSE otherwise (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
is_pull_merged <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(
      gh_url("repos", repo, "pulls", pull_request, "merge", api = api),
      accept = "raw", token = token, ...)
  }))

  attributes(response) <- NULL
  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_pull_review -------------------------------------------------------------------
#
#' Get a single review
#'
#' <https://developer.github.com/v3/pulls/reviews/#get-a-single-review>
#'
#' @param review (integer) The number assigned to the review.
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the pull request review (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_pull_review <- function(
  review,
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(review))
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_get(
    gh_url("repos", repo, "pulls", pull_request, "reviews", review, api = api),
    token = token, ...)
}

#  FUNCTION: gh_pull_reviews ------------------------------------------------------------------
#
#' List reviews on a pull request
#'
#' <https://developer.github.com/v3/pulls/reviews/#list-reviews-on-a-pull-request>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the pull request reviews (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_pull_reviews <- function(
  pull_request,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_page(
    gh_url("repos", repo, "pulls", pull_request, "reviews", api = api),
    n_max = n_max, token = token, ...)
}

#  FUNCTION: gh_pull_comment ------------------------------------------------------------------
#
#' Get a single pull request review comment
#'
#' <https://developer.github.com/v3/pulls/comments/#get-a-single-comment>
#'
#' @param comment (integer) The number assigned to the review comment.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the pull request review comment (see GitHub's API documentation
#'   for details).
#'
#' @export
#'
gh_pull_comment <- function(
  comment,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(comment))
  stopifnot(is_repo(repo))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_get(gh_url("repos", repo, "pulls/comments", comment, api = api), token = token, ...)
}

#  FUNCTION: gh_pull_comments -----------------------------------------------------------------
#
#' List review comments on a pull request or all pull requests in a repository.
#'
#' <https://developer.github.com/v3/pulls/reviews/#get-comments-for-a-single-review>
#' <https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request>
#' <https://developer.github.com/v3/pulls/comments/#list-comments-in-a-repository>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param pull_request (integer, optional) The number assigned to the pull request. If
#'   not specified, the comments on all pull requests are listed.
#' @param review (integer, optional) The number assigned to the review. If specified,
#'   along with a `pull_request`, only the comments on this review are returned.
#' @param sort (string) Can be either created or updated. Default: created
#' @param direction (string) Can be either asc or desc. Ignored without sort parameter.
#' @param since (string) Only comments updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the pull request comments (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_pull_comments <- function(
  repo,
  pull_request,
  review,
  sort      = NULL,
  direction = NULL,
  since     = NULL,
  n_max     = 1000L,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  stopifnot(is_repo(repo))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  if (missing(pull_request)) {
    url <- gh_url(
      "repos", repo, "pulls/comments", api = api,
      sort = sort, direction = direction, since = since)
  } else if (missing(review)) {
    stopifnot(is_count(pull_request))
    url <- gh_url("repos", repo, "pulls", pull_request, "comments", api = api)
  } else {
    stopifnot(is_count(review))
    url <- gh_url("repos", repo, "pulls", pull_request, "reviews", review, "comments", api = api)
  }

  gh_page(url, n_max = n_max, token = token, ...)
}

#  FUNCTION: gh_pull_review_requests ----------------------------------------------------------
#
#' List pull request review requests
#'
#' <https://developer.github.com/v3/pulls/review_requests/#list-review-requests>
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the pull request review requests (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
gh_pull_review_requests <- function(
  pull_request,
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  stopifnot(is_count(pull_request))
  stopifnot(is_repo(repo))
  stopifnot(is_count(n_max))
  stopifnot(is_sha(token))
  stopifnot(is_url(api))

  gh_page(
    gh_url("repos", repo, "pulls", pull_request, "requested_reviewers", api = api),
    n_max = n_max, token = token, ...)
}
