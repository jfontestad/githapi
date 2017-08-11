#  FUNCTION: gh_pull_request ------------------------------------------------------------------
#' Get a single pull request
#'
#' url{https://developer.github.com/v3/pulls/#get-a-single-pull-request}
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the pull request (see GitHub's API documentation for details).
#' @export
gh_pull_request <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "pulls", pull_request, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_pull_requests -----------------------------------------------------------------
#' List pull requests
#'
#' url{https://developer.github.com/v3/pulls/#list-pull-requests}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param state (string) Either open, closed, or all to filter by state. Default: open
#' @param head (string) Filter pulls by head user and branch name in the format of
#'   \code{user:ref-name}. Example: \code{github:new-script-format}.
#' @param base (string) Filter pulls by base branch name. Example: gh-pages.
#' @param sort (string) What to sort results by. Can be either created, updated, popularity
#'   (comment count) or long-running (age, filtering by pulls updated in the last month).
#'   Default: created
#' @param direction (string) The direction of the sort. Can be either asc or desc.
#'   Default: desc when sort is created or sort is not specified, otherwise asc."
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the pull requests (see GitHub's API documentation for details).
#' @export
gh_pull_requests <- function(
  repo,
  state     = NULL,
  head      = NULL,
  base      = NULL,
  sort      = NULL,
  direction = NULL,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.null(state) || is.string(state))
  assert_that(is.null(head) || is.string(head))
  assert_that(is.null(base) || is.string(base))
  assert_that(is.null(sort) || is.string(sort))
  assert_that(is.null(direction) || is.string(direction))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url(
    "repos", repo, "pulls", api = api,
    state = state, head = head, base = base, sort = sort, direction = direction) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at),
      closed_at  = parse_datetime(closed_at),
      merged_at  = parse_datetime(merged_at)) %>%
    select(
      id, number, title, body, user_login, state, created_at, updated_at, closed_at, merged_at,
      merge_commit_sha, head_ref, head_sha, head_user_login, head_repo_full_name, locked, url)
}

#  FUNCTION: gh_pull_commits ------------------------------------------------------------------
#' List commits on a pull request
#'
#' url{https://developer.github.com/v3/pulls/#list-commits-on-a-pull-request}
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the commits on a pull request (see GitHub's API documentation
#'   for details).
#' @export
gh_pull_commits <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "pulls", pull_request, "commits", api = api) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    mutate(commit_date = parse_datetime(commit_author_date)) %>%
    select(sha, author_login, commit_date, commit_message, url, parents_sha)
}

#  FUNCTION: gh_pull_files --------------------------------------------------------------------
#' List pull requests files
#'
#' url{https://developer.github.com/v3/pulls/#list-pull-requests-files}
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the files changed on a pull request (see GitHub's API
#'   documentation for details).
#' @export
gh_pull_files <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "pulls", pull_request, "files", api = api) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows() %>%
    select(sha, filename, status, additions, deletions, changes, blob_url, contents_url, patch)
}

#  FUNCTION: gh_pull_merged -------------------------------------------------------------------
#' Get if a pull request has been merged
#'
#' url{https://developer.github.com/v3/pulls/#get-if-a-pull-request-has-been-merged}
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return TRUE if the pull request has been merged, FALSE otherwise (see GitHub's API
#'   documentation for details).
#' @export
gh_pull_merged <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, {
    gh_url("repos", repo, "pulls", pull_request, "merge", api = api) %>%
      gh_get(token = token, ...)
  })

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_pull_review -------------------------------------------------------------------
#' Get a single review
#'
#' url{https://developer.github.com/v3/pulls/reviews/#get-a-single-review}
#'
#' @param review (integer) The number assigned to the review.
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the pull request review (see GitHub's API documentation for
#'   details).
#' @export
gh_pull_review <- function(
  review,
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(review))
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "pulls", pull_request, "reviews", review, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_pull_reviews ------------------------------------------------------------------
#' List reviews on a pull request
#'
#' url{https://developer.github.com/v3/pulls/reviews/#list-reviews-on-a-pull-request}
#'
#' @param pull_request (integer) The number assigned to the pull request.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the pull request reviews (see GitHub's API documentation for
#'   details).
#' @export
gh_pull_reviews <- function(
  pull_request,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(pull_request))
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("repos", repo, "pulls", pull_request, "reviews", api = api) %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows()
}

#  FUNCTION: gh_pull_comment ------------------------------------------------------------------
#' Get a single pull request review comment
#'
#' url{https://developer.github.com/v3/pulls/comments/#get-a-single-comment}
#'
#' @param comment (integer) The number assigned to the review comment.
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A list describing the pull request review comment (see GitHub's API documentation
#'   for details).
#' @export
gh_pull_comment <- function(
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

  gh_url("repos", repo, "pulls/comments", comment, api = api) %>%
    gh_page(token = token, ...)
}

#  FUNCTION: gh_pull_comments -----------------------------------------------------------------
#' List review comments on a pull request or all pull requests in a repository.
#'
#' url{https://developer.github.com/v3/pulls/reviews/#get-comments-for-a-single-review}
#' url{https://developer.github.com/v3/pulls/comments/#list-comments-on-a-pull-request}
#' url{https://developer.github.com/v3/pulls/comments/#list-comments-in-a-repository}
#'
#' @param repo (string) The repository specified in the format: \code{"owner/repo"}.
#' @param pull_request (integer, optional) The number assigned to the pull request. If
#'   not specified, the comments on all pull requests are listed.
#' @param review (integer, optional) The number assigned to the review. If specified,
#'   along with a \code{pull_request}, only the comments on this review are returned.
#' @param sort (string) Can be either created or updated. Default: created
#' @param direction (string) Can be either asc or desc. Ignored without sort parameter.
#' @param since (string) Only comments updated at or after this time are returned. This is a
#'   timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the pull request comments (see GitHub's API documentation for
#'   details).
#' @export
gh_pull_comments <- function(
  repo,
  pull_request,
  review,
  sort      = NULL,
  direction = NULL,
  since     = NULL,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert_that(is.string(repo) && identical(str_count(repo, "/"), 1L))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(pull_request)) {
    url <- gh_url(
      "repos", repo, "pulls/comments", api = api,
      sort = sort, direction = direction, since = since)
  } else if (missing(review)) {
    assert_that(is.count(pull_request))
    url <- gh_url("repos", repo, "pulls", pull_request, "comments", api = api)
  } else {
    assert_that(is.count(review))
    url <- gh_url("repos", repo, "pulls", pull_request, "reviews", review, "comments", api = api)
  }

  url %>%
    gh_page(token = token, ...) %>%
    map(flatten_) %>%
    bind_rows()
}
