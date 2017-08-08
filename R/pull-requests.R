#  FUNCTION: gh_pull_requests -----------------------------------------------------------------
#' List pull requests
#'
#' url{https://developer.github.com/v3/pulls/#list-pull-requests}
#'
#' @param state (string) Either open, closed, or all to filter by state. Default: open
#' @param head (string) Filter pulls by head user and branch name in the format of
#'   \code{user:ref-name}. Example: \code{github:new-script-format}.
#' @param base (string) Filter pulls by base branch name. Example: gh-pages.
#' @param sort (string) What to sort results by. Can be either created, updated, popularity
#'   (comment count) or long-running (age, filtering by pulls updated in the last month).
#'   Default: created
#' @param direction (string) The direction of the sort. Can be either asc or desc.
#'   Default: desc when sort is created or sort is not specified, otherwise asc."
#' @return A tibble describing the pull requests
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
