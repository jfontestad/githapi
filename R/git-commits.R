#  FUNCTION: view_commits ---------------------------------------------------------------------
#
#' View information about Git commits.
#'
#' This function returns details about the commits for the specified repository in GitHub.
#'
#' <https://developer.github.com/v3/git/commits/#get-a-commit>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param shas (character, optional) The SHAs of the commits.
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
  shas,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_sha(token))
  assert(is_url(api))

  commits_list <- map(shas, function(sha) {
    info("Getting commit for sha '", sha, "' from repository '", repo, "'")
    tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "git/commits", sha, api = api),
        token = token, ...)
    }, error = function(e) {
      info("sha '", sha, "' failed!")
      e
    })
  })

  if (any(map_vec(commits_list, is, "error"))) {
    collate_errors(commits_list, "view_commits() failed!")
  }

  info("Transforming results")
  commits_tbl <- bind_fields(commits_list, list(
    sha             = c("sha",                as = "character"),
    message         = c("message",            as = "character"),
    author_name     = c("author", "name",     as = "character"),
    author_email    = c("author", "email",    as = "character"),
    committer_name  = c("committer", "name",  as = "character"),
    committer_email = c("committer", "email", as = "character"),
    date            = c("author", "date",     as = "datetime"),
    url             = c("url",                as = "character"),
    tree_sha        = c("tree", "sha",        as = "character"),
    tree_url        = c("tree", "url",        as = "character"),
    parent_sha      = "",
    parent_url      = "")) %>%
    mutate(parent_sha = map(commits_list, use_names = FALSE, function(cm) {
      map_vec(cm$parents, getElement, "sha")
    })) %>%
    mutate(parent_url = map(commits_list, use_names = FALSE, function(cm) {
      map_vec(cm$parents, getElement, "url")
    }))

  info("Done")
  commits_tbl
}
