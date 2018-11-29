#  FUNCTION: view_blobs -----------------------------------------------------------------------
#
#' View information about Git blobs (files)
#'
#' This function returns details about the blobs (files) for the specified repository in
#' GitHub. Note: This API supports blobs up to 100 megabytes in size.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param shas (character, optional) The SHAs of the blobs (files).
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the blobs, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/blobs/) for more details):
#'   - **sha**: The SHA of the blob.
#'   - **content**: The content of the blob, in base64 encoding.
#'   - **encoding**: The encoding used for `content`. Always `"base64"`.
#'   - **url**: The URL to get the blob details from GitHub.
#'   - **size**: The size of the blob, in bytes.
#'
#' @export
#'
view_blobs <- function(
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

  blobs_list <- map(shas, function(sha) {
    info("Getting blob '", sha, "' from repository '", repo, "'")
    tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "git/blobs", sha, api = api),
        token = token, ...)
    }, error = function(e) {
      info("Blob '", sha, "' failed!")
      e
    })
  })

  if (any(map_vec(blobs_list, is, "error"))) {
    collate_errors(blobs_list, "view_blobs() failed!")
  }

  info("Transforming results")
  blobs_tbl <- bind_fields(blobs_list, list(
    sha      = c("sha",      as = "character"),
    content  = c("content",  as = "character"),
    encoding = c("encoding", as = "character"),
    url      = c("url",      as = "character"),
    size     = c("size",     as = "integer")))

  info("Done")
  blobs_tbl
}
