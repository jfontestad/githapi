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

#  FUNCTION: create_blobs ---------------------------------------------------------------------
#
#' Create blobs (files).
#'
#' This function creates the specified blobs within a repository in GitHub.
#'
#' <https://developer.github.com/v3/git/blobs/#create-a-blob>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param contents (character) The contents of the files.
#' @param encoding (character, optional) The encoding used for the content. Currently,
#'   `"utf-8"` and `"base64"` are supported. Can supply either a single string, which will apply
#'   to every blob, or a character vector the same length as the contents, specifying the
#'   encoding for each blob separately. Default: `"utf-8"`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the blobs, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **sha**: The SHA of the blob.
#'   - **url**: The URL to get the blob details from GitHub.
#'
#' @export
#'
create_blobs <- function(
  repo,
  contents,
  encoding = "utf-8",
  token    = getOption("github.token"),
  api      = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(contents))
  assert(is_character(encoding) && (identical(length(encoding), 1L)) || identical(length(encoding), length(contents)))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    content  = contents,
    encoding = encoding)

  info("Posting ", length(contents), " blob(s) to repository '", repo, "'")
  blobs_list <- pmap(params, function(content, encoding) {
    tryCatch({
      gh_request(
        "POST", gh_url("repos", repo, "git/blobs", api = api),
        payload = list(content = content, encoding = encoding),
        token = token, ...)
    }, error = function(e) {
      info("blob failed!")
      e
    })
  })

  if (any(map_vec(blobs_list, is, "error"))) {
    collate_errors(blobs_list, "create_blobs() failed!")
  }

  info("Transforming results")
  blobs_tbl <- bind_fields(blobs_list, list(
    sha = c("sha", as = "character"),
    url = c("url", as = "character")))

  info("Done")
  blobs_tbl
}

#  FUNCTION: upload_blobs ---------------------------------------------------------------------
#
#' Upload blobs (files).
#'
#' This function uploads blobs from the specified file paths into a repository in GitHub.
#'
#' <https://developer.github.com/v3/git/blobs/#create-a-blob>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (character) The paths to the files.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the blobs, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the file.
#'   - **sha**: The SHA of the blob.
#'   - **url**: The URL to get the blob details from GitHub.
#'
#' @export
#'
upload_blobs <- function(
  repo,
  paths,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(all(map_vec(paths, is_readable)))
  assert(is_sha(token))
  assert(is_url(api))

  blobs_list <- map(paths, function(path) {
    info("Uploading file '", basename(path), "' to repository '", repo, "'")
    tryCatch({
      content <- readBin(path, "raw", file.info(path)$size) %>% base64_enc()

      gh_request(
        "POST", gh_url("repos", repo, "git/blobs", api = api),
        payload = list(content = content, encoding = "base64"),
        token = token, ...)
    }, error = function(e) {
      info("Blob '", path, "' failed!")
      e
    })
  })

  if (any(map_vec(blobs_list, is, "error"))) {
    collate_errors(blobs_list, "upload_blobs() failed!")
  }

  info("Transforming results")
  blobs_tbl <- bind_fields(blobs_list, list(
    name = "",
    sha  = c("sha", as = "character"),
    url  = c("url", as = "character"))) %>%
    mutate(name = basename(paths))

  info("Done")
  blobs_tbl
}
