#  FUNCTION: view_blobs -----------------------------------------------------------------------
#
#' View information about Git blobs (files)
#'
#' This function returns details about the blobs (files) for the specified repository in
#' GitHub.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param shas (character) The SHAs of the blobs (files).
#' @param repo (string) The repository specified in the format: `owner/repo`.
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
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(shas) && all(map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  blobs_list <- try_map(shas, function(sha) {
    info("Getting blob '", sha, "' from repository '", repo, "'")

    gh_request(
      "GET", gh_url("repos", repo, "git/blobs", sha, api = api),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  blobs_tbl <- bind_fields(blobs_list, list(
    sha      = c("sha",      as = "character"),
    content  = c("content",  as = "character"),
    encoding = c("encoding", as = "character"),
    url      = c("url",      as = "character"),
    size     = c("size",     as = "integer")))

  info("Done", level = 3)
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
#' @param contents (character) The contents of the files.
#' @param repo (string) The repository specified in the format: `owner/repo`.
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
  contents,
  repo,
  encoding = "utf-8",
  token    = getOption("github.token"),
  api      = getOption("github.api"),
  ...)
{
  {
    (is_character(contents)) ||
      error("'contents' must be a character vector:\n  '", paste(contents, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_character(encoding) && (identical(length(encoding), 1L)) || identical(length(encoding), length(contents))) ||
      error("'encoding' must be either a string or a character vector the same length as 'contents':\n  '", paste(encoding, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  params <- tibble(
    content  = contents,
    encoding = encoding)

  info("Posting ", length(contents), " blob(s) to repository '", repo, "'")
  blobs_list <- try_pmap(params, function(content, encoding) {
    gh_request(
      "POST", gh_url("repos", repo, "git/blobs", api = api),
      payload = list(content = content, encoding = encoding),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  blobs_tbl <- bind_fields(blobs_list, list(
    sha = c("sha", as = "character"),
    url = c("url", as = "character")))

  info("Done", level = 3)
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
#' @param paths (character) The paths to the files.
#' @param repo (string) The repository specified in the format: `owner/repo`.
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
  paths,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (all(map(paths, simplify = TRUE, function(p) is_file(p) && is_readable(p)))) ||
      error("'paths' must be a character vector of readable file paths:\n  '", paste(paths, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  blobs_list <- try_map(paths, function(path) {
    info("Uploading file '", basename(path), "' to repository '", repo, "'")

    content <- readBin(path, "raw", file.info(path)$size) %>% base64_enc()

    gh_request(
      "POST", gh_url("repos", repo, "git/blobs", api = api),
      payload = list(content = content, encoding = "base64"),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  blobs_tbl <- bind_fields(blobs_list, list(
    name = "",
    sha  = c("sha", as = "character"),
    url  = c("url", as = "character"))) %>%
    mutate(name = basename(paths))

  info("Done", level = 3)
  blobs_tbl
}

#  FUNCTION: read_files -----------------------------------------------------------------------
#
#' Read the contents of text files
#'
#' This function reads the contents of the specified files from a repository in GitHub.
#' Note: This API supports files up to 100 megabytes in size.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param paths (string) The paths to the files.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If not specified the head of the default branch is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named character vector with each element containing a file's contents.
#'
#' @export
#'
read_files <- function(
  paths,
  ref,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(repo)) {
      info("'repo' is missing, so using 'ref' argument: ", ref, level = 2)
      repo <- ref
      ref <- NA
    }
    if (missing(ref) || is_null(ref)) {
      ref <- NA
    }

    (is_character(paths)) ||
      error("'paths' must be a character vector:\n  '", paste(paths, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_na(ref) || is_string(ref)) ||
      error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  all_files <- view_files(ref = ref, repo = repo, token = token, api = api)
  file_shas <- set_names(all_files$sha, all_files$path)

  files <- try_map(paths, simplify = TRUE, function(path) {
    info("Reading file '", path, "' from repository '", repo, "'")

    (path %in% names(file_shas)) ||
      error("Cannot find specified file path '", path, "' in repository '", repo, "'")

    file <- gh_request(
      "GET", gh_url("repos", repo, "git/blobs", file_shas[[path]], api = api),
      accept = "raw", token = token, ...)

    attr(file, "header") <- NULL
    file
  })

  info("Done", level = 3)
  files
}

#  FUNCTION: download_files -------------------------------------------------------------------
#
#' Download files and save them to a location
#'
#' This function downloads the contents of the specified files from a repository in GitHub and
#' saves them to a specified location. Note: This API supports files up to 100 megabytes in
#' size.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param paths (string) The paths to the files in the repository.
#' @param location (string) The location to save the files to.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If not specified the head of the default branch is
#'   used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_download_binary()].
#'
#' @return A named character vector with each element containing a file's contents (invisibly).
#'
#' @export
#'
download_files <- function(
  paths,
  location,
  ref,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(repo)) {
      info("'repo' is missing, so using 'ref' argument: ", ref, level = 2)
      repo <- ref
      ref <- NA
    }
    if (missing(ref) || is_null(ref)) {
      ref <- NA
    }

    (is_character(paths)) ||
      error("'paths' must be a character vector:\n  '", paste(paths, collapse = "'\n  '"), "'")
    (is_string(location)) ||
      error("'location' must be a string:\n  ", paste(location, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_na(ref) || is_string(ref)) ||
      error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  all_files <- view_files(ref = ref, repo = repo, token = token, api = api)
  file_shas <- set_names(all_files$sha, all_files$path)

  files <- try_map(paths, simplify = TRUE, function(path) {
    info("Downloading file '", path, "' from repository '", repo, "'")

    (path %in% names(file_shas)) ||
      error("Cannot find specified file path '", path, "' in repository '", repo, "'")

    file_path <- file.path(location, path) %>% normalizePath(winslash = "/", mustWork = FALSE)
    if (!dir.exists(dirname(file_path))) dir.create(dirname(file_path), recursive = TRUE)

    gh_download_binary(
      gh_url("repos", repo, "git/blobs", file_shas[[path]], api = api),
      path = file_path, token = token, ...)

    file_path
  })

  info("Done", level = 3)
  invisible(files)
}

#  FUNCTION: blobs_exist ----------------------------------------------------------------------
#
#' Determine whether blobs exist in the specified repository.
#'
#' This function returns `TRUE` if the blob exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param shas (character) The SHAs of the blobs (files).
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each blob specified.
#'
#' @export
#'
blobs_exist <- function(
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(shas) && all(map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must be a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  map(shas, simplify = TRUE, function(sha) {
    info("Checking blob '", sha, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", gh_url("repos", repo, "git/blobs", sha, api = api),
        token = token, ...)
      TRUE
    },
    on_error = function(e) {
      FALSE
    })
  })
}

#  FUNCTION: source_files ---------------------------------------------------------------------
#
#' Source R files from a GitHub repository
#'
#' This function downloads the specified files from a repository in GitHub to a temporary
#' location, and then executes [source()] on each of them. Note: This API supports files up
#' to 100 megabytes in size.
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param paths (string) The paths to the files in the repository.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If not specified the head of the default branch is
#'   used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [source()].
#'
#' @return A list containing the results of sourcing each file.
#'
#' @export
#'
source_files <- function(
  paths,
  ref,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(repo)) {
      info("'repo' is missing, so using 'ref' argument: ", ref, level = 2)
      repo <- ref
      ref <- NA
    }
    if (missing(ref) || is_null(ref)) {
      ref <- NA
    }

    (is_character(paths)) ||
      error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_na(ref) || is_string(ref)) ||
      error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  temp_path <- tempfile("githapi")
  on.exit(unlink(temp_path, recursive = TRUE))

  download_files(paths = paths, location = temp_path, ref = ref, repo = repo, token = token, api = api)

  source_list <- try_map(paths, function(path) {
    source(file.path(temp_path, path), ...)
  })

  invisible(source_list)
}
