#  FUNCTION: gh_gist --------------------------------------------------------------------------
#
#' Get a single gist
#'
#' <https://developer.github.com/v3/gists/#get-a-single-gist>
#' <https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the gist (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gist <- function(
  gist,
  sha,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(sha)) {
    url <- gh_url("gists", gist, api = api)
  } else {
    assert(is_sha(sha))
    url <- gh_url("gists", gist, sha, api = api)
  }

  gh_get(url, token = token, ...)
}

#  FUNCTION: gh_gists -------------------------------------------------------------------------
#
#' List a user's gists, or all public or starred gists
#'
#' <https://developer.github.com/v3/gists/#list-a-users-gists>
#' <https://developer.github.com/v3/gists/#list-a-users-gists>
#' <https://developer.github.com/v3/gists/#list-all-public-gists>
#' <https://developer.github.com/v3/gists/#list-starred-gists>
#'
#' @param user (string) The GitHub username of the user. If user set to 'public' all public
#'   gists are returned; and if user is set to 'starred' all the starred repositories for the
#'   authenticated user are returned.
#' @param since (string) A timestamp in ISO 8601 format: `YYYY-MM-DDTHH:MM:SSZ`. Only gists
#'   updated at or after this time are returned."
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `"https://api.github.com"`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the gists (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gists <- function(
  user,
  since = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_null(since) || is_scalar_character(since))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(user)) {
    url <- gh_url("gists", since = since, api = api)
  } else {
    assert(is_scalar_character(user))
    if (user %in% c("public", "starred")) {
      url <- gh_url("gists", user, since = since, api = api)
    } else {
      url <- gh_url("users", user, "gists", since = since, api = api)
    }
  }

  gists <- gh_page(url, n_max = n_max, token = token, ...)

  bind_fields(gists, list(
    id          = c("id",             as = "character"),
    description = c("description",    as = "character"),
    owner_login = c("owner", "login", as = "character"),
    created_at  = c("created_at",     as = "datetime"),
    updated_at  = c("updated_at",     as = "datetime"),
    comments    = c("comments",       as = "integer"),
    files       = "",
    public      = c("public",         as = "logical"),
    url         = c("url",            as = "character"))) %>%
    mutate(files = lapply(gists, function(g) {
      bind_fields(g$files, list(
        filename = c("filename", as = "character"),
        language = c("language", as = "character"),
        size     = c("size",     as = "integer")))
    }))
}

#  FUNCTION: gh_gist_commits ------------------------------------------------------------------
#
#' List gist commits
#'
#' <https://developer.github.com/v3/gists/#list-gist-commits>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the gist commits (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gist_commits <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  commits <- gh_page(
    gh_url("gists", gist, "commits", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(commits, list(
    version           = c("version",                    as = "character"),
    user_login        = c("user", "login",              as = "character"),
    committed_at      = c("committed_at",               as = "datetime"),
    changes_total     = c("change_status", "total",     as = "integer"),
    changes_additions = c("change_status", "additions", as = "integer"),
    changes_deletions = c("change_status", "deletions", as = "integer"),
    url               = c("url",                        as = "character")))
}

#  FUNCTION: is_gist_starred ------------------------------------------------------------------
#
#' Check if a gist is starred
#'
#' <https://developer.github.com/v3/gists/#check-if-a-gist-is-starred>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return TRUE if the gist has been starred, FALSE otherwise (see GitHub's API documentation
#'   for details).
#'
#' @export
#'
is_gist_starred <- function(
  gist,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_sha(token))
  assert(is_url(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_get(
      gh_url("gists", gist, "star", api = api),
      accept = "raw", token = token, ...)
  }))

  attributes(response) <- NULL
  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_gist_forks --------------------------------------------------------------------
#
#' List gist forks
#'
#' <https://developer.github.com/v3/gists/#list-gist-forks>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the forks (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gist_forks <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  forks <- gh_page(
    gh_url("gists", gist, "forks", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(forks, list(
    id          = c("id",             as = "character"),
    description = c("description",    as = "character"),
    owner_login = c("owner", "login", as = "character"),
    created_at  = c("created_at",     as = "datetime"),
    updated_at  = c("updated_at",     as = "datetime"),
    public      = c("public",         as = "logical"),
    comments    = c("comments",       as = "integer"),
    url         = c("url",            as = "character")))
}

#  FUNCTION: gh_gist_comment ------------------------------------------------------------------
#
#' Get a single comment
#'
#' <https://developer.github.com/v3/gists/comments/#get-a-single-comment>
#'
#' @param comment (integer) The ID of the comment in GitHub.
#' @param gist (string) The ID of the gist in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the gist comment (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gist_comment <- function(
  comment,
  gist,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_integerish(comment) && isTRUE(comment > 0))
  assert(is_scalar_character(gist))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("gists", gist, "comments", comment, api = api),
    token = token, ...)
}

#  FUNCTION: gh_gist_comments -----------------------------------------------------------------
#
#' List comments on a gist
#'
#' <https://developer.github.com/v3/gists/comments/#list-comments-on-a-gist>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the gist comments (see GitHub's API documentation for details).
#'
#' @export
#'
gh_gist_comments <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_scalar_integerish(n_max) && isTRUE(n_max > 0))
  assert(is_sha(token))
  assert(is_url(api))

  comments <- gh_page(
    gh_url("gists", gist, "comments", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(comments, list(
    id         = c("id",            as = "integer"),
    body       = c("body",          as = "character"),
    user_login = c("user", "login", as = "character"),
    created_at = c("created_at",    as = "datetime"),
    updated_at = c("updated_at",    as = "datetime"),
    url        = c("url",           as = "character")))
}

#  FUNCTION: gh_save_gist -------------------------------------------------------------------------
#
#' Save files in a gist
#'
#' <https://developer.github.com/v3/gists/#get-a-single-gist>
#' <https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist>
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param path (string) The location to save the files to.
#' @param files (character, optional) The name of the files to download. Default: all files in
#'   the gist
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_gist()].
#'
#' @return The file path of the saved file (invisibly).
#'
#' @export
#'
gh_save_gist <- function(
  gist,
  path,
  files,
  sha,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(gist))
  assert(is_scalar_character(path))
  assert(is_sha(token))
  assert(is_url(api))

  gist_files <- gh_gist(gist, sha = sha, token = token, api = api, ...)[["files"]]
  gist_urls <- sapply(gist_files, getElement, "raw_url")

  if (!missing(files)) {
    assert(is_character(files))
    if (!all(files %in% names(gist_urls)))
      error("Cannot find all specified files")
    gist_urls <- gist_urls[names(gist_urls) %in% files]
  }

  if (!dir.exists(path)) dir.create(path)

  for (gist in names(gist_urls)) {
    gh_download_binary(gist_urls[[gist]], path = file.path(path, basename(gist)), token = token)
  }

  invisible(path)
}

#  FUNCTION: gh_source_gist -------------------------------------------------------------------
#
#' Source an R file from a gist
#'
#' <https://developer.github.com/v3/gists/#get-a-single-gist>
#' <https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist>
#'
#' @param file (string) The name of the file to source.
#' @param gist (string) The ID of the gist in GitHub.
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [base::source()].
#'
#' @return Nothing. The file is sourced into global environment.
#'
#' @export
#'
gh_source_gist <- function(
  file,
  gist,
  sha,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_scalar_character(file))
  assert(is_scalar_character(gist))
  assert(is_sha(token))
  assert(is_url(api))

  temp_path <- tempdir()
  on.exit(unlink(file.path(tempdir(), basename(file)), recursive = TRUE))

  gh_save_gist(gist = gist, path = temp_path, files = file, sha = sha, token = token, api = api)
  source(file.path(tempdir(), basename(file)), ...)
}
