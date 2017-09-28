#  FUNCTION: gh_gist --------------------------------------------------------------------------
#' Get a single gist
#'
#' url{https://developer.github.com/v3/gists/#get-a-single-gist}
#' url{https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the gist (see GitHub's API documentation for details).
#' @export
gh_gist <- function(
  gist,
  sha,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(sha)) {
    url <- gh_url("gists", gist, api = api)
  } else {
    assert_that(is.string(sha) && identical(str_length(sha), 40L))
    url <- gh_url("gists", gist, sha, api = api)
  }

  url %>% gh_get(token = token, ...)
}

#  FUNCTION: gh_gists -------------------------------------------------------------------------
#' List a user's gists, or all public or starred gists.
#'
#' url{https://developer.github.com/v3/gists/#list-a-users-gists}
#' url{https://developer.github.com/v3/gists/#list-a-users-gists}
#' url{https://developer.github.com/v3/gists/#list-all-public-gists}
#' url{https://developer.github.com/v3/gists/#list-starred-gists}
#'
#' @param user (string) The GitHub username of the user. If user set to 'public' all public
#'   gists are returned; and if user is set to 'starred' all the starred repositories for the
#'   authenticated user are returned.
#' @param since (string) A timestamp in ISO 8601 format: YYYY-MM-DDTHH:MM:SSZ. Only gists
#'   updated at or after this time are returned."
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_page}}.
#' @return A tibble describing the gists (see GitHub's API documentation for details).
#' @export
gh_gists <- function(
  user,
  since = NULL,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.null(since) || is.string(since))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  if (missing(user)) {
    url <- gh_url("gists", api = api)
  } else {
    assert_that(is.string(user))
    if (user %in% c("public", "starred")) {
      url <- gh_url("gists", user)
    } else {
      url <- gh_url("users", user, "gists")
    }
  }

  url %>%
    gh_page(n_max = n_max, token = token, ...) %>%
    map(function(g) {g$files <- unname(g$files); g}) %>%
    toJSON(auto_unbox = TRUE) %>%
    fromJSON(simplifyDataFrame = TRUE, flatten = TRUE) %>%
    as_tibble() %>%
    set_names(str_replace_all(names(.), "\\.", "_")) %>%
    mutate(
      description = ifelse(as.character(description) == "list()", NA, as.character(description)),
      owner_login = ifelse(has_name(., "owner_login"), owner_login, NA_character_),
      filenames   = collapse_list(files, "filename"),
      languages   = collapse_list(files, "language"),
      file_sizes  = collapse_list(files, "size"),
      created_at  = parse_datetime(created_at),
      updated_at  = parse_datetime(updated_at)) %>%
    select(
      id, description, owner_login, created_at, updated_at, comments,
      filenames, languages, file_sizes, public, url)
}

#  FUNCTION: gh_gist_commits ------------------------------------------------------------------
#' List gist commits
#'
#' url{https://developer.github.com/v3/gists/#list-gist-commits}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the gist commits (see GitHub's API documentation for details).
#' @export
gh_gist_commits <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("gists", gist, "commits", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(committed_at = parse_datetime(committed_at)) %>%
    select(
      version, user_login, committed_at, change_status_total,
      change_status_additions, change_status_deletions, url)
}

#  FUNCTION: is_gist_starred ------------------------------------------------------------------
#' Check if a gist is starred
#'
#' url{https://developer.github.com/v3/gists/#check-if-a-gist-is-starred}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return TRUE if the gist has been starred, FALSE otherwise (see GitHub's API documentation
#'   for details).
#' @export
is_gist_starred <- function(
  gist,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  response <- try(silent = TRUE, suppressMessages({
    gh_url("gists", gist, "star", api = api) %>%
      gh_get(accept = "raw", token = token, ...)
  }))

  if (identical(response, "")) {
    TRUE
  } else {
    FALSE
  }
}

#  FUNCTION: gh_gist_forks --------------------------------------------------------------------
#' List gist forks
#'
#' url{https://developer.github.com/v3/gists/#list-gist-forks}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the forks (see GitHub's API documentation for details).
#' @export
gh_gist_forks <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("gists", gist, "forks", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at)) %>%
    select(id, description, owner_login, created_at, updated_at, public, comments, url)
}

#  FUNCTION: gh_gist_comment ------------------------------------------------------------------
#' Get a single comment
#'
#' url{https://developer.github.com/v3/gists/comments/#get-a-single-comment}
#'
#' @param comment (integer) The ID of the comment in GitHub.
#' @param gist (string) The ID of the gist in GitHub.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A list describing the gist comment (see GitHub's API documentation for details).
#' @export
gh_gist_comment <- function(
  comment,
  gist,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.count(comment))
  assert_that(is.string(gist))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("gists", gist, "comments", comment, api = api) %>%
    gh_get(token = token, ...)
}

#  FUNCTION: gh_gist_comments -----------------------------------------------------------------
#' List comments on a gist
#'
#' url{https://developer.github.com/v3/gists/comments/#list-comments-on-a-gist}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return A tibble describing the gist comments (see GitHub's API documentation for details).
#' @export
gh_gist_comments <- function(
  gist,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.count(n_max))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gh_url("gists", gist, "comments", api = api) %>%
    gh_page(simplify = TRUE, n_max = n_max, token = token, ...) %>%
    mutate(
      created_at = parse_datetime(created_at),
      updated_at = parse_datetime(updated_at)) %>%
    select(id, body, user_login, created_at, updated_at, url)
}

#  FUNCTION: gh_save_gist -------------------------------------------------------------------------
#' Save files in a gist
#'
#' url{https://developer.github.com/v3/gists/#get-a-single-gist}
#' url{https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist}
#'
#' @param gist (string) The ID of the gist in GitHub.
#' @param path (string) The location to save the files to.
#' @param files (character, optional) The name of the files to download. Default: all files in
#'   the gist
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{gh_get}}.
#' @return The file path of the saved file (invisibly).
#' @export
gh_save_gist <- function(
  gist,
  path,
  files,
  sha,
  token   = gh_token(),
  api     = getOption("github.api"),
  ...)
{
  assert_that(is.string(gist))
  assert_that(is.string(path))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  gist_files <- gh_gist(gist, sha = sha, token = token, api = api) %>%
    .[["files"]] %>%
    bind_rows()

  if (!missing(files)) {
    assert_that(is.character(files))
    if (!all(files %in% gist_files$filename))
      stop("Cannot find all specified files")
    gist_files <- filter(gist_files, filename %in% files)
  }

  if (!dir.exists(path)) dir.create(path)

  map2(gist_files$filename, gist_files$raw_url, function(filename, url) {
    gh_get(url, accept = "raw", binary = TRUE) %>%
      writeBin(file.path(path, basename(filename)))
  })

  invisible(path)
}

#  FUNCTION: gh_source_gist -------------------------------------------------------------------
#' Source an R file from a gist
#'
#' url{https://developer.github.com/v3/gists/#get-a-single-gist}
#' url{https://developer.github.com/v3/gists/#get-a-specific-revision-of-a-gist}
#'
#' @param file (string) The name of the file to source.
#' @param gist (string) The ID of the gist in GitHub.
#' @param sha (string, optional) The SHA-1 of the version required. Default: latest version.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable \code{"GITHUB_TOKEN"} or \code{"GITHUB_PAT"}.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable \code{"GITHUB_API_URL"} or \code{"https://api.github.com"}.
#' @param ... Parameters passed to \code{\link{source}}.
#' @return Nothing. The file is sourced into global environment.
#' @export
gh_source_gist <- function(
  file,
  gist,
  sha,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert_that(is.string(file))
  assert_that(is.string(gist))
  assert_that(is.string(token) && identical(str_length(token), 40L))
  assert_that(is.string(api))

  temp_path <- tempdir()
  on.exit(unlink(file.path(tempdir(), basename(file)), recursive = TRUE))

  gh_save_gist(gist = gist, path = temp_path, files = file, sha = sha, token = token, api = api)
  source(file.path(tempdir(), basename(file)), ...)
}
