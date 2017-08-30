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
      filenames   = map_chr(files, ~str_c(.$filename, collapse = ",")),
      languages   = map_chr(files, ~ifelse(is.null(.$language), NA, str_c(.$language, collapse = ","))),
      file_sizes  = map_chr(files, ~str_c(.$size, collapse = ",")),
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
