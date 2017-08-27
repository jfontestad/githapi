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
