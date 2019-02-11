#  FUNCTION: view_tags ------------------------------------------------------------------------
#
#' View information about a tags
#'
#' This function returns details about the tags for the specified repository in GitHub. If no
#' tags are supplied then details about all the tags are returned. If tags are supplied then only
#' information about those ones is returned.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' <https://developer.github.com/v3/git/refs/#get-all-references>
#'
#' @param tags (character, optional) The tag names. If missing or `NULL` all the tags are
#'   requested.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the tags, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the tag.
#'   - **ref**: The full git reference for the tag.
#'   - **url**: The URL to get the tag details from GitHub.
#'   - **object_sha**: The SHA of the object tagged.
#'   - **object_type**: The type of the object tagged.
#'   - **object_url**: The URL to get the object details from GitHub
#'
#' @export
#'
view_tags <- function(
  tags,
  repo,
  n_max = 1000L,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  if (missing(repo)) {
    info("'repo' is missing, so using 'tags' argument: ", tags, level = 2)
    repo <- tags
    tags <- NULL
  }

  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(tags) || is_null(tags) || is_na(tags)) {
    info("Getting up to ", n_max, " tags from repository '", repo, "'")
    tags_list <- tryCatch({
      gh_page(
        gh_url("repos", repo, "git/refs/tags", api = api),
        n_max = n_max, token = token, ...)
    }, error = function(e) {
      warn("Failed!", level = 2)
      list(e)
    })
  } else {
    assert(is_character(tags))
    tags_list <- map(tags, function(tag) {
      info("Getting tag '", tag, "' from repository '", repo, "'")
      tryCatch({
        gh_request(
          "GET", gh_url("repos", repo, "git/refs/tags", tag, api = api),
          token = token, ...)
      }, error = function(e) {
        warn("Tag '", tag, "' failed!", level = 2)
        e
      })
    })
  }

  if (any(map_vec(tags_list, is, "error"))) {
    collate_errors(tags_list, "view_tags() failed!")
  }

  info("Transforming results", level = 2)
  tags_tbl <- bind_fields(tags_list[!map_vec(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 2)
  tags_tbl
}

#  FUNCTION: create_tags ----------------------------------------------------------------------
#
#' Create tags at specified commits.
#'
#' This function creates the specified tags at the specified commits within a repository in
#' GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param tags (character) The tag names.
#' @param shas (character) The SHAs of the commits to tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the tags, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the tag.
#'   - **ref**: The full git reference for the tag.
#'   - **url**: The URL to get the tag details from GitHub.
#'   - **object_sha**: The SHA of the object tagged.
#'   - **object_type**: The type of the object tagged.
#'   - **object_url**: The URL to get the object details from GitHub
#'
#' @export
#'
create_tags <- function(
  tags,
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(tags))
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  tags_list <- pmap(list(tags, shas), function(tag, sha) {
    info("Posting tag '", tag, "' to repository '", repo, "'")
    tryCatch({
      gh_request(
        "POST", gh_url("repos", repo, "git/refs", api = api),
        payload = list(ref = paste0("refs/tags/", tag), sha = sha),
        token = token, ...)
    }, error = function(e) {
      warn("Tag '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(tags_list, is, "error"))) {
    collate_errors(tags_list, "create_tags() failed!")
  }

  info("Transforming results", level = 2)
  tags_tbl <- bind_fields(tags_list[!map_vec(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 2)
  tags_tbl
}

#  FUNCTION: update_tags ----------------------------------------------------------------------
#
#' Update tags to new commits.
#'
#' This function updates the specified tags to point at new commits within a repository in
#' GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#update-a-reference>
#'
#' @param tags (character) The tag names.
#' @param shas (character) Ths SHAs of the commits to tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the tags, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the tag.
#'   - **ref**: The full git reference for the tag.
#'   - **url**: The URL to get the tag details from GitHub.
#'   - **object_sha**: The SHA of the object tagged.
#'   - **object_type**: The type of the object tagged.
#'   - **object_url**: The URL to get the object details from GitHub
#'
#' @export
#'
update_tags <- function(
  tags,
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(tags))
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  tags_list <- pmap(list(tags, shas), function(tag, sha) {
    info("Updating tag '", tag, "' in repository '", repo, "'")
    tryCatch({
      gh_request(
        "PATCH", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        payload = list(sha = sha, force = TRUE),
        token = token, ...)
    }, error = function(e) {
      warn("Tag '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(tags_list, is, "error"))) {
    collate_errors(tags_list, "update_tags() failed!")
  }

  info("Transforming results", level = 2)
  tags_tbl <- bind_fields(tags_list[!map_vec(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 2)
  tags_tbl
}

#  FUNCTION: delete_tags ----------------------------------------------------------------------
#
#' Delete tags.
#'
#' This function deletes the specified tags from a repository in GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#delete-a-reference>
#'
#' @param tags (character) The tag names.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named list containing `TRUE` if the tag was deleted. An error is thrown otherwise.
#'
#' @export
#'
delete_tags <- function(
  tags,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(tags))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  tags_list <- map(tags, function(tag) {
    info("Deleting tag '", tag, "' from repository '", repo, "'")
    tryCatch({
      gh_request(
        "DELETE", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        token = token, parse = FALSE, ...)
      TRUE
    }, error = function(e) {
      warn("Tag '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(tags_list, is, "error"))) {
    collate_errors(tags_list, "delete_tags() failed!")
  }

  info("Done", level = 2)
  tags_list
}

#  FUNCTION: tags_exist -----------------------------------------------------------------------
#
#' Determine whether a tags exist in the specified repository.
#'
#' This function returns `TRUE` if the tag exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param tags (character) The names of the tags.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each tag specified.
#'
#' @export
#'
tags_exist <- function(
  tags,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(tags))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  map_vec(tags, function(tag) {
    info("Checking tag '", tag, "' exists in repository '", repo, "'")
    tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        token = token, ...)
      TRUE
    }, error = function(e) {
      FALSE
    })
  })
}
