#  FUNCTION: view_releases --------------------------------------------------------------------
#
#' View information about a releases
#'
#' This function returns details about the release made in GitHub for the specified repository.
#' If no tags are supplied then all the releases are returned. If tags are supplied then only
#' information about those releases is returned. Finally, if "latest" is specified in the place
#' of a tag the latest non-draft, non-prerelease release is returned.
#'
#' <https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name>
#'
#' <https://developer.github.com/v3/repos/releases/#get-the-latest-release>
#'
#' <https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository>
#'
#' @param tags (character, optional) The tag names. If missing or `NULL` all the releases are
#'   requested. If set to `"latest"`, then the latest non-draft, non-prerelease is returned.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the releases, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/releases/)
#'   for more details):
#'   - **id**: The unique ID assigned by GitHub.
#'   - **tag_name**: The name of the tag associated with the release.
#'   - **name**: The name, or title, of the release.
#'   - **body**: The body, or description, of the release.
#'   - **author_login**: The author of the release.
#'   - **draft**: Whether the release is a draft.
#'   - **prerelease**: Whether the release is a pre-release.
#'   - **target_commitish**: The commit or branch the release was applied to.
#'   - **created_at**: The date and time of creation.
#'   - **published_at**: The date and time of publication.
#'   - **assets**: The names of any assets associated with the release.
#'   - **zipball_url**: The URL to download a zip file of the release commit.
#'   - **url**: The URL to get the release details from GitHub.
#'
#' @export
#'
view_releases <- function(
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

  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_scalar_integerish(n_max) && isTRUE(n_max > 0)) ||
    error("'n_max' must be a positive integer:\n  '", paste(n_max, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  if (missing(tags) || is_null(tags) || all(is_na(tags))) {
    info("Getting up to ", n_max, " releases from repository '", repo, "'")

    releases_list <- try_catch({
      gh_page(
        gh_url("repos", repo, "releases", api = api),
        n_max = n_max, token = token, ...)
    })
  } else if (identical(tags, "latest")) {
    info("Getting latest release from repository '", repo, "'")

    releases_list <- try_catch({
      list(gh_request(
        "GET", url = gh_url("repos", repo, "releases/latest", api = api),
        token = token, ...))
    })
  } else {
    .Deprecated(msg = "Specifying tags within view_releases() has been deprecated. Please use view_release() instead.")

    (is_character(tags)) ||
      error("'tags' must be a character vector:\n  '", paste(tags, collapse = "'\n  '"), "'")

    releases_list <- try_map(tags, function(tag) {
      info("Getting release '", tag, "' from repository '", repo, "'")

      gh_request(
        "GET", url = gh_url("repos", repo, "releases/tags", tag, api = api),
        token = token, ...)
    })
  }

  info("Transforming results", level = 3)
  releases_tbl <- bind_fields(releases_list, list(
    id               = c("id",               as = "integer"),
    tag_name         = c("tag_name",         as = "character"),
    name             = c("name",             as = "character"),
    body             = c("body",             as = "character"),
    author_login     = c("author", "login",  as = "character"),
    draft            = c("draft",            as = "logical"),
    prerelease       = c("prerelease",       as = "logical"),
    target_commitish = c("target_commitish", as = "character"),
    created_at       = c("created_at",       as = "datetime"),
    published_at     = c("published_at",     as = "datetime"),
    assets           = "",
    zipball_url      = c("zipball_url",      as = "character"),
    url              = c("url",              as = "character"))) %>%
    mutate(assets = gh_map(releases_list, use_names = FALSE, list_fields, c("assets"), "name"))

  info("Done", level = 3)
  releases_tbl
}

#  FUNCTION: create_releases ----------------------------------------------------------------------
#
#' Create releases at specified commits.
#'
#' This function creates releases for specified commits. Tag names must be specified for each
#' commit along with release names and bodies (descriptions). You can also specify whether the
#' releases are draft and whether they are pre-releases.
#'
#' <https://developer.github.com/v3/repos/releases/#create-a-release>
#'
#' @param tags (character) The tag names.
#' @param commits (character) The commits to tag. Can be either a SHA or a branch name.
#' @param names (character) The names of the releases.
#' @param bodies (character) The bodies for the releases.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param draft (logical, optional) Whether the releases are draft. Default: `FALSE`.
#' @param prerelease (logical, optional) Whether the releases are a prerelease. Default: `FALSE`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the releases, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/releases/)
#'   for more details):
#'   - **id**: The unique ID assigned by GitHub.
#'   - **tag_name**: The name of the tag associated with the release.
#'   - **name**: The name, or title, of the release.
#'   - **body**: The body, or description, of the release.
#'   - **author_login**: The author of the release.
#'   - **draft**: Whether the release is a draft.
#'   - **prerelease**: Whether the release is a pre-release.
#'   - **target_commitish**: The commit or branch the release was applied to.
#'   - **created_at**: The date and time of creation.
#'   - **published_at**: The date and time of publication.
#'   - **assets**: The names of any assets associated with the release.
#'   - **zipball_url**: The URL to download a zip file of the release commit.
#'   - **url**: The URL to view the release details on GitHub.
#'
#' @export
#'
create_releases <- function(
  tags,
  commits,
  names,
  bodies,
  repo,
  draft      = FALSE,
  prerelease = FALSE,
  token      = getOption("github.token"),
  api        = getOption("github.api"),
  ...)
{
  .Deprecated("create_release", package = "githapi")

  (is_character(tags)) ||
    error("'tags' must be a character vector:\n  '", paste(tags, collapse = "'\n  '"), "'")
  (is_character(commits) && identical(length(commits), length(tags))) ||
    error("'commits' must be a character vector of the same length as 'tags':\n  'commits':  ", length(commits), "\n  'tags':    ", length(tags))
  (is_character(names) && identical(length(names), length(tags))) ||
    error("'names' must be a character vector of the same length as 'tags':\n  'names':  ", length(names), "\n  'tags':  ", length(tags))
  (is_character(bodies) && identical(length(bodies), length(tags))) ||
    error("'bodies' must be a character vector of the same length as 'tags':\n  'bodies':  ", length(bodies), "\n  'tags':   ", length(tags))
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_logical(draft) && (is_scalar_atomic(draft) || identical(length(draft), length(tags)))) ||
    error("'draft' must be a string or a character vector of the same length as 'tags':\n  'draft':  ", length(draft), "\n  'tags':  ", length(tags))
  (is_logical(prerelease) && (is_scalar_atomic(prerelease) || identical(length(prerelease), length(tags)))) ||
    error("'prerelease' must be a string or a character vector of the same length as 'tags':\n  'prerelease':  ", length(prerelease), "\n  'tags':       ", length(tags))
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  params <- tibble(
    tag        = tags,
    commit     = commits,
    name       = names,
    body       = bodies,
    draft      = draft,
    prerelease = prerelease)

  releases_list <- try_pmap(params, function(tag, commit, name, body, draft, prerelease) {
    info("Posting release '", tag, "' to repository '", repo, "'")

    gh_request(
      "POST", url = gh_url("repos", repo, "releases", api = api),
      payload = list(
        tag_name         = tag,
        target_commitish = commit,
        name             = name,
        body             = body,
        draft            = draft,
        prerelease       = prerelease),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  releases_tbl <- bind_fields(releases_list, list(
    id               = c("id",               as = "integer"),
    tag_name         = c("tag_name",         as = "character"),
    name             = c("name",             as = "character"),
    body             = c("body",             as = "character"),
    author_login     = c("author", "login",  as = "character"),
    draft            = c("draft",            as = "logical"),
    prerelease       = c("prerelease",       as = "logical"),
    target_commitish = c("target_commitish", as = "character"),
    created_at       = c("created_at",       as = "datetime"),
    published_at     = c("published_at",     as = "datetime"),
    assets           = "",
    zipball_url      = c("zipball_url",      as = "character"),
    url              = c("url",              as = "character"))) %>%
    mutate(assets = gh_map(releases_list, use_names = FALSE, list_fields, c("assets"), "name"))

  info("Done", level = 3)
  releases_tbl
}

#  FUNCTION: update_releases ----------------------------------------------------------------------
#
#' Update releases to new commits.
#'
#' This function updates an existing releases. It can update the names or bodies (description) of
#' the releases. You can also change whether the releases are draft and whether they are
#' pre-releases.
#'
#' <https://developer.github.com/v3/repos/releases/#edit-a-release>
#'
#' @param tags (character) The tag names.
#' @param names (character, optional) The names of the releases. If missing or `NA` then the
#'   name is not changed. Default: `NA`.
#' @param bodies (character, optional) The bodies for the releases. If missing or `NA` then the
#'   body is not changed. Default: `NA`.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param draft (logical, optional) Whether the releases are draft. Default: `FALSE`.
#' @param prerelease (logical, optional) Whether the releases are a prerelease. Default: `FALSE`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the releases, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/releases/)
#'   for more details):
#'   - **id**: The unique ID assigned by GitHub.
#'   - **tag_name**: The name of the tag associated with the release.
#'   - **name**: The name, or title, of the release.
#'   - **body**: The body, or description, of the release.
#'   - **author_login**: The author of the release.
#'   - **draft**: Whether the release is a draft.
#'   - **prerelease**: Whether the release is a pre-release.
#'   - **target_commitish**: The commit or branch the release was applied to.
#'   - **created_at**: The date and time of creation.
#'   - **published_at**: The date and time of publication.
#'   - **assets**: The names of any assets associated with the release.
#'   - **zipball_url**: The URL to download a zip file of the release commit.
#'   - **url**: The URL to view the release details on GitHub.
#'
#' @export
#'
update_releases <- function(
  tags,
  names,
  bodies,
  repo,
  draft      = FALSE,
  prerelease = FALSE,
  token      = getOption("github.token"),
  api        = getOption("github.api"),
  ...)
{
  .Deprecated("update_release", package = "githapi")

  (is_character(tags)) ||
    error("'tags' must be a character vector:\n  '", paste(tags, collapse = "'\n  '"), "'")
  (is_character(names) && identical(length(names), length(tags))) ||
    error("'names' must be a character vector of the same length as 'tags':\n  'names':  ", length(names), "\n  'tags':  ", length(tags))
  (is_character(bodies) && identical(length(bodies), length(tags))) ||
    error("'bodies' must be a character vector of the same length as 'tags':\n  'bodies':  ", length(bodies), "\n  'tags':   ", length(tags))
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_logical(draft) && (is_scalar_atomic(draft) || identical(length(draft), length(tags)))) ||
    error("'draft' must be a string or a character vector of the same length as 'tags':\n  'draft':  ", length(draft), "\n  'tags':  ", length(tags))
  (is_logical(prerelease) && (is_scalar_atomic(prerelease) || identical(length(prerelease), length(tags)))) ||
    error("'prerelease' must be a string or a character vector of the same length as 'tags':\n  'prerelease':  ", length(prerelease), "\n  'tags':       ", length(tags))
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  params <- tibble(
    tag        = tags,
    name       = names,
    body       = bodies,
    draft      = draft,
    prerelease = prerelease)

  releases_list <- try_pmap(params, function(tag, name, body, draft, prerelease) {
    info("Updating release '", tag, "' to repository '", repo, "'")

    release <- gh_request(
      "GET", url = gh_url("repos", repo, "releases/tags", tag, api = api),
      token = token, ...)

    payload <- list(
      tag_name         = tag,
      name             = name,
      body             = body,
      draft            = draft,
      prerelease       = prerelease) %>%
      remove_missing()

    gh_request(
      "PATCH", url = gh_url("repos", repo, "releases", release$id, api = api),
      payload = payload, token = token, ...)
  })

  info("Transforming results", level = 3)
  releases_tbl <- bind_fields(releases_list, list(
    id               = c("id",               as = "integer"),
    tag_name         = c("tag_name",         as = "character"),
    name             = c("name",             as = "character"),
    body             = c("body",             as = "character"),
    author_login     = c("author", "login",  as = "character"),
    draft            = c("draft",            as = "logical"),
    prerelease       = c("prerelease",       as = "logical"),
    target_commitish = c("target_commitish", as = "character"),
    created_at       = c("created_at",       as = "datetime"),
    published_at     = c("published_at",     as = "datetime"),
    assets           = "",
    zipball_url      = c("zipball_url",      as = "character"),
    url              = c("url",              as = "character"))) %>%
    mutate(assets = gh_map(releases_list, use_names = FALSE, list_fields, c("assets"), "name"))

  info("Done", level = 3)
  releases_tbl
}

#  FUNCTION: delete_releases ----------------------------------------------------------------------
#
#' Delete releases.
#'
#' This function deletes the specified releases. It also deletes the tags associated with the
#' releases from the repository.
#'
#' <https://developer.github.com/v3/repos/releases/#delete-a-release>
#'
#' @param tags (character) The tag names associated with the releases.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named list containing `TRUE` if the release was deleted. An error is thrown otherwise.
#'
#' @export
#'
delete_releases <- function(
  tags,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("delete_release", package = "githapi")

  (is_character(tags)) ||
    error("'tags' must be a character vector:\n  '", paste(tags, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  releases_list <- try_map(tags, function(tag) {
    info("Deleting release '", tag, "' from repository '", repo, "'")

    release <- gh_request(
      "GET", url = gh_url("repos", repo, "releases/tags", tag, api = api),
      token = token, ...)

    gh_request(
      "DELETE", url = gh_url("repos", repo, "releases", release$id, api = api),
      token = token, ...)

    gh_request(
      "DELETE", url = gh_url("repos", repo, "git/refs/tags", tag, api = api),
      token = token, ...)

    TRUE
  })

  info("Done", level = 3)
  releases_list
}

#  FUNCTION: releases_exist -------------------------------------------------------------------
#
#' Determine whether releases exist in the specified repository.
#'
#' This function returns `TRUE` if the release exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name>
#'
#' @param tags (character) The names of the release tags.
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
releases_exist <- function(
  tags,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(msg = "This function will be removed in a future version")

  (is_character(tags)) ||
    error("'tags' must be a character vector:\n  '", paste(tags, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  gh_map(tags, simplify = TRUE, function(tag) {
    info("Checking release '", tag, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "releases/tags", tag, api = api),
        token = token, ...)
      TRUE
    }, on_error = function(e) {
      FALSE
    })
  })
}
