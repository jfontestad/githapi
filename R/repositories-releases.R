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

  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(tags) || is_null(tags) || is_na(tags)) {
    info("Getting up to ", n_max, " releases from repository '", repo, "'")
    releases_list <- tryCatch({
      gh_page(
        gh_url("repos", repo, "releases", api = api),
        n_max = n_max, token = token, ...)
    }, error = function(e) {
      warn("Failed!", level = 2)
      list(e)
    })
  } else if (identical(tags, "latest")) {
    info("Getting latest release from repository '", repo, "'")
    releases_list <- list(tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "releases/latest", api = api),
        token = token, ...)
    }, error = function(e) {
      warn("Release 'latest' failed!", level = 2)
      e
    }))
  } else {
    assert(is_character(tags))
    releases_list <- map(tags, function(tag) {
      info("Getting release '", tag, "' from repository '", repo, "'")
      tryCatch({
        gh_request(
          "GET", gh_url("repos", repo, "releases/tags", tag, api = api),
          token = token, ...)
      }, error = function(e) {
        warn("Release '", tag, "' failed!", level = 2)
        e
      })
    })
  }

  if (any(map_vec(releases_list, is, "error"))) {
    collate_errors(releases_list, "view_releases() failed!")
  }

  info("Transforming results", level = 2)
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
    mutate(assets = map(releases_list, use_names = FALSE, function(r) {
      if (is_null(r$assets) || identical(length(r$assets), 0L)) {
        NULL
      } else {
        map_vec(r$assets, getElement, "name")
      }
    }))

  info("Done", level = 2)
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
  assert(is_character(tags))
  assert(is_character(commits) && identical(length(commits), length(tags)))
  assert(is_character(names) && identical(length(names), length(tags)))
  assert(is_character(bodies) && identical(length(bodies), length(tags)))
  assert(is_repo(repo))
  assert(is_logical(draft) && (is_scalar(draft) || identical(length(draft), length(tags))))
  assert(is_logical(prerelease) && (is_scalar(prerelease) || identical(length(prerelease), length(tags))))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    tag        = tags,
    commit     = commits,
    name       = names,
    body       = bodies,
    draft      = draft,
    prerelease = prerelease)

  releases_list <- pmap(params, function(tag, commit, name, body, draft, prerelease) {
    info("Posting release '", tag, "' to repository '", repo, "'")
    tryCatch({
      gh_request(
        "POST", gh_url("repos", repo, "releases", api = api),
        payload = list(
          tag_name         = tag,
          target_commitish = commit,
          name             = name,
          body             = body,
          draft            = draft,
          prerelease       = prerelease),
        token = token, ...)
    }, error = function(e) {
      warn("Tag '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(releases_list, is, "error"))) {
    collate_errors(releases_list, "create_releases() failed!")
  }

  info("Transforming results", level = 2)
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
    mutate(assets = map(releases_list, use_names = FALSE, function(r) {
      if (is_null(r$assets) || identical(length(r$assets), 0L)) {
        NULL
      } else {
        map_vec(r$assets, getElement, "name")
      }
    }))

  info("Done", level = 2)
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
  assert(is_character(tags))
  assert(is_na(names) || (is_character(names) && identical(length(names), length(tags))))
  assert(is_na(bodies) || (is_character(bodies) && identical(length(bodies), length(tags))))
  assert(is_repo(repo))
  assert(is_logical(draft) && (is_scalar(draft) || identical(length(draft), length(tags))))
  assert(is_logical(prerelease) && (is_scalar(prerelease) || identical(length(prerelease), length(tags))))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    tag        = tags,
    name       = names,
    body       = bodies,
    draft      = draft,
    prerelease = prerelease)

  releases_list <- pmap(params, function(tag, name, body, draft, prerelease) {
    info("Updating release '", tag, "' to repository '", repo, "'")
    tryCatch({
      release <- gh_request(
        "GET", gh_url("repos", repo, "releases/tags", tag, api = api),
        token = token, ...)

      payload <- list(
        tag_name         = tag,
        name             = name,
        body             = body,
        draft            = draft,
        prerelease       = prerelease) %>%
        remove_missing()

      gh_request(
        "PATCH", gh_url("repos", repo, "releases", release$id, api = api),
        payload = payload, token = token, ...)
    }, error = function(e) {
      warn("Tag '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(releases_list, is, "error"))) {
    collate_errors(releases_list, "update_releases() failed!")
  }

  info("Transforming results", level = 2)
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
    mutate(assets = map(releases_list, use_names = FALSE, function(r) {
      if (is_null(r$assets) || identical(length(r$assets), 0L)) {
        NULL
      } else {
        map_vec(r$assets, getElement, "name")
      }
    }))

  info("Done", level = 2)
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
  assert(is_character(tags))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  releases_list <- map(tags, function(tag) {
    info("Deleting release '", tag, "' from repository '", repo, "'")
    tryCatch({
      release <- gh_request(
        "GET", gh_url("repos", repo, "releases/tags", tag, api = api),
        token = token, ...)

      gh_request(
        "DELETE", gh_url("repos", repo, "releases", release$id, api = api),
        token = token, parse = FALSE, ...)

      gh_request(
        "DELETE", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        token = token, parse = FALSE, ...)

      TRUE
    }, error = function(e) {
      warn("Release '", tag, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(releases_list, is, "error"))) {
    collate_errors(releases_list, "delete_releases() failed!")
  }

  info("Done", level = 2)
  releases_list
}
