#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
#
#' Get a blob
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param sha (string) SHA-1 of the blob
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A string containing the contents of the specified file (see GitHub's API
#'   documentation for details).
#'
#' @export
#'
gh_git_blob <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  blob <- gh_get(
    gh_url("repos", repo, "git/blobs", sha, api = api),
    accept = "raw", token = token, ...)

  attr(blob, "header") <- NULL
  blob
}

#  FUNCTION: gh_git_commit --------------------------------------------------------------------
#
#' Get a commit
#'
#' <https://developer.github.com/v3/git/commits/#get-a-commit>
#'
#' @param sha (string) The SHA-1 of the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the commit (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_commit <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/commits", sha, api = api),
    token = token, ...)
}

#  FUNCTION: gh_git_reference -----------------------------------------------------------------
#
#' Get a reference
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param ref (string) tags, specified as `tags/<tag_name>`, or branches, specified as
#'   `heads/<branch_name>`
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the reference (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_reference <- function(
  ref,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/refs", ref, api = api),
    token = token, ...)
}

#  FUNCTION: is_tag ---------------------------------------------------------------------------
#
#' Check whether the input is a valid tag
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param tag (string) tag name.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A boolean, with attributes describing the errors, if there are any.
#'
#' @export
#'
is_tag <- function(
  tag,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  if (!is_string(tag)) {
    return(FALSE)
  }

  result <- try(
    gh_git_reference(ref = paste0("tags/", tag), repo = repo, token = token, api = api, ...),
    silent = TRUE)

  if (is(result, "try-error")) {
    return(FALSE)
  }

  TRUE
}

#  FUNCTION: gh_git_references ----------------------------------------------------------------
#
#' Get all references
#'
#' <https://developer.github.com/v3/git/refs/#get-all-references>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return A tibble describing the references (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_references <- function(
  repo,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  ref_map <- c(
    "refs/heads" = "branch",
    "refs/tags"  = "tag",
    "head"       = "pull request",
    "merge"      = "pull merge")

  references <- gh_page(
    gh_url("repos", repo, "git/refs", api = api),
    n_max = n_max, token = token, ...)

  bind_fields(references, list(
    name        = "",
    type        = "",
    object_type = c("object", "type", as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"))) %>%
    mutate(
      name = ifelse(grepl("pull", .data$ref), basename(dirname(.data$ref)), basename(.data$ref)),
      type = ifelse(grepl("pull", .data$ref), ref_map[basename(.data$ref)], ref_map[dirname(.data$ref)]))
}

#  FUNCTION: gh_git_tag -----------------------------------------------------------------------
#
#' Get an annotated tag
#'
#' <https://developer.github.com/v3/git/tags/#get-a-tag>
#'
#' @param sha (string) The SHA-1 of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list describing the tag (see GitHub's API documentation for details).
#'
#' @export
#'
gh_git_tag <- function(
  sha,
  repo,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_sha(sha))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  gh_get(
    gh_url("repos", repo, "git/tags", sha, api = api),
    token = token, ...)
}

#  FUNCTION: gh_git_tree ----------------------------------------------------------------------
#
#' Get a tree
#'
#' <https://developer.github.com/v3/git/trees/#get-a-tree>
#'
#' @param ref (string) A git reference: either a SHA-1, tag or branch. If a branch is specified
#'   the head commit is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param recursive (logical) Whether to list files recursively. Default: TRUE.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A tibble describing the files in a commit (see GitHub's API documentation for
#'   details).
#'
#' @export
#'
gh_git_tree <- function(
  ref,
  repo,
  recursive = TRUE,
  token     = gh_token(),
  api       = getOption("github.api"),
  ...)
{
  assert(is_string(ref))
  assert(is_repo(repo))
  assert(is_boolean(recursive))
  assert(is_sha(token))
  assert(is_url(api))

  tree <- gh_get(
    gh_url("repos", repo, "git/trees", ref, recursive = as.integer(recursive), api = api),
    token = token, ...)

  bind_fields(tree$tree, list(
    path = c("path", as = "character"),
    type = c("type", as = "character"),
    sha  = c("sha",  as = "character"),
    size = c("size", as = "integer"),
    url  = c("url",  as = "character")))
}

#  FUNCTION: gh_save --------------------------------------------------------------------------
#
#' Download files and save them to a location
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param files (string) The paths to the files in the repository.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string) The location to save the files to.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_git_tree()].
#'
#' @return The file path of the saved file (invisibly).
#'
#' @export
#'
gh_save <- function(
  files,
  repo,
  path,
  ref   = "master",
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(files))
  assert(is_repo(repo))
  assert(is_string(path))
  assert(is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  repo_files <- gh_git_tree(ref = ref, repo = repo, recursive = TRUE, token = token, api = api, ...)

  if (all(files %in% repo_files$path)) {
    blob_paths <- repo_files[repo_files$path %in% files,][["path"]]
    names(blob_paths) <- repo_files[repo_files$path %in% files,][["sha"]]
  } else {
    error(
      "Cannot find specified files in repo '", repo, "':\n\n  ",
      paste(files, collapse = "\n  "))
  }

  if (!dir.exists(path)) dir.create(path)

  for (blob in names(blob_paths)) {
    gh_download_binary(
      gh_url("repos", repo, "git/blobs", blob, api = api),
      path = file.path(path, basename(blob_paths[[blob]])), token = token)
  }

  invisible(path)
}

#  FUNCTION: gh_source ------------------------------------------------------------------------
#
#' Source an R file
#'
#' <https://developer.github.com/v3/git/blobs/#get-a-blob>
#'
#' @param file (string) The path to the file in the repository
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. Default: "master".
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [base::source()].
#'
#' @return The result of sourcing the file.
#'
#' @export
#'
gh_source <- function(
  file,
  repo,
  ref   = "master",
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(file))
  assert(is_repo(repo))
  assert(is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  path <- file.path(tempdir(), "gh_source")
  on.exit(unlink(path, recursive = TRUE))

  gh_save(files = file, repo = repo, path = path, ref = ref, token = token, api = api)
  source(file.path(path, basename(file)), ...)
}

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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param tags (character, optional) The tag names. If missing or `NULL` all the tags are
#'   requested.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
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
  repo,
  tags,
  n_max = 1000L,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_count(n_max))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(tags) || is_null(tags)) {
    info("Getting up to ", n_max, " tags from GitHub")
    tags_list <- tryCatch({
      gh_page(
        gh_url("repos", repo, "git/refs/tags", api = api),
        n_max = n_max, token = token, ...)
    }, error = function(e) {
      info("Failed!")
      list(e)
    })
  } else {
    assert(is_character(tags))
    info("Getting tags '", paste(tags, collapse = "', '"), "' from GitHub")
    tags_list <- sapply(tags, simplify = FALSE, USE.NAMES = TRUE, function(tag) {
      tryCatch({
        gh_request(
          "GET", gh_url("repos", repo, "git/refs/tags", tag, api = api),
          token = token, ...)
      }, error = function(e) {
        info("Tag '", tag, "' failed!")
        e
      })
    })
  }

  if (any(sapply(tags_list, is, "error"))) {
    collate_errors(tags_list, "view_tags() failed!")
  }

  info("Transforming results")
  tags_tbl <- bind_fields(tags_list[!sapply(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done")
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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param tags (character) The tag names.
#' @param shas (character) The SHAs of the commits to tag.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
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
  repo,
  tags,
  shas,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(tags))
  assert(is_character(shas))
  assert(is_sha(token))
  assert(is_url(api))

  info("Posting tags '", paste(tags, collapse = "', '"), "' to GitHub")
  tags_list <- mapply(tags, shas, USE.NAMES = TRUE, SIMPLIFY = FALSE, FUN = function(tag, sha) {
    tryCatch({
      gh_request(
        "POST", gh_url("repos", repo, "git/refs", api = api),
        payload = list(ref = paste0("refs/tags/", tag), sha = sha),
        token = token, ...)
    }, error = function(e) {
      info("Tag '", tag, "' failed!")
      e
    })
  })

  if (any(sapply(tags_list, is, "error"))) {
    collate_errors(tags_list, "create_tags() failed!")
  }

  info("Transforming results")
  tags_tbl <- bind_fields(tags_list[!sapply(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done")
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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param tags (character) The tag names.
#' @param shas (character) Ths SHAs of the commits to tag.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
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
  repo,
  tags,
  shas,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(tags))
  assert(is_character(shas))
  assert(is_sha(token))
  assert(is_url(api))

  info("Patching tags '", paste(tags, collapse = "', '"), "' in GitHub")
  tags_list <- mapply(tags, shas, USE.NAMES = TRUE, SIMPLIFY = FALSE, FUN = function(tag, sha) {
    tryCatch({
      gh_request(
        "PATCH", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        payload = list(sha = sha, force = TRUE),
        token = token, ...)
    }, error = function(e) {
      info("Tag '", tag, "' failed!")
      e
    })
  })

  if (any(sapply(tags_list, is, "error"))) {
    collate_errors(tags_list, "update_tags() failed!")
  }

  info("Transforming results")
  tags_tbl <- bind_fields(tags_list[!sapply(tags_list, is_null)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done")
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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param tags (character) The tag names.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named list containing `TRUE` if the tag was deleted. An error is thrown otherwise.
#'
#' @export
#'
delete_tags <- function(
  repo,
  tags,
  token = gh_token(),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(tags))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  info("Deleting tags '", paste(tags, collapse = "', '"), "' from GitHub")
  tags_list <- sapply(tags, simplify = FALSE, USE.NAMES = TRUE, function(tag) {
    tryCatch({
      gh_request(
        "DELETE", gh_url("repos", repo, "git/refs/tags", tag, api = api),
        token = token, parse = FALSE, ...)
      TRUE
    }, error = function(e) {
      info(e$message)
      e
    })
  })

  if (any(sapply(tags_list, is, "error"))) {
    collate_errors(tags_list, "delete_tags() failed!")
    tags_list[sapply(tags_list, is, "error")] <- FALSE
  }

  info("Done")
  tags_list
}
