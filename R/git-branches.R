#  FUNCTION: view_branches --------------------------------------------------------------------
#
#' View information about branches
#'
#' This function returns details about the branches for the specified repository in GitHub.
#' If no branches are supplied then details about all the branches are returned. If branches
#' are supplied then only information about those ones is returned.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' <https://developer.github.com/v3/git/refs/#get-all-references>
#'
#' @param branches (character, optional) The branch names. If missing or `NULL` all the
#' @param repo (string) The repository specified in the format: `owner/repo`.
#'   branches are requested.
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the branches, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the branch.
#'   - **ref**: The full git reference for the branch.
#'   - **url**: The URL to get the branch details from GitHub.
#'   - **object_sha**: The SHA of the object at the HEAD of the branch.
#'   - **object_type**: The type of the object at the HEAD of the branch.
#'   - **object_url**: The URL to get the object details from GitHub.
#'
#' @export
#'
view_branches <- function(
  branches,
  repo,
  n_max = 1000L,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    if (missing(repo)) {
      info("'repo' is missing, so using 'branches' argument: ", branches, level = 2)
      repo <- branches
      branches <- NULL
    }

    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_scalar_integerish(n_max) && isTRUE(n_max > 0)) ||
      error("'n_max' must be a positive integer:\n  '", paste(n_max, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  if (missing(branches) || is_null(branches) || all(is_na(branches))) {
    info("Getting up to ", n_max, " branches from repository '", repo, "'")

    branches_list <- try_catch({
      gh_page(
        gh_url("repos", repo, "git/refs/heads", api = api),
        n_max = n_max, token = token, ...)
    })
  } else {
    (is_character(branches)) ||
      error("'branches' must be a character vector:\n  '", paste(branches, collapse = "'\n  '"), "'")

    branches_list <- try_map(branches, function(branch) {
      info("Getting branch '", branch, "' from repository '", repo, "'")

      gh_request(
        "GET", url = gh_url("repos", repo, "git/refs/heads", branch, api = api),
        token = token, ...)
    })
  }

  info("Transforming results", level = 3)
  branches_tbl <- bind_fields(branches_list[!gh_map(branches_list, is_null, simplify = TRUE)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 3)
  branches_tbl
}

#  FUNCTION: create_branches ------------------------------------------------------------------
#
#' Create branches at specified commits.
#'
#' This function creates the specified branches at the specified commits within a repository in
#' GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param branches (character) The branch names.
#' @param shas (character) The SHAs of the commits to create a branch at.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the branches, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the branch.
#'   - **ref**: The full git reference for the branch.
#'   - **url**: The URL to get the branch details from GitHub.
#'   - **object_sha**: The SHA of the object at the HEAD of the branch.
#'   - **object_type**: The type of the object at the HEAD of the branch.
#'   - **object_url**: The URL to get the object details from GitHub.
#'
#' @export
#'
create_branches <- function(
  branches,
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(branches)) ||
      error("'branches' must be a character vector:\n  '", paste(branches, collapse = "'\n  '"), "'")
    (is_character(shas) && all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (identical(length(branches), length(shas))) ||
      error("'branches' and 'shas' must have the same length:\n  'branches': ", length(branches), "\n  'shas':     ", length(shas))
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  branches_list <- try_pmap(list(branches, shas), function(branch, sha) {
    info("Posting branch '", branch, "' to repository '", repo, "'")

    gh_request(
      "POST", url = gh_url("repos", repo, "git/refs", api = api),
      payload = list(ref = paste0("refs/heads/", branch), sha = sha),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  branches_tbl <- bind_fields(branches_list[!gh_map(branches_list, is_null, simplify = TRUE)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 3)
  branches_tbl
}

#  FUNCTION: update_branches ------------------------------------------------------------------
#
#' Update branches to new commits.
#'
#' This function updates the specified branches to point at new commits within a repository in
#' GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#update-a-reference>
#'
#' @param branches (character) The branch names.
#' @param shas (character) The SHAs of the commits to update the branch to.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the branches, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/git/refs/) for more details):
#'   - **name**: The name of the branch.
#'   - **ref**: The full git reference for the branch.
#'   - **url**: The URL to get the branch details from GitHub.
#'   - **object_sha**: The SHA of the object at the HEAD of the branch.
#'   - **object_type**: The type of the object at the HEAD of the branch.
#'   - **object_url**: The URL to get the object details from GitHub.
#'
#' @export
#'
update_branches <- function(
  branches,
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(branches)) ||
      error("'branches' must be a character vector:\n  '", paste(branches, collapse = "'\n  '"), "'")
    (is_character(shas) && all(gh_map(shas, is_sha, simplify = TRUE))) ||
      error("'shas' must a vector of 40 character strings:\n  '", paste(shas, collapse = "'\n  '"), "'")
    (identical(length(branches), length(shas))) ||
      error("'branches' and 'shas' must have the same length:\n  'branches': ", length(branches), "\n  'shas':     ", length(shas))
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  branches_list <- try_pmap(list(branches, shas), function(branch, sha) {
    info("Updating branch '", branch, "' in repository '", repo, "'")

    gh_request(
      "PATCH", url = gh_url("repos", repo, "git/refs/heads", branch, api = api),
      payload = list(sha = sha, force = TRUE),
      token = token, ...)
  })

  info("Transforming results", level = 3)
  branches_tbl <- bind_fields(branches_list[!gh_map(branches_list, is_null, simplify = TRUE)], list(
    name        = "",
    ref         = c("ref",            as = "character"),
    url         = c("url",            as = "character"),
    object_sha  = c("object", "sha",  as = "character"),
    object_type = c("object", "type", as = "character"),
    object_url  = c("object", "url",  as = "character"))) %>%
    mutate(name = basename(.data$ref))

  info("Done", level = 3)
  branches_tbl
}

#  FUNCTION: delete_branches ----------------------------------------------------------------------
#
#' Delete branches.
#'
#' This function deletes the specified branches from a repository in GitHub.
#'
#' <https://developer.github.com/v3/git/refs/#delete-a-reference>
#'
#' @param branches (character) The branch names.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named list containing `TRUE` if the branch was deleted. An error is thrown otherwise.
#'
#' @export
#'
delete_branches <- function(
  branches,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(branches)) ||
      error("'branches' must be a character vector:\n  '", paste(branches, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  branches_list <- try_map(branches, function(branch) {
    info("Deleting branch '", branch, "' from repository '", repo, "'")

    gh_request(
      "DELETE", url = gh_url("repos", repo, "git/refs/heads", branch, api = api),
      token = token, ...)
    TRUE
  })

  info("Done", level = 3)
  branches_list
}

#  FUNCTION: branches_exist -------------------------------------------------------------------
#
#' Determine whether branches exist in the specified repository.
#'
#' This function returns `TRUE` if the branch exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param branches (character) The name of the branch.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each branch specified.
#'
#' @export
#'
branches_exist <- function(
  branches,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  {
    (is_character(branches)) ||
      error("'branches' must be a character vector:\n  '", paste(branches, collapse = "'\n  '"), "'")
    (is_repo(repo)) ||
      error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
    (is_url(api)) ||
      error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")
  }

  gh_map(branches, simplify = TRUE, function(branch) {
    info("Checking branch '", branch, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "git/refs/heads", branch, api = api),
        token = token, ...)
      TRUE
    },
    on_error = function(e) {
      FALSE
    })
  })
}
