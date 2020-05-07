#  FUNCTION: view_files -----------------------------------------------------------------------
#
#' View information about a files
#'
#' This function returns details about the files in GitHub for the specified repository.
#' If no files are supplied then all the files in the reference commit are returned. If files
#' are supplied then only information about those files is returned. Finally, if a directory
#' is specified then details about all the files within the directory is returned.
#'
#' NOTE: This function is not recursive. If you want to view information about all the files
#' in a repository use [view_trees()].
#'
#' <https://developer.github.com/v3/repos/contents/#get-contents>
#'
#' @param paths (string, optional) The paths to the files or directories in the repository.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If not specified the head of the default branch is used.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the files, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/contents/)
#'   for more details):
#'   - **name**: The name of the file.
#'   - **path**: The path to the file.
#'   - **sha**: The SHA of the file.
#'   - **size**: The size of the file in bytes.
#'   - **type**: Either `"file"`, `"dir"`, `"symlink"` or `"submodule"`.
#'   - **url**: The URL to the file contents.
#'   - **html_url**: The URL to view the file in GitHub.
#'   - **git_url**: The URL to the Git blob.
#'   - **download_url**: The URL to download the file.
#'
#' @export
#'
view_files <- function(
  paths,
  repo,
  ref,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  if (missing(repo)) {
    info("'repo' is missing, so using 'paths' argument: ", paths, level = 2)
    repo <- paths
    paths <- NULL
  }
  if (missing(ref) || is_null(ref)) {
    ref <- NULL
  }

  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_null(ref) || is_scalar_character(ref)) ||
    error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  if (missing(paths) || is_null(paths)) {
    info("Getting files from repository '", repo, "'")

    files_list <- try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "contents", ref = ref, api = api),
        token = token, ...)
    })
  } else {
    .Deprecated(msg = "Specifying paths within view_files() has been deprecated. Please use view_file() instead.")

    (is_character(paths)) ||
      error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")

    files_list <- try_map(paths, function(path) {
      info("Getting file '", path, "' from repository '", repo, "'")

      gh_request(
        "GET", url = gh_url("repos", repo, "contents", path, ref = ref, api = api),
        token = token, ...)
    })
  }

  info("Transforming results", level = 3)
  files_tbl <- bind_fields(files_list, list(
    name         = c("name",         as = "character"),
    path         = c("path",         as = "character"),
    sha          = c("sha",          as = "character"),
    size         = c("size",         as = "integer"),
    type         = c("type",         as = "character"),
    url          = c("url",          as = "character"),
    html_url     = c("html_url",     as = "character"),
    git_url      = c("git_url",      as = "character"),
    download_url = c("download_url", as = "character")))

  info("Done", level = 3)
  files_tbl
}

#  FUNCTION: create_files ---------------------------------------------------------------------
#
#' Create a new files
#'
#' This function creates a new text files in a GitHub repository. It adds a new commit for
#' each file created with the specified commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#create-a-file>
#'
#' @param paths (character) The paths to the files in the repository.
#' @param contents (character) The text to add to the files.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If not specified the default branch for the repository
#'   is used.
#' @param parents (character, optional) SHAs, branches or tags of the commits to use as parents if
#'   creating a new branch. Not required if making a new commit on an existing branch. A character
#'   vector can be supplied corresponding to a different parent for each file, or a string can be
#'   provided if the same parent is used for all files.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the files, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/contents/)
#'   for more details):
#'   - **name**: The name of the file.
#'   - **path**: The path to the file.
#'   - **sha**: The SHA of the file.
#'   - **size**: The size of the file in bytes.
#'   - **type**: Either `"file"`, `"dir"`, `"symlink"` or `"submodule"`.
#'   - **url**: The URL to the file contents.
#'   - **html_url**: The URL to view the file in GitHub.
#'   - **git_url**: The URL to the Git blob.
#'   - **download_url**: The URL to download the file.
#'   - **commit_message**: The commit message recorded in GitHub.
#'   - **commit_sha**: The SHA of the commit.
#'   - **commit_url**: The URL to the commit.
#'   - **commit_author**: The author of the commit.
#'   - **commit_committer**: The committer of the commit.
#'   - **commit_tree_sha**: The SHA for the git tree.
#'   - **commit_tree_url**: The URL for the git tree.
#'   - **commit_parent_sha**: The SHA of the parent commit.
#'   - **commit_parent_url**: The URL to the parent commit.
#'
#' @export
#'
create_files <- function(
  paths,
  contents,
  messages,
  branches,
  parents,
  committer,
  author,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("create_file", package = "githapi")

  if (missing(branches) || is_null(branches)) {
    branches <- NA
  }
  if (missing(parents) || is_null(parents)) {
    parents <- NA
  }
  if (missing(committer) || is_null(committer)) {
    committer <- NA
  }
  if (missing(author) || is_null(author)) {
    author <- NA
  }

  (is_character(paths)) ||
    error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
  (is_character(contents) && identical(length(contents), length(paths))) ||
    error("'contents' must be a character vector of the same length as 'paths':\n  'contents':  ", length(contents), "\n  'paths':    ", length(paths))
  (is_character(messages) && (is_scalar_atomic(messages) || identical(length(messages), length(paths)))) ||
    error("'messages' must be a character vector of the same length as 'paths':\n  'messages':  ", length(messages), "\n  'paths':    ", length(paths))
  ((all(is_na(branches)) || is_character(branches)) && (is_scalar_atomic(branches) || identical(length(branches), length(paths)))) ||
    error("'branches' must be NA, a string or a character vector of the same length as paths:\n  'branches':  ", length(branches), "\n  'paths':    ", length(paths))
  (all(is_na(parents)) || is_character(parents)) ||
    error("'parents' must be NA or a character vector:\n  '", paste(parents, collapse = "'\n  '"), "'")
  (all(is_na(committer)) || (is_list(committer) && identical(names(committer), c("name", "email")) && is_scalar_character(committer$name) && is_scalar_character(committer$email))) ||
    error("'committer' must be NA or a list containing 'name' and 'email':\n '", paste(committer, collapse = "'\n  '"), "'")
  (all(is_na(author)) || (is_list(author) && identical(names(author), c("name", "email")) && is_scalar_character(author$name) && is_scalar_character(author$email))) ||
    error("'author' must be NA or a list containing 'name' and 'email':\n '", paste(author, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  params <- tibble(
    path    = paths,
    content = contents,
    message = messages,
    branch  = branches,
    parent  = parents)

  files_list <- try_pmap(params, function(path, content, message, branch, parent) {
    if (!is_na(branch) && !branches_exist(branches = branch, repo = repo, token = token, api = api)) {
      (!is_na(parent)) ||
        error("Specified branch '", branch, "' does not exist. To create it a parent commit must be specified!")

      if (!is_sha(parent)) {
        parent <- suppressWarnings(view_shas(refs = parent, repo = repo, token = token, api = api))
      }

      suppressWarnings(create_branches(branches = branch, shas = parent, repo = repo, token = token, api = api))
    }

    payload <- list(
      message   = message,
      content   = jsonlite::base64_enc(content),
      branch    = branch,
      committer = committer,
      author    = author) %>%
      remove_missing()

    info("Posting file '", basename(path), "' to repository '", repo, "'")
    gh_request(
      "PUT", url = gh_url("repos", repo, "contents", path, api = api),
      payload = payload, token = token, ...)
  })

  info("Transforming results", level = 3)
  files_tbl <- bind_fields(files_list, list(
    name              = c("content", "name",              as = "character"),
    path              = c("content", "path",              as = "character"),
    sha               = c("content", "sha",               as = "character"),
    size              = c("content", "size",              as = "integer"),
    type              = c("content", "type",              as = "character"),
    url               = c("content", "url",               as = "character"),
    html_url          = c("content", "html_url",          as = "character"),
    git_url           = c("content", "git_url",           as = "character"),
    download_url      = c("content", "download_url",      as = "character"),
    commit_message    = c("commit", "message",            as = "character"),
    commit_sha        = c("commit",  "sha",               as = "character"),
    commit_url        = c("commit",  "url",               as = "character"),
    commit_author     = c("commit",  "author", "name",    as = "character"),
    commit_committer  = c("commit",  "committer", "name", as = "character"),
    commit_tree_sha   = c("commit",  "tree", "sha",       as = "character"),
    commit_tree_url   = c("commit",  "tree", "url",       as = "character"),
    commit_parent_sha = "",
    commit_parent_url = "")) %>%
    mutate(commit_parent_sha = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "sha")) %>%
    mutate(commit_parent_url = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "url"))

  info("Done", level = 3)
  files_tbl
}

#  FUNCTION: update_files ---------------------------------------------------------------------
#
#' Update existing files
#'
#' This function updates existing text files in a GitHub repository. It adds a new commit for
#' each file updated with the specified commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#update-a-file>
#'
#' @param paths (character) The paths to the files in the repository.
#' @param contents (character) The text to add to the files.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If not specified the default branch for the repository
#'   is used.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the files, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/contents/)
#'   for more details):
#'   - **name**: The name of the file.
#'   - **path**: The path to the file.
#'   - **sha**: The SHA of the file.
#'   - **size**: The size of the file in bytes.
#'   - **type**: Either `"file"`, `"dir"`, `"symlink"` or `"submodule"`.
#'   - **url**: The URL to the file contents.
#'   - **html_url**: The URL to view the file in GitHub.
#'   - **git_url**: The URL to the Git blob.
#'   - **download_url**: The URL to download the file.
#'   - **commit_message**: The commit message recorded in GitHub.
#'   - **commit_sha**: The SHA of the commit.
#'   - **commit_url**: The URL to the commit.
#'   - **commit_author**: The author of the commit.
#'   - **commit_committer**: The committer of the commit.
#'   - **commit_tree_sha**: The SHA for the git tree.
#'   - **commit_tree_url**: The URL for the git tree.
#'   - **commit_parent_sha**: The SHA of the parent commit.
#'   - **commit_parent_url**: The URL to the parent commit.
#'
#' @export
#'
update_files <- function(
  paths,
  contents,
  messages,
  branches,
  committer,
  author,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("update_file", package = "githapi")

  if (missing(branches) || is_null(branches)) {
    branches <- NA
  }
  if (missing(committer) || is_null(committer)) {
    committer <- NA
  }
  if (missing(author) || is_null(author)) {
    author <- NA
  }

  (is_character(paths)) ||
    error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
  (is_character(contents) && identical(length(contents), length(paths))) ||
    error("'contents' must be a character vector of the same length as 'paths':\n  'contents':  ", length(contents), "\n  'paths':    ", length(paths))
  (is_character(messages) && (is_scalar_atomic(messages) || identical(length(messages), length(paths)))) ||
    error("'messages' must be a character vector of the same length as 'paths':\n  'messages':  ", length(messages), "\n  'paths':    ", length(paths))
  ((all(is_na(branches)) || is_character(branches)) && (is_scalar_atomic(branches) || identical(length(branches), length(paths)))) ||
    error("'branches' must be NA, a string or a character vector of the same length as paths:\n  'branches':  ", length(branches), "\n  'paths':    ", length(paths))
  (all(is_na(committer)) || (is_list(committer) && identical(names(committer), c("name", "email")) && is_scalar_character(committer$name) && is_scalar_character(committer$email))) ||
    error("'committer' must be NA or a list containing 'name' and 'email':\n '", paste(committer, collapse = "'\n  '"), "'")
  (all(is_na(author)) || (is_list(author) && identical(names(author), c("name", "email")) && is_scalar_character(author$name) && is_scalar_character(author$email))) ||
    error("'author' must be NA or a list containing 'name' and 'email':\n '", paste(author, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  params <- tibble(
    path    = paths,
    content = contents,
    message = messages,
    branch  = branches) %>%
    mutate(branch = ifelse(is_na(.data$branch), list(NULL), .data$branch))

  files_list <- try_pmap(params, function(path, content, message, branch) {
    info("Posting file '", basename(path), "' to repository '", repo, "'")

    old_file <- gh_request(
      "GET", url = gh_url("repos", repo, "contents", path, ref = branch, api = api),
      token = token, ...)

    payload <- list(
      message   = paste(message, "- updated", path),
      content   = jsonlite::base64_enc(content),
      sha       = old_file$sha,
      branch    = branch,
      committer = committer,
      author    = author) %>%
      remove_missing()

    gh_request(
      "PUT", url = gh_url("repos", repo, "contents", path, api = api),
      payload = payload, token = token, ...)
  })

  info("Transforming results", level = 3)
  files_tbl <- bind_fields(files_list, list(
    name              = c("content", "name",              as = "character"),
    path              = c("content", "path",              as = "character"),
    sha               = c("content", "sha",               as = "character"),
    size              = c("content", "size",              as = "integer"),
    type              = c("content", "type",              as = "character"),
    url               = c("content", "url",               as = "character"),
    html_url          = c("content", "html_url",          as = "character"),
    git_url           = c("content", "git_url",           as = "character"),
    download_url      = c("content", "download_url",      as = "character"),
    commit_message    = c("commit", "message",            as = "character"),
    commit_sha        = c("commit",  "sha",               as = "character"),
    commit_url        = c("commit",  "url",               as = "character"),
    commit_author     = c("commit",  "author", "name",    as = "character"),
    commit_committer  = c("commit",  "committer", "name", as = "character"),
    commit_tree_sha   = c("commit",  "tree", "sha",       as = "character"),
    commit_tree_url   = c("commit",  "tree", "url",       as = "character"),
    commit_parent_sha = "",
    commit_parent_url = "")) %>%
    mutate(commit_parent_sha = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "sha")) %>%
    mutate(commit_parent_url = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "url"))

  info("Done", level = 3)
  files_tbl
}

#  FUNCTION: delete_files ---------------------------------------------------------------------
#
#' Delete files.
#'
#' This function deletes the specified files from a repository in GitHub. It adds a new commit
#' for each file deleted with the specified commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#delete-a-file>
#'
#' @param paths (character) The paths to the files in the repository.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If not specified the default branch for the repository
#'   is used.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the files, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/contents/)
#'   for more details):
#'   - **commit_message**: The commit message recorded in GitHub.
#'   - **commit_sha**: The SHA of the commit.
#'   - **commit_url**: The URL to the commit.
#'   - **commit_author**: The author of the commit.
#'   - **commit_committer**: The committer of the commit.
#'   - **commit_tree_sha**: The SHA for the git tree.
#'   - **commit_tree_url**: The URL for the git tree.
#'   - **commit_parent_sha**: The SHA of the parent commit.
#'   - **commit_parent_url**: The URL to the parent commit.
#'
#' @export
#'
delete_files <- function(
  paths,
  messages,
  branches,
  committer,
  author,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated("delete_file", package = "githapi")

  if (missing(branches) || is_null(branches)) {
    branches <- NA
  }
  if (missing(committer) || is_null(committer)) {
    committer <- NA
  }
  if (missing(author) || is_null(author)) {
    author <- NA
  }

  (is_character(paths)) ||
    error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
  (is_character(messages) && (is_scalar_atomic(messages) || identical(length(messages), length(paths)))) ||
    error("'messages' must be a character vector of the same length as 'paths':\n  'messages':  ", length(messages), "\n  'paths':    ", length(paths))
  ((all(is_na(branches)) || is_character(branches)) && (is_scalar_atomic(branches) || identical(length(branches), length(paths)))) ||
    error("'branches' must be NA, a string or a character vector of the same length as paths:\n  'branches':  ", length(branches), "\n  'paths':    ", length(paths))
  (all(is_na(committer)) || (is_list(committer) && identical(names(committer), c("name", "email")) && is_scalar_character(committer$name) && is_scalar_character(committer$email))) ||
    error("'committer' must be NA or a list containing 'name' and 'email':\n '", paste(committer, collapse = "'\n  '"), "'")
  (all(is_na(author)) || (is_list(author) && identical(names(author), c("name", "email")) && is_scalar_character(author$name) && is_scalar_character(author$email))) ||
    error("'author' must be NA or a list containing 'name' and 'email':\n '", paste(author, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  params <- tibble(
    path    = paths,
    message = messages,
    branch  = branches) %>%
    mutate(branch = ifelse(is_na(.data$branch), list(NULL), .data$branch))

  files_list <- try_pmap(params, function(path, message, branch) {
    info("Deleting file '", basename(path), "' from repository '", repo, "'")

    old_file <- gh_request(
      "GET", url = gh_url("repos", repo, "contents", path, ref = branch, api = api),
      token = token, ...)

    payload <- list(
      message   = paste(message, "- deleted", path),
      sha       = old_file$sha,
      branch    = branch,
      committer = committer,
      author    = author) %>%
      remove_missing()

    gh_request(
      "DELETE", url = gh_url("repos", repo, "contents", path, api = api),
      payload = payload, token = token, ...)
  })

  info("Transforming results", level = 3)
  files_tbl <- bind_fields(files_list, list(
    commit_message    = c("commit", "message",            as = "character"),
    commit_sha        = c("commit",  "sha",               as = "character"),
    commit_url        = c("commit",  "url",               as = "character"),
    commit_author     = c("commit",  "author", "name",    as = "character"),
    commit_committer  = c("commit",  "committer", "name", as = "character"),
    commit_tree_sha   = c("commit",  "tree", "sha",       as = "character"),
    commit_tree_url   = c("commit",  "tree", "url",       as = "character"),
    commit_parent_sha = "",
    commit_parent_url = "")) %>%
    mutate(commit_parent_sha = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "sha")) %>%
    mutate(commit_parent_url = gh_map(files_list, use_names = FALSE, list_fields, c("commit", "parents"), "url"))

  info("Done", level = 3)
  files_tbl
}

#  FUNCTION: files_exist ----------------------------------------------------------------------
#
#' Determine whether files exist in the specified commit.
#'
#' This function returns `TRUE` if the file exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/repos/contents/#get-contents>
#'
#' @param paths (string, optional) The paths to the files or directories in the repository.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If not specified the head of the default branch is used.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each file specified.
#'
#' @export
#'
files_exist <- function(
  paths,
  repo,
  ref,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  .Deprecated(msg = "This function will be removed in a future version")

  if (missing(ref) || is_null(ref)) {
    ref <- NULL
  }

  (is_character(paths)) ||
    error("'paths' must be a character vector\n  '", paste(paths, collapse = "'\n  '"), "'")
  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_null(ref) || is_scalar_character(ref)) ||
    error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  gh_map(paths, simplify = TRUE, function(path) {
    info("Checking file '", path, "' exists in repository '", repo, "'")

    try_catch({
      gh_request(
        "GET", url = gh_url("repos", repo, "contents", path, ref = ref, api = api),
        token = token, ...)
      TRUE
    }, on_error = function(e) {
      FALSE
    })
  })
}

#  FUNCTION: download_commit ------------------------------------------------------------------
#
#' Download the contents of a commit
#'
#' This function downloads the entire commit to the specified location. The commit is
#' downloaded as a zip file, which is then unzipped into the desired location. The commit may
#' be specified by a branch, tag or SHA.
#'
#' <https://developer.github.com/v3/repos/contents/#get-archive-link>
#'
#' @param path (string, optional) The path to save to. If not specified the current working
#'   directory is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used. If not specified the default branch is used.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#' @param ... Parameters passed to [gh_download_binary()].
#'
#' @return A string containing the path. Also, all the files in the commit are downloaded to
#'   the specified location.
#'
#' @export
#'
download_commit <- function(
  path,
  repo,
  ref,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  if (missing(repo)) {
    info("'repo' is missing, so using 'path' argument: ", path, level = 2)
    repo <- path
    path <- NULL
  }
  if (missing(path) || is_null(path) || all(is_na(path))) {
    path <- getwd()
  }
  if (missing(ref) || is_null(ref)) {
    ref <- NULL
  }

  (is_repo(repo)) ||
    error("'repo' must be a string in the format 'owner/repo':\n  '", paste(repo, collapse = "'\n  '"), "'")
  (is_scalar_character(path)) ||
    error("'path' must be a string:\n  '", paste(path, collapse = "'\n  '"), "'")
  (is_null(ref) || is_scalar_character(ref)) ||
    error("'ref' must be NA or a string:\n  '", paste(ref, collapse = "'\n  '"), "'")
  (is_sha(token)) ||
    error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  (is_url(api)) ||
    error("'api' must be a valid URL:\n  '", paste(api, collapse = "'\n  '"), "'")

  if (!file.exists(path)) dir.create(path, recursive = TRUE)

  archive_path <- file.path(path, paste0(sub("/", "-", repo), "-", ref, ".zip"))
  on.exit(unlink(archive_path, recursive = TRUE), add = TRUE)

  info("Downloading commit '", ref, "' from repository '", repo, "'")
  gh_download_binary(
    gh_url("repos", repo, "zipball", ref, api = api),
    path = archive_path, token = token, ...)

  info("Unpacking commit into '", path, "'", level = 3)
  utils::unzip(archive_path, exdir = path)

  archive_folder <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  on.exit(unlink(archive_folder, recursive = TRUE), add = TRUE)

  subfolders <- list.dirs(archive_folder, recursive = TRUE, full.names = FALSE)
  gh_map(file.path(path, subfolders[subfolders != ""]), dir.create)

  files <- list.files(archive_folder, recursive = TRUE)
  file.rename(file.path(archive_folder, files), file.path(path, files))

  info("Done", level = 3)
  invisible(path)
}
