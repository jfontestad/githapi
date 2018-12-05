#  FUNCTION: view_readme ----------------------------------------------------------------------
#
#' View the contents of the README file
#'
#' This function reads the contents of the README file for a repository in GitHub and returns
#' it as a single string.
#'
#' <https://developer.github.com/v3/repos/contents/#get-the-readme>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If `NA` the head of the default branch is used.
#'   Default: `NA`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A string containing the README contents.
#'
#' @export
#'
view_readme <- function(
  repo,
  ref   = NA,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_na(ref) || is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  info("Getting README from repository '", repo, "'")
  readme <- gh_request(
    "GET", gh_url("repos", repo, "readme", ref = ref, api = api),
    accept  = "raw", token = token, ...)

  info("Transforming results")
  attr(readme, "header") <- NULL

  info("Done")
  readme
}

#  FUNCTION: view_contents --------------------------------------------------------------------
#
#' View the contents of files
#'
#' This function reads the contents of the specified files from a repository in GitHub.
#'
#' <https://developer.github.com/v3/repos/contents/#get-contents>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (string) The paths to the files.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If `NA` the head of the default branch is used.
#'   Default: `NA`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A named character vector with each element containing a file's contents.
#'
#' @export
#'
view_contents <- function(
  repo,
  paths,
  ref   = NA,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(paths))
  assert(is_na(ref) || is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  files <- map_vec(paths, function(path) {
    info("Getting contents of file '", path, "' from repository '", repo, "'")
    tryCatch({
      file <- gh_request(
        "GET", gh_url("repos", repo, "contents", path, ref = ref, api = api),
        accept  = "raw", token = token, ...)

      attr(file, "header") <- NULL
      file
    }, error = function(e) {
      info("File '", path, "' failed!")
      e
    })
  })

  info("Done")
  files
}

#  FUNCTION: view_files -----------------------------------------------------------------------
#
#' View information about a files
#'
#' This function returns details about the files in GitHub for the specified repository.
#' If no files are supplied then all the files in the reference commit are returned. If files
#' are supplied then only information about those files is returned. Finally, if a directory
#' is specified then details about all the files within the directory is returned.
#'
#' <https://developer.github.com/v3/repos/contents/#get-contents>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (string, optional) The paths to the files or directories in the repository.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch
#'   is specified the head commit is used. If `NA` the head of the default branch is used.
#'   Default: `NA`.
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
  repo,
  paths,
  ref   = NA,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_na(ref) || is_string(ref))
  assert(is_sha(token))
  assert(is_url(api))

  if (missing(paths) || is_na(paths)) {
    info("Getting files from repository '", repo, "'")
    files_list <- tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "contents", ref = ref, api = api),
        token = token, ...)
    }, error = function(e) {
      info("Failed!")
      list(e)
    })
  } else {
    assert(is_character(paths))
    files_list <- map(paths, function(path) {
      info("Getting file '", path, "' from repository '", repo, "'")
      tryCatch({
        gh_request(
          "GET", gh_url("repos", repo, "contents", path, ref = ref, api = api),
          token = token, ...)
      }, error = function(e) {
        info("File '", path, "' failed!")
        e
      })
    })
  }

  if (any(map_vec(files_list, is, "error"))) {
    collate_errors(files_list, "view_files() failed!")
  }

  info("Transforming results")
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

  info("Done")
  files_tbl
}

#  FUNCTION: create_files ---------------------------------------------------------------------
#
#' Create a new files
#'
#' This function creates a new text files in a GitHub repository. It adds a new commit for
#' each file created with the specifed commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#create-a-file>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (character) The paths to the files in the repository.
#' @param contents (character) The text to add to the files.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If `NA` the default branch for the repository
#'   is used. Default: `NA`.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
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
  repo,
  paths,
  contents,
  messages,
  branches  = NA,
  committer = NA,
  author    = NA,
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(paths))
  assert(is_character(contents) && identical(length(contents), length(paths)))
  assert(is_character(messages) && (is_scalar(messages) || identical(length(messages), length(paths))))
  assert(
    (is_na(branches) || is_character(branches)) &&
      (is_scalar(branches) || identical(length(branches), length(paths))))
  assert(is_na(committer) || (
    is_list(committer) && identical(names(committer), c("name", "email")) &&
      is_string(committer$name) && is_string(committer$email)))
  assert(is_na(author) || (
    is_list(author) && identical(names(author), c("name", "email")) &&
      is_string(author$name) && is_string(author$email)))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    path    = paths,
    content = contents,
    message = messages,
    branch  = branches)

  # TODO: If branches do not exist, create them

  files_list <- pmap(params, function(path, content, message, branch) {
    info("Posting file '", path, "' to repository '", repo, "'")
    tryCatch({
      payload <- list(
        message   = paste(message, "- added", path),
        content   = base64_enc(content),
        branch    = branch,
        committer = committer,
        author    = author) %>%
        remove_missing()

      gh_request(
        "PUT", gh_url("repos", repo, "contents", path, api = api),
        payload = payload, token = token, ...)
    }, error = function(e) {
      info("File '", path, "' failed!")
      e
    })
  })

  if (any(map_vec(files_list, is, "error"))) {
    collate_errors(files_list, "create_files() failed!")
  }

  info("Transforming results")
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
    mutate(commit_parent_sha = map(files_list, use_names = TRUE, function(f) {
      map_vec(f$commit$parents, getElement, "sha")
    })) %>%
    mutate(commit_parent_url = map(files_list, use_names = TRUE, function(f) {
      map_vec(f$commit$parents, getElement, "url")
    }))

  info("Done")
  files_tbl
}

#  FUNCTION: update_files ---------------------------------------------------------------------
#
#' Update existing files
#'
#' This function updates existing text files in a GitHub repository. It adds a new commit for
#' each file updated with the specifed commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#update-a-file>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (character) The paths to the files in the repository.
#' @param contents (character) The text to add to the files.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If `NA` the default branch for the repository
#'   is used. Default: `NA`.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
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
  repo,
  paths,
  contents,
  messages,
  branches  = NA,
  committer = NA,
  author    = NA,
  token     = getOption("github.token"),
  api       = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(paths))
  assert(is_character(contents) && identical(length(contents), length(paths)))
  assert(is_character(messages) && (is_scalar(messages) || identical(length(messages), length(paths))))
  assert(
    (is_na(branches) || is_character(branches)) &&
      (is_scalar(branches) || identical(length(branches), length(paths))))
  assert(is_na(committer) || (
    is_list(committer) && identical(names(committer), c("name", "email")) &&
      is_string(committer$name) && is_string(committer$email)))
  assert(is_na(author) || (
    is_list(author) && identical(names(author), c("name", "email")) &&
      is_string(author$name) && is_string(author$email)))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    path    = paths,
    content = contents,
    message = messages,
    branch  = branches)

  files_list <- pmap(params, function(path, content, message, branch) {
    info("Posting file '", path, "' to repository '", repo, "'")
    tryCatch({
      old_file <- gh_request(
        "GET", gh_url("repos", repo, "contents", path, ref = branch, api = api),
        token = token, ...)

      payload <- list(
        message   = paste(message, "- updated", path),
        content   = base64_enc(content),
        sha       = old_file$sha,
        branch    = branch,
        committer = committer,
        author    = author) %>%
        remove_missing()

      gh_request(
        "PUT", gh_url("repos", repo, "contents", path, api = api),
        payload = payload, token = token, ...)
    }, error = function(e) {
      info("File '", path, "' failed!")
      e
    })
  })

  if (any(map_vec(files_list, is, "error"))) {
    collate_errors(files_list, "update_files() failed!")
  }

  info("Transforming results")
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
    mutate(commit_parent_sha = map(files_list, use_names = FALSE, function(f) {
      map_vec(f$commit$parents, getElement, "sha")
    })) %>%
    mutate(commit_parent_url = map(files_list, use_names = FALSE, function(f) {
      map_vec(f$commit$parents, getElement, "url")
    }))

  info("Done")
  files_tbl
}

#  FUNCTION: delete_files ---------------------------------------------------------------------
#
#' Delete files.
#'
#' This function deletes the specified files from a repository in GitHub. It adds a new commit
#' for each file deleted with the specifed commit message and on the specified branch. The
#' committer and author can be set explicitly if necessary.
#'
#' <https://developer.github.com/v3/repos/contents/#delete-a-file>
#'
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param paths (character) The paths to the files in the repository.
#' @param messages (character) The commit messages to use. If a single message is provided it
#'   is used for all commits with the file added as a suffix.
#' @param branches (character, optional) The branches to make the commits on. If a single branch
#'   is provided all commits are made on it. If `NA` the default branch for the repository
#'   is used. Default: `NA`.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If `NA` then the authenticated user is used. Default: `NA`
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
  repo,
  paths,
  messages,
  branches  = NA,
  committer = NA,
  author    = NA,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_character(paths))
  assert(is_character(messages) && (is_scalar(messages) || identical(length(messages), length(paths))))
  assert(
    (is_na(branches) || is_character(branches)) &&
      (is_scalar(branches) || identical(length(branches), length(paths))))
  assert(is_na(committer) || (
    is_list(committer) && identical(names(committer), c("name", "email")) &&
      is_string(committer$name) && is_string(committer$email)))
  assert(is_na(author) || (
    is_list(author) && identical(names(author), c("name", "email")) &&
      is_string(author$name) && is_string(author$email)))
  assert(is_sha(token))
  assert(is_url(api))

  params <- tibble(
    path    = paths,
    message = messages,
    branch  = branches)

  files_list <- pmap(params, function(path, message, branch) {
    info("Deleting file '", path, "' from repository '", repo, "'")
    tryCatch({
      old_file <- gh_request(
        "GET", gh_url("repos", repo, "contents", path, ref = branch, api = api),
        token = token, ...)

      payload <- list(
        message   = paste(message, "- deleted", path),
        sha       = old_file$sha,
        branch    = branch,
        committer = committer,
        author    = author) %>%
        remove_missing()

      gh_request(
        "DELETE", gh_url("repos", repo, "contents", path, api = api),
        payload = payload, token = token, ...)
    }, error = function(e) {
      info(e$message)
      e
    })
  })

  if (any(map_vec(files_list, is, "error"))) {
    collate_errors(files_list, "delete_files() failed!")
    files_list[map_vec(files_list, is, "error")] <- FALSE
  }

  info("Transforming results")
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
    mutate(commit_parent_sha = map(files_list, use_names = FALSE, function(f) {
      map_vec(f$commit$parents, getElement, "sha")
    })) %>%
    mutate(commit_parent_url = map(files_list, use_names = FALSE, function(f) {
      map_vec(f$commit$parents, getElement, "url")
    }))

  info("Done")
  files_tbl
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
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string, optional) The path to save to. If `NA` the current working directory is
#'   used. Default: `NA`.
#' @param ref (string, optional) A git reference: either a SHA-1, tag or branch. If a branch is
#'   specified the head commit is used. If `NA` the default branch is used, Default: `NA`.
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
  repo,
  path  = getwd(),
  ref   = NA,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_repo(repo))
  assert(is_na(ref) || is_string(ref))
  assert(is_string(path))
  assert(is_sha(token))
  assert(is_url(api))

  if (!file.exists(path)) dir.create(path, recursive = TRUE)

  archive_path <- file.path(path, paste0(sub("/", "-", repo), "-", ref, ".zip"))
  on.exit(unlink(archive_path, recursive = TRUE), add = TRUE)

  info("Downloading commit")
  gh_download_binary(
    gh_url("repos", repo, "zipball", ref, api = api),
    path = archive_path, token = token, ...)

  info("Unpacking commit into '", path, "'")
  unzip(archive_path, exdir = path)

  archive_folder <- list.dirs(path, recursive = FALSE, full.names = TRUE)
  on.exit(unlink(archive_folder, recursive = TRUE), add = TRUE)

  subfolders <- list.dirs(archive_folder, recursive = TRUE, full.names = FALSE)
  map(file.path(path, subfolders[subfolders != ""]), dir.create)

  files <- list.files(archive_folder, recursive = TRUE)
  file.rename(file.path(archive_folder, files), file.path(path, files))

  info("Done")
  invisible(path)
}
