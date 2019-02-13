#  FUNCTION: view_commits ---------------------------------------------------------------------
#
#' View information about Git commits.
#'
#' This function returns details about the commits for the specified repository in GitHub.
#'
#' <https://developer.github.com/v3/git/commits/#get-a-commit>
#'
#' @param shas (character) The SHAs of the commits.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the commits, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **sha**: The SHA of the commit.
#'   - **message**: The commit message recorded in GitHub.
#'   - **author_name**: The author of the commit.
#'   - **author_email**: The author's email address.
#'   - **committer_name**: The committer of the commit.
#'   - **committer_email**: The committer's email address.
#'   - **date**: The date and time of the commit.
#'   - **url**: The URL to get the commit details from GitHub.
#'   - **tree_sha**: The SHA of the git tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **parent_sha**: The SHA(s) of the parent commit(s).
#'   - **parent_url**: The URL(s) to get the parent's details from GitHub.
#'
#' @export
#'
view_commits <- function(
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  commits_list <- map(shas, function(sha) {
    info("Getting commit for sha '", sha, "' from repository '", repo, "'")
    tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "git/commits", sha, api = api),
        token = token, ...)
    }, error = function(e) {
      warn("sha '", sha, "' failed!", level = 2)
      e
    })
  })

  if (any(map_vec(commits_list, is, "error"))) {
    collate_errors(commits_list, "view_commits() failed!")
  }

  info("Transforming results", level = 2)
  commits_tbl <- bind_fields(commits_list, list(
    sha             = c("sha",                as = "character"),
    message         = c("message",            as = "character"),
    author_name     = c("author", "name",     as = "character"),
    author_email    = c("author", "email",    as = "character"),
    committer_name  = c("committer", "name",  as = "character"),
    committer_email = c("committer", "email", as = "character"),
    date            = c("author", "date",     as = "datetime"),
    url             = c("url",                as = "character"),
    tree_sha        = c("tree", "sha",        as = "character"),
    tree_url        = c("tree", "url",        as = "character"),
    parent_sha      = "",
    parent_url      = "")) %>%
    mutate(parent_sha = map(commits_list, use_names = FALSE, function(cm) {
      if (is_null(cm$parents) || identical(length(cm$parents), 0L)) {
        NULL
      } else {
        map_vec(cm$parents, getElement, "sha")
      }
    })) %>%
    mutate(parent_url = map(commits_list, use_names = FALSE, function(cm) {
      if (is_null(cm$parents) || identical(length(cm$parents), 0L)) {
        NULL
      } else {
        map_vec(cm$parents, getElement, "url")
      }
    }))

  info("Done", level = 2)
  commits_tbl
}

#  FUNCTION: create_commit --------------------------------------------------------------------
#
#' Create a commit
#'
#' This function creates a commit object in a GitHub repository. The commit must point to a
#' tree object that has already been uploaded and identified by it's SHA. In addition at
#' least one parent commit must be specified.
#'
#' <https://developer.github.com/v3/git/commits/#create-a-commit>
#'
#' @param message (string) The commit message
#' @param tree (string) The SHA of the tree object this commit points to
#' @param parents (character, optional) The SHAs of the commits that were the parents of this
#'   commit. If not specified the commit will be written as a root commit. For a single parent,
#'   one SHA should be provided; for a merge commit, a character vector of more than one should be
#'   provided.
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
#' @return A tibble describing the commit, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **sha**: The SHA of the commit.
#'   - **message**: The commit message recorded in GitHub.
#'   - **author_name**: The author of the commit.
#'   - **author_email**: The author's email address.
#'   - **committer_name**: The committer of the commit.
#'   - **committer_email**: The committer's email address.
#'   - **date**: The date and time of the commit.
#'   - **url**: The URL to get the commit details from GitHub.
#'   - **tree_sha**: The SHA of the git tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **parent_sha**: The SHA(s) of the parent commit(s).
#'   - **parent_url**: The URL(s) to get the parent's details from GitHub.
#'
#' @export
#'
create_commit <- function(
  message,
  tree,
  parents,
  committer,
  author,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_string(message))
  assert(is_sha(tree))

  if (missing(parents) || is_null(parents)) {
    parents <- NA
  }
  assert(is_na(parents) || is_character(parents))

  if (missing(committer) || is_null(committer)) {
    committer <- NA
  }
  assert(is_na(committer) || (is_list(committer) && identical(names(committer), c("name", "email")) && is_string(committer$name) && is_string(committer$email)))

  if (missing(author) || is_null(author)) {
    author <- NA
  }
  assert(is_na(author) || (is_list(author) && identical(names(author), c("name", "email")) && is_string(author$name) && is_string(author$email)))

  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  parents <- map(parents, use_names = FALSE, function(p) {
    if (!is_na(p) && !is_sha(p)) {
      p <- view_shas(p, repo = repo) %>% unname()
    }
    p
  })

  payload <- list(
    message   = message,
    tree      = tree,
    parents   = parents,
    committer = committer,
    author    = author) %>%
    remove_missing()

  info("Posting commit to repo '", repo, "'")
  commit_list <- gh_request(
    "POST", gh_url("repos", repo, "git/commits", api = api),
    payload = payload, token = token, ...) %>%
    list()

  info("Transforming results", level = 2)
  commit_tbl <- bind_fields(commit_list, list(
    sha             = c("sha",                as = "character"),
    message         = c("message",            as = "character"),
    author_name     = c("author", "name",     as = "character"),
    author_email    = c("author", "email",    as = "character"),
    committer_name  = c("committer", "name",  as = "character"),
    committer_email = c("committer", "email", as = "character"),
    date            = c("author", "date",     as = "datetime"),
    url             = c("url",                as = "character"),
    tree_sha        = c("tree", "sha",        as = "character"),
    tree_url        = c("tree", "url",        as = "character"),
    parent_sha      = "",
    parent_url      = "")) %>%
    mutate(parent_sha = map(commit_list, use_names = FALSE, function(cm) {
      if (is_null(cm$parents) || identical(length(cm$parents), 0L)) {
        NULL
      } else {
        map_vec(cm$parents, getElement, "sha")
      }
    })) %>%
    mutate(parent_url = map(commit_list, use_names = FALSE, function(cm) {
      if (is_null(cm$parents) || identical(length(cm$parents), 0L)) {
        NULL
      } else {
        map_vec(cm$parents, getElement, "url")
      }
    }))

  info("Done", level = 2)
  commit_tbl
}

#  FUNCTION: upload_commit --------------------------------------------------------------------
#
#' Upload a directory of files and create a commit on a branch
#'
#' This function uploads all the files in the specified directory path (using [upload_tree()]).
#' It the creates a new commit (using [create_commit()]) and sets it as the head of the
#' specified branch (using [create_branches()]).
#'
#' The default behaviour is to treat the directory as containing the entire set of files and
#' completely `replace` the previous commit with this one. This means that if a previously
#' existing file is not in the directory it will be deleted in the commit. Alternatively, if
#' you just want to update a few files, you can specify `replace = FALSE`, in which case
#' the files in the directory will replace the ones in the previous commit, and all other
#' files will remain unchanged. This is useful if you have a lot of files in the commit.
#'
#' <https://developer.github.com/v3/git/blobs/#create-a-blob>
#'
#' <https://developer.github.com/v3/git/trees/#create-a-tree>
#'
#' <https://developer.github.com/v3/git/commits/#create-a-commit>
#'
#' <https://developer.github.com/v3/git/refs/#create-a-reference>
#'
#' @param branch (string) The name of the branch to set the commit as head.
#' @param message (string) The commit message
#' @param path (string) The path to the directory, which is to be uploaded as a tree.
#' @param parents (character, optional) The SHAs of the commits that were the parents of this
#'   commit. If not specified the commit will be written as a root commit. For a single parent,
#'   one SHA should be provided; for a merge commit, a character vector of more than one should be
#'   provided.
#' @param committer (list, optional) The name and email address of the committer. This needs
#'   to be specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param author (list, optional) The name and email address of the author. This needs to be
#'   specified as a named list, e.g. `list(name = "Bob Smith", email = "bob.smith@acme.com")`.
#'   If not specified then the authenticated user is used.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param replace (logical, optional) Whether the commit should replace the previous one or
#'   just update the files within the directory (see description). Default: `TRUE`.
#' @param ignore (character) A character vector of regular expressions. If any of these are
#'   detected in a file name they are not uploaded. Default: `"\\.git"`, `"\\.Rproj\\.user"`,
#'   `"\\.Rhistory"`, `"\\.RData"` & `"\\.Ruserdata"`
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A tibble describing the commit, with the following columns
#'   (see [GitHub's documentation](https://developer.github.com/v3/repos/commits/)
#'   for more details):
#'   - **sha**: The SHA of the commit.
#'   - **message**: The commit message recorded in GitHub.
#'   - **author_name**: The author of the commit.
#'   - **author_email**: The author's email address.
#'   - **committer_name**: The committer of the commit.
#'   - **committer_email**: The committer's email address.
#'   - **date**: The date and time of the commit.
#'   - **url**: The URL to get the commit details from GitHub.
#'   - **tree_sha**: The SHA of the git tree.
#'   - **tree_url**: The URL to get the tree details from GitHub.
#'   - **parent_sha**: The SHA(s) of the parent commit(s).
#'   - **parent_url**: The URL(s) to get the parent's details from GitHub.
#'
#' @export
#'
upload_commit <- function(
  branch,
  message,
  path,
  parents,
  committer,
  author,
  repo,
  replace = TRUE,
  ignore  = c("\\.git", "\\.Rproj\\.user", "\\.Rhistory", "\\.RData", "\\.Ruserdata"),
  token   = getOption("github.token"),
  api     = getOption("github.api"),
  ...)
{
  assert(is_string(branch))
  assert(is_string(message))
  assert(is_dir(path) && is_readable(path))

  if (missing(parents) || is_null(parents)) {
    parents <- NA
  }
  assert(is_na(parents) || is_character(parents))

  if (missing(committer) || is_null(committer)) {
    committer <- NA
  }
  assert(is_na(committer) || (is_list(committer) && identical(names(committer), c("name", "email")) && is_string(committer$name) && is_string(committer$email)))

  if (missing(author) || is_null(author)) {
    author <- NA
  }
  assert(is_na(author) || (is_list(author) && identical(names(author), c("name", "email")) && is_string(author$name) && is_string(author$email)))

  assert(is_repo(repo))
  assert(is_boolean(replace))
  assert(is_character(ignore))
  assert(is_sha(token))
  assert(is_url(api))

  if (replace) {
    base_tree <- NA
  } else {
    parent_commit <- view_history(ref = parents[[1]], repo = repo, n_max = 1)
    base_tree <- parent_commit$tree_sha
  }

  tree <- upload_tree(
    path      = path,
    base_tree = base_tree,
    repo      = repo,
    ignore    = ignore,
    token     = token,
    api       = api)

  commit <- create_commit(
    message   = message,
    tree      = tree$tree_sha[[1]],
    parents   = parents,
    committer = committer,
    author    = author,
    repo      = repo,
    token     = token,
    api       = api)

  if (branches_exist(branch = branch, repo = repo, token = token, api = api)) {
    update_branches(
      branches  = branch,
      shas      = commit$sha,
      repo      = repo,
      token     = token,
      api       = api)
  } else {
    create_branches(
      branches  = branch,
      shas      = commit$sha,
      repo      = repo,
      token     = token,
      api       = api)
  }

  commit
}

#  FUNCTION: commits_exist --------------------------------------------------------------------
#
#' Determine whether commits exist in the specified repository.
#'
#' This function returns `TRUE` if the commit exists and `FALSE` otherwise.
#'
#' <https://developer.github.com/v3/git/refs/#get-a-reference>
#'
#' @param shas (character) The SHAs of the commits.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` (or `GITHUB_PAT`) or in the
#'   R option `"github.token"`.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or in the R option `"github.api"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return A logical vector containing `TRUE` or `FALSE` for each commit specified.
#'
#' @export
#'
commits_exist <- function(
  shas,
  repo,
  token = getOption("github.token"),
  api   = getOption("github.api"),
  ...)
{
  assert(is_character(shas) && all(map_vec(shas, is_sha)))
  assert(is_repo(repo))
  assert(is_sha(token))
  assert(is_url(api))

  map_vec(shas, function(sha) {
    info("Checking commit '", sha, "' exists in repository '", repo, "'")
    tryCatch({
      gh_request(
        "GET", gh_url("repos", repo, "git/commits", sha, api = api),
        token = token, ...)
      TRUE
    }, error = function(e) {
      FALSE
    })
  })
}
