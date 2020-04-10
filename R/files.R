# FUNCTION: .upload_blob ----------------------------------------------------------------------
#
# Read a file and upload it to GitHub
#
# @param path (string) The path to the file to upload. It must be readable.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param ... Parameters passed to [gh_request()].
#
# @return `.upload_blob()` returns a list of the blob's properties.
#
.upload_blob <- function(
  path,
  repo,
  ...)
{
  assert(is_file(path) && is_readable(path), "'path' must be a readable file path:\n  ", path)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  info("Uploading file '", basename(path), "' to repository '", repo, "'", level = 2)
  content <- jsonlite::base64_enc(readBin(path, "raw", file.info(path)$size))

  gh_url("repos", repo, "git/blobs") %>%
    gh_request("POST", payload = list(content = content, encoding = "base64"), ...)
}


# FUNCTION: .upload_tree ----------------------------------------------------------------------
#
# Upload a directory of files as a tree
#
# @param path (string) The path to the directory to upload. It must be readable.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param base_commit (string, optional) Either a SHA, branch or tag used to identify the
#   commit to base the specified file change to. If not supplied the tree will just contain
#   the files specified.
# @param placeholder (boolean, optional) Whether the files are placeholders, containing the
#   SHA of the blob, of the actual contents. Default: `FALSE`.
# @param ignore (character, optional) The files to ignore in the directory. Default:
#   `".git"`, `".Rproj.user"`, `".Rhistory"`, `".RData"` and `".Ruserdata"`.
# @param ... Parameters passed to [gh_request()].
#
# @return `.upload_tree()` returns a list containing the tree SHA and the base commit SHA.
#
.upload_tree <- function(
  path,
  repo,
  base_commit = NULL,
  placeholder = FALSE,
  ignore      = c(".git", ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata"),
  ...)
{
  assert(is_dir(path) && is_readable(path), "'path' must be a readable directory path:\n  ", path)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_character(ignore), "'ignore' must be a character vector:\n  ", ignore)

  ignore <- unique(c(".", "..", ignore))

  info("Uploading files in '", path, "' to repository '", repo, "'", level = 2)
  tree <- list.files(path, all.files = TRUE, include.dirs = TRUE, full.names = TRUE) %>%
    discard(~ basename(.) %in% ignore) %>%
    file.info() %>%
    rownames_to_column("path") %>%
    mutate(sha = map2_chr(.data$path, .data$isdir, function(path, isdir)
    {
      if (isdir)
      {
        .upload_tree(
          path = path,
          repo = repo,
          placeholder = placeholder,
          ignore = ignore,
          ...)$tree_sha
      }
      else
      {
        if (placeholder)
        {
          readr::read_lines(file = path)
        }
        else
        {
          .upload_blob(path = path, repo = repo, ...)$sha
        }
      }
    })) %>%
    mutate(
      type = ifelse(.data$isdir, "tree", "blob"),
      mode = ifelse(.data$isdir, "040000", "100644"),
      path = basename(path)) %>%
    select("path", "mode", "type", "sha")

  payload <- tibble(
    path = tree$path,
    mode = tree$mode,
    type = tree$type,
    sha  = tree$sha) %>%
    pmap(list) %>%
    list(tree = .)

  if (!is_null(base_commit))
  {
    base_commit <- try_catch(
      view_commit(ref = base_commit, repo = repo, ...),
      on_error = function(e) NULL)
    payload$base_tree <- base_commit$tree_sha
  }

  info("Creating tree in repository '", repo, "'", level = 2)
  tree <- gh_url("repos", repo, "git/trees") %>% gh_request("POST", payload = payload, ...)

  list(commit_sha = base_commit$sha, tree_sha = tree$sha)
}


#  FUNCTION: upload_files ---------------------------------------------------------------------
#
#' Upload files and create a commit
#'
#' This function uploads the specified files to a repository in GitHub and creates a commit
#' on the specified branch.
#'
#' This function uploads the specified files to a repository and creates a new commit on the
#' specified branch. Note: the files are created, or updated if they already exist, and any
#' other files in the parent are left unchanged.
#'
#' The `author` and `committer` arguments are optional and if not supplied the current
#' authenticated user is used. However, if you want to set them explicitly you must specify
#' a named list with `name` and `email` as the elements (see examples).
#'
#' If a parent has been specified the files are created or updated. If a parent has not been
#' specified, but the branch exists the current head commit is used as a parent. If the
#' branch does not exist then an orphan commit is created.
#'
#' Note: The GitHub API imposes a file size limit of 100MB for this request.
#'
#' @param from_path (string) The paths to the files to upload. They must be readable.
#' @param to_path (string) The paths to write the files to, within the repository.
#' @param branch (string) The name of the branch to make the new commit on.
#' @param message (string) The commit message.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param author (list, optional) A the name and email address of the user who wrote the
#'   changes in the commit.
#' @param committer (list, optional) A the name and email address of the user who created
#'   the commit.
#' @param parent (string, optional) Reference for the commit to use as a parent, can be
#'   either a SHA, branch or tag. If it is a branch then the head commit is used. See the
#'   details section for more information.
#' @param force (boolean, optional) Whether to force the update if it is not a simple
#'   fast-forward. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `upload_files()` returns a list of the commit properties.
#'
#' **Commit Properties:**
#'
#' - **sha**: The commit SHA.
#' - **message**: The commit message.
#' - **author_name**: The author's name.
#' - **author_email**: The author's email address.
#' - **committer_name**: The committer's name.
#' - **committer_email**: The committer's email address.
#' - **tree_sha**: The SHA of the file tree.
#' - **parent_sha**: The commit SHA of the parent(s).
#' - **date**: The date the commit was made.
#'
#' @examples
#' \dontrun{
#'
#'   # Upload files to the master branch
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "master",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi")
#'
#'   # Upload files into directories within the master branch
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt", "c:/test/file3.txt"),
#'     to_path   = c("dir-1/file-1.txt", "dir-1/dir-1-1/file-2.txt", "dir-2/file-3.txt"),
#'     branch    = "master",
#'     message   = "Commit to test upload_files()",
#'     repo      = str_c("ChadGoymer/test-files-", now))
#'
#'   # Upload files to the master branch specifying an author and committer
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "master",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi",
#'     author    = list(name = "Bob",  email = "bob@acme.com"),
#'     committer = list(name = "Jane", email = "jane@acme.com"))
#'
#'   # Create a new branch from the master branch
#'   upload_files(
#'     from_path = c("c:/test/file1.txt", "c:/test/file2.txt"),
#'     to_path   = c("file1.txt", "file2.txt"),
#'     branch    = "new-branch",
#'     message   = "Commit to test upload_files()",
#'     repo      = "ChadGoymer/githapi",
#'     parent    = "master")
#' }
#'
#' @export
#'
upload_files <- function(
  from_path,
  to_path,
  branch,
  message,
  repo,
  author,
  committer,
  parent,
  force = FALSE,
  ...)
{
  assert(
    all(map_lgl(from_path, ~ is_file(.) && is_readable(.))),
    "All 'from_path' must be a readable file paths:\n  ", from_path)
  assert(is_character(to_path), "'to_path' must be a character vector:\n  ", to_path)
  assert(
    identical(length(from_path), length(to_path)),
    "'from_path' and 'to_path' must be the same length:\n  ", length(from_path), " vs ", length(to_path))
  assert(is_ref(branch), "'branch' must be a valid git reference - see help(is_ref):\n  ", branch)
  assert(is_scalar_character(message), "'message' must be a string:\n  ", message)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_logical(force), "'force' must be a boolean:\n  ", force)

  payload <- list(message = message)

  if (!missing(author))
  {
    assert(
      is_list(author) && is_scalar_character(author$name) && is_scalar_character(author$email),
      "'author' must be a list containing 'name' and 'email':\n ", author)
    payload$author <- author
  }

  if (!missing(committer))
  {
    assert(
      is_list(committer) && is_scalar_character(committer$name) && is_scalar_character(committer$email),
      "'committer' must be a list containing 'name' and 'email':\n ", committer)
    payload$committer <- committer
  }

  if (missing(parent))
  {
    parent <- branch
  }
  assert(is_scalar_character(parent), "'parent' must be a string:\n  ", parent)

  info("Uploading files to repository '", repo, "'")
  blob_shas <- map_chr(from_path, ~ .upload_blob(path = ., repo = repo, ...)$sha)

  temp_path <- tempfile("tree-")
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  walk2(file.path(temp_path, to_path), blob_shas, function(path, sha)
  {
    if (!dir.exists(dirname(path))) dir.create(dirname(path), recursive = TRUE)
    readr::write_lines(sha, path)
  })

  info("Uploading tree to repository '", repo, "'", level = 3)
  result <- .upload_tree(
    path        = temp_path,
    repo        = repo,
    base_commit = parent,
    placeholder = TRUE,
    ...)

  payload$tree    <- result$tree_sha
  payload$parents <- as.list(result$commit_sha)

  info("Creating commit in repo '", repo, "'")
  commit <- gh_url("repos", repo, "git/commits") %>% gh_request("POST", payload = payload, ...)

  if (identical(branch, parent) && is_null(result$commit_sha))
  {
    create_branch(name = branch, ref = commit$sha, repo = repo, ...)
  }
  else
  {
    branch_sha <- try_catch(
      view_commit(ref = branch, repo = repo, ...),
      on_error = function(e) NULL)

    if (is_null(branch_sha))
    {
      create_branch(name = branch, ref = commit$sha, repo = repo, ...)
    }
    else
    {
      update_branch(branch = branch, ref = commit$sha, repo = repo, force = force, ...)
    }
  }

  view_commit(commit$sha, repo = repo, ...)
}
