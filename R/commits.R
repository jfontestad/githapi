#  FUNCTION: upload_commit --------------------------------------------------------------------
#
# TODO: Uncomment in version 1.0
# Upload a directory of files and create a commit
#
# This function uploads all the files in the directory (except those in the `ignore`
# argument) and creates a new commit on the specified branch. Note: the new commit is
# created with exactly the files uploaded, so if a file in the parent commit has not been
# uploaded it will be removed in the new commit.
#
# The `author` and `committer` arguments are optional and if not supplied the current
# authenticated user is used. However, if you want to set them explicitly you must specify
# a named list with `name` and `email` as the elements (see examples).
#
# A commit may have none, one or two parents. If none are specified and the branch already
# exists, the head of the branch will be used as a parent. If the branch does not exist,
# an orphan commit is created without a parent. If one parent is specified the commit is
# added and the branch set to the new commit, if it is a simple fast-forward. If you want
# to set the branch to the new commit no matter what then set the `force` argument to
# `TRUE`. If two parents are specified then a merge commit is created.
#
# @param path (string) The path to the directory to upload. It must be readable.
# @param branch (string) The name of the branch to make the new commit on.
# @param message (string) The commit message.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param author (list, optional) A the name and email address of the user who wrote the
#   changes in the commit.
# @param committer (list, optional) A the name and email address of the user who created
#   the commit.
# @param parents (character, optional) References for the commits to use as parents, can
#   be either a SHA, branch or tag. If it is a branch then the head commit is used. See
#   the details section for more information.
# @param ignore (character, optional) The files to ignore in the directory. Default: `".git"`,
#   `".Rproj.user"`, `".Rhistory"`, `".RData"` and `".Ruserdata"`.
# @param force (boolean, optional) Whether to force the update if it is not a simple
#   fast-forward. Default: `FALSE`.
# @param ... Parameters passed to [gh_request()].
#
# @return `upload_commit()` returns a list of the commit properties.
#
# **Commit Properties:**
#
# - **sha**: The commit SHA.
# - **message**: The commit message.
# - **author_name**: The author's name.
# - **author_email**: The author's email address.
# - **committer_name**: The committer's name.
# - **committer_email**: The committer's email address.
# - **tree_sha**: The SHA of the file tree.
# - **parent_sha**: The commit SHA of the parent(s).
# - **date**: The date the commit was made.
#
# @examples
# \dontrun{
#   # Add a commit to the master branch
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi")
#
#   # override the author and committer
#   upload_commit(
#     path      = "C:/files-to-upload",
#     branch    = "master",
#     message   = "Commit to test upload_commit()",
#     repo      = "ChadGoymer/test-githapi",
#     author    = list(name = "Bob",   email = "bob\@acme.com"),
#     committer = list(name = "Jane",  email = "jane\@acme.com"))
#
#   # Create a commit on a new branch
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "test-commits-1",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi",
#     parents = "master")
#
#   # Create an orphan commit
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "test-commits-2",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi")
#
#   # Force branch to point at the new commit
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi",
#     parents = "test-commits-1",
#     force   = TRUE)
#
#   # Create a commit merging a branch into the master branch
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi",
#     parents = c("master", "test-commits-1"))
#
#   # Create a commit merging two branches into a new branch
#   upload_commit(
#     path    = "C:/files-to-upload",
#     branch  = "test-commits-3",
#     message = "Commit to test upload_commit()",
#     repo    = "ChadGoymer/test-githapi",
#     parents = c("master", "test-commits-2"))
# }
#
# @export
#
# upload_commit <- function(
#   path,
#   branch,
#   message,
#   repo,
#   author,
#   committer,
#   parents,
#   ignore = c("\\.git", "\\.Rproj\\.user", "\\.Rhistory", "\\.RData", "\\.Ruserdata"),
#   force  = FALSE,
#   ...)
# {
#   assert(is_dir(path) && is_readable(path), "'path' must be a readable directory path:\n  ", path)
#   assert(is_ref(branch), "'branch' must be a valid git reference - see help(is_ref):\n  ", branch)
#   assert(is_scalar_character(message), "'message' must be a string:\n  ", message)
#   assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
#   assert(is_character(ignore), "'ignore' must be a character vector:\n  ", ignore)
#   assert(is_scalar_logical(force), "'force' must be a boolean:\n  ", force)
#
#   payload <- list(message = message)
#
#   if (!missing(author))
#   {
#     assert(
#       is_list(author) && is_scalar_character(author$name) && is_scalar_character(author$email),
#       "'author' must be a list containing 'name' and 'email':\n ", author)
#     payload$author <- author
#   }
#
#   if (!missing(committer))
#   {
#     assert(
#       is_list(committer) && is_scalar_character(committer$name) && is_scalar_character(committer$email),
#       "'committer' must be a list containing 'name' and 'email':\n ", committer)
#     payload$committer <- committer
#   }
#
#   info("Uploading files in '", path, "' to repository '", repo, "'")
#   payload$tree <- .upload_tree(path = path, repo = repo, ignore = ignore)$sha
#
#   branch_sha <- try_catch(view_sha(ref = branch, repo = repo), on_error = function(e) NULL)
#
#   if (missing(parents))
#   {
#     if (is_null(branch_sha))
#       parents <- NULL
#     else
#       parents <- as.list(branch_sha)
#   }
#   else
#   {
#     assert(is_character(parents), "'parents' must be a character vector:\n  ", parents)
#     parents <- map(parents, function(p) if (!is_sha(p)) view_sha(ref = p, repo = repo) else p)
#   }
#   payload$parents <- parents
#
#   info("Creating commit in repo '", repo, "'")
#   commit_lst <- gh_url("repos", repo, "git/commits") %>%
#     gh_request("POST", payload = payload, ...)
#
#   if (is_null(branch_sha))
#     create_branch(name = branch, ref = commit_lst$sha, repo = repo)
#   else
#     update_branch(branch = branch, ref = commit_lst$sha, repo = repo, force = force)
#
#   view_commit(commit_lst$sha, repo = repo)
# }


#  FUNCTION: download_commit ------------------------------------------------------------------
#
# Download a commit from GitHub
#
# This function downloads all the files in a commit, plus any folders, into the path
# specified.
#
# @param ref (string) Either a SHA, branch or tag used to identify the commit the tag is
#   pointing at.
# @param repo (string) The repository specified in the format: `owner/repo`.
# @param path (string, optional) The path to the directory to upload. It must be readable.
#   Default: working directory.
# @param ... Parameters passed to [gh_request()].
#
# @return `download_commit()` returns the path where the commit is downloaded to.
#
# @examples
# \dontrun{
#
#   # Download the head of the master branch to the home directory
#   download_commit(
#     ref  = "master",
#     repo = "ChadGoymer/test-githapi",
#     path = "~")
# }
#
# @export
#
# download_commit <- function(
#   ref,
#   repo,
#   path = getwd(),
#   ...)
# {
#   assert(is_ref(ref), "'ref' must be a string:\n  ", ref)
#   assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
#
#   if (!file.exists(path)) dir.create(path, recursive = TRUE)
#
#   archive_path <- str_replace(repo, "/", "-") %>% str_c("-", ref, ".zip") %>% file.path(path, .)
#   on.exit(unlink(archive_path, recursive = TRUE), add = TRUE)
#
#   info("Downloading commit '", ref, "' from repository '", repo, "'")
#   path_gh <- gh_url("repos", repo, "zipball", ref) %>%
#     gh_download(archive_path, ...)
#
#   info("Unpacking commit into '", path, "'", level = 3)
#   utils::unzip(archive_path, exdir = path)
#
#   archive_folder <- list.dirs(path, recursive = FALSE, full.names = TRUE)
#   on.exit(unlink(archive_folder, recursive = TRUE), add = TRUE)
#
#   subfolders <- list.dirs(archive_folder, recursive = TRUE, full.names = FALSE)
#   walk(file.path(path, subfolders[subfolders != ""]), dir.create)
#
#   files <- list.files(archive_folder, recursive = TRUE)
#   file.rename(file.path(archive_folder, files), file.path(path, files))
#
#   info("Done", level = 3)
#   structure(
#     normalizePath(path, winslash = "/"),
#     class   = class(path_gh),
#     url     = attr(path_gh, "url"),
#     request = attr(path_gh, "request"),
#     status  = attr(path_gh, "status"),
#     header  = attr(path_gh, "header"))
# }
