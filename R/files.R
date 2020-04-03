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
# **Blob Properties:**
#
# - **sha**: The SHA-1 hash of the blob.
# - **url**: The URL of the blob API.
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
# @param ignore (character, optional) The files to ignore in the directory. Default:
#   `".git"`, `".Rproj.user"`, `".Rhistory"`, `".RData"` and `".Ruserdata"`.
# @param ... Parameters passed to [gh_request()].
#
# @return `.upload_tree()` returns a list of the tre's properties.
#
# **Blob Properties:**
#
# - **sha**: The SHA-1 hash of the blob.
# - **url**: The URL of the blob API.
# - **tree**: list of files and directories, each having properties:
#   - **path**: The file or directory path.
#   - **mode**: The file or directory mode.
#   - **type**: Whether it is a blob (file) or a tree (directory).
#   - **size**: The size in bytes.
#   - **sha**: The SHA-1 hash of the blob or tree.
#   - **url**: The URL of the blob or tree API.
#
.upload_tree <- function(
  path,
  repo,
  ignore = c(".git", ".Rproj.user", ".Rhistory", ".RData", ".Ruserdata"),
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
        .upload_tree(path = path, repo = repo)$sha
      else
        .upload_blob(path = path, repo = repo)$sha
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

  info("Creating tree in repository '", repo, "'", level = 2)
  gh_url("repos", repo, "git/trees") %>%
    gh_request("POST", payload = payload, ...)
}
