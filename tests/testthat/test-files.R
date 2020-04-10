context("files")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  test_repo <- create_repository(
    name        = str_c("test-files-", now),
    description = "This is a repository to test files",
    auto_init   = TRUE)

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-files-", now))

}))


# TEST: .upload_blob --------------------------------------------------------------------------

test_that(".upload_blob uploads a file to github", {

  temp_path <- file.path(tempdir(), "test-upload-blob")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
    writeLines(file.path(temp_path, "file-blob.txt"))

  blob <- .upload_blob(
    path = file.path(temp_path, "file-blob.txt"),
    repo = str_c("ChadGoymer/test-files-", now))

  expect_is(blob, "list")
  expect_identical(attr(blob, "status"), 201L)
  expect_identical(
    map_chr(blob, ~ class(.)[[1]]),
    c(sha = "character",
      url = "character"))

  expect_true(is_sha(blob$sha))

})


# TEST: .upload_tree --------------------------------------------------------------------------

test_that(".upload_tree uploads a directory structure to github", {

  temp_path <- file.path(tempdir(), "test-upload-tree")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f)
  {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_tree <- .upload_tree(
    path = flat_path,
    repo = str_c("ChadGoymer/test-files-", now))

  expect_is(flat_tree, "list")
  expect_identical(
    map_chr(flat_tree, ~ class(.)[[1]]),
    c(commit_sha = "NULL",
      tree_sha   = "character"))

  expect_null(flat_tree$commit_sha)
  expect_true(is_sha(flat_tree$tree_sha))


  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"))
  file.path(recursive_path, c("dir-1/dir-1-1/dir-1-1-1", "dir-1/dir-1-2", "dir-2")) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f)
  {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(f)
  })


  recursive_tree <- .upload_tree(
    path = recursive_path,
    repo = str_c("ChadGoymer/test-files-", now))

  expect_is(recursive_tree, "list")
  expect_identical(
    map_chr(recursive_tree, ~ class(.)[[1]]),
    c(commit_sha = "NULL",
      tree_sha   = "character"))

  expect_null(recursive_tree$commit_sha)
  expect_true(is_sha(recursive_tree$tree_sha))


  base_tree <- .upload_tree(
    path = flat_path,
    repo = str_c("ChadGoymer/test-files-", now),
    base_commit = "master")

  expect_is(base_tree, "list")
  expect_identical(
    map_chr(base_tree, ~ class(.)[[1]]),
    c(commit_sha = "character",
      tree_sha   = "character"))

  expect_true(is_sha(base_tree$commit_sha))
  expect_true(is_sha(base_tree$tree_sha))


  placeholder_path <- file.path(temp_path, "placeholders")
  dir.create(placeholder_path)

  walk(str_c("file", 1:2, ".txt"), function(f)
  {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(file.path(placeholder_path, f))
  })

  blob_shas <- list.files(placeholder_path, full.names = TRUE) %>%
    map_chr(~ .upload_blob(path = ., repo = str_c("ChadGoymer/test-files-", now))$sha)

  walk2(blob_shas, list.files(placeholder_path, full.names = TRUE), writeLines)

  placeholder_tree <- .upload_tree(
    path        = placeholder_path,
    repo        = str_c("ChadGoymer/test-files-", now),
    placeholder = TRUE)

  expect_is(placeholder_tree, "list")
  expect_identical(
    map_chr(placeholder_tree, ~ class(.)[[1]]),
    c(commit_sha = "NULL",
      tree_sha   = "character"))

  expect_null(placeholder_tree$commit_sha)
  expect_true(is_sha(placeholder_tree$tree_sha))


  ignore_tree <- .upload_tree(
    path   = flat_path,
    repo   = str_c("ChadGoymer/test-files-", now),
    ignore = "file1.txt")

  expect_is(ignore_tree, "list")
  expect_identical(
    map_chr(ignore_tree, ~ class(.)[[1]]),
    c(commit_sha = "NULL",
      tree_sha   = "character"))

  expect_null(ignore_tree$commit_sha)
  expect_true(is_sha(ignore_tree$tree_sha))

})


# TEST: upload_files --------------------------------------------------------------------------

test_that("upload_files uploads files and creates a new commit", {

  temp_path <- file.path(tempdir(), "test-upload-files")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f)
  {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "master",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now))

  expect_is(flat_commit, "list")
  expect_identical(attr(flat_commit, "status"), 200L)
  expect_identical(
    map_chr(flat_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(flat_commit$message, "Commit to test upload_files()")
  expect_identical(flat_commit$author_name, "Chad Goymer")
  expect_identical(flat_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(flat_commit$committer_name, "Chad Goymer")
  expect_identical(flat_commit$committer_email, "chad.goymer@gmail.com")

  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"))
  file.path(recursive_path, c("dir-1/dir-1-1/dir-1-1-1", "dir-1/dir-1-2", "dir-2")) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f)
  {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(f)
  })


  recursive_commit <- upload_files(
    from_path = recursive_file_paths[c(1,3,5)],
    to_path   = c("dir-1/file-1.txt", "dir-1/dir-1-1/file-2.txt", "dir-2/file-3.txt"),
    branch    = "master",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now))

  expect_is(recursive_commit, "list")
  expect_identical(attr(recursive_commit, "status"), 200L)
  expect_identical(
    map_chr(recursive_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(recursive_commit$message, "Commit to test upload_files()")


  author_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "master",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now),
    author    = list(name = "Bob",  email = "bob@acme.com"),
    committer = list(name = "Jane", email = "jane@acme.com"))

  expect_is(author_commit, "list")
  expect_identical(attr(author_commit, "status"), 200L)
  expect_identical(
    map_chr(author_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(author_commit$message, "Commit to test upload_files()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")


  orphan_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = str_c("test-files-1-", now),
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now))

  expect_is(orphan_commit, "list")
  expect_identical(attr(orphan_commit, "status"), 200L)
  expect_identical(
    map_chr(orphan_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(orphan_commit$message, "Commit to test upload_files()")
  expect_identical(orphan_commit$parents, character())


  valid_parent_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "master",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now),
    parent    = author_commit$sha)

  expect_is(valid_parent_commit, "list")
  expect_identical(attr(valid_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(valid_parent_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(valid_parent_commit$message, "Commit to test upload_files()")
  expect_identical(valid_parent_commit$parents, author_commit$sha)


  expect_error(
    upload_files(
      from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
      to_path   = c("file-1.txt", "file-2.txt"),
      branch    = "master",
      message   = "Commit to test upload_files()",
      repo      = str_c("ChadGoymer/test-files-", now),
      parent    = flat_commit$sha),
    "Update is not a fast forward")


  force_parent_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = "master",
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now),
    parent    = recursive_commit$sha,
    force     = TRUE)

  expect_is(force_parent_commit, "list")
  expect_identical(attr(force_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(force_parent_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(force_parent_commit$message, "Commit to test upload_files()")
  expect_identical(force_parent_commit$parents, recursive_commit$sha)


  new_branch_commit <- upload_files(
    from_path = file.path(flat_path, c("file1.txt", "file2.txt")),
    to_path   = c("file-1.txt", "file-2.txt"),
    branch    = str_c("test-files-2-", now),
    message   = "Commit to test upload_files()",
    repo      = str_c("ChadGoymer/test-files-", now),
    parent    = flat_commit$sha)

  expect_is(new_branch_commit, "list")
  expect_identical(attr(new_branch_commit, "status"), 200L)
  expect_identical(
    map_chr(new_branch_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(new_branch_commit$message, "Commit to test upload_files()")
  expect_identical(new_branch_commit$parents, flat_commit$sha)

})
