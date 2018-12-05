context("git commits")

# TEST: view_commits --------------------------------------------------------------------------

test_that("view_commits returns a tibble of information about the commits", {
  commits <- view_commits(
    repo = "ChadGoymer/test-githapi",
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "ccb62ec75de7e40c689be427cd038c8a1a9d3c44"))

  expect_is(commits, "tbl")
  expect_identical(
    map_vec(commits, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  expect_identical(
    commits$message,
    c("Initial commit", "Testing create_files() - added aaaa.txt"))
  expect_identical(
    as.character(commits$date),
    c("2018-10-29 08:10:24", "2018-10-31 17:06:53"))
})

# TEST: create_commit -------------------------------------------------------------------------

test_that("create_commit creates a new commit in a repository", {
  created_commit <- create_commit(
    repo    = "ChadGoymer/test-githapi",
    message = "This is created with create_commit()",
    tree    = "bbac77ba8fc1afa9a815a0cc8fc17e221cb0c027",
    parents = "master")

  expect_is(created_commit, "tbl")
  expect_identical(
    map_vec(created_commit, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  viewed_commit <- view_commits("ChadGoymer/test-githapi", created_commit$sha)
  expect_identical(created_commit, viewed_commit)
})

# TEST: upload_commit -------------------------------------------------------------------------

test_that("upload_commit uploads files and directory structure to github", {
  skip("Does not work in Travis")

  flat_commit <- upload_commit(
    repo    = "ChadGoymer/test-githapi",
    branch  = "master",
    message = "Test commit made with upload_commit",
    path    = system.file("test-data/upload-tree/test-dir", package = "githapi"),
    parents = "master")

  expect_is(flat_commit, "tbl")
  expect_identical(
    map_vec(flat_commit, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  expect_identical(flat_commit$message, c("Test commit made with upload_commit"))

  master_files <- view_trees("ChadGoymer/test-githapi", shas = flat_commit$sha)
  expect_identical(master_files$path, c("file-in-dir-1.txt", "file-in-dir-2.txt"))

  recursive_commit <- upload_commit(
    repo    = "ChadGoymer/test-githapi",
    branch  = "master",
    message = "Recursive commit made with upload_commit",
    path    = system.file("test-data/upload-tree", package = "githapi"),
    parents = "master")

  expect_is(recursive_commit, "tbl")
  expect_identical(
    map_vec(recursive_commit, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  expect_identical(recursive_commit$message, c("Recursive commit made with upload_commit"))

  master_files <- view_trees("ChadGoymer/test-githapi", shas = recursive_commit$sha)
  expect_identical(
    master_files$path,
    c("README.md",
      "test-dir",
      "test-dir/file-in-dir-1.txt",
      "test-dir/file-in-dir-2.txt",
      "test-file.txt"))

  temp_path <- file.path(tempdir(), "test-git-commits")
  if (!dir.exists(temp_path)) dir.create(temp_path)
  on.exit(unlink(temp_path))

  download_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "test-file.txt",
    location = temp_path,
    ref      = "master")

  temp_file <- file.path(temp_path, "test-file.txt")
  now <- as.character(Sys.time())
  writeLines(c("This file was updated:", now), temp_file)

  updated_commit <- upload_commit(
    repo      = "ChadGoymer/test-githapi",
    branch    = "master",
    message   = "Update commit made with upload_commit",
    path      = temp_path,
    parents   = "master",
    replace   = FALSE)

  expect_is(updated_commit, "tbl")
  expect_identical(
    map_vec(updated_commit, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  expect_identical(updated_commit$message, c("Update commit made with upload_commit"))

  master_files <- view_trees("ChadGoymer/test-githapi", shas = updated_commit$sha)
  expect_identical(
    master_files$path,
    c("README.md",
      "test-dir",
      "test-dir/file-in-dir-1.txt",
      "test-dir/file-in-dir-2.txt",
      "test-file.txt"))

  file_contents <- read_files("ChadGoymer/test-githapi", "test-file.txt", ref = "master")
  expect_match(file_contents, now)

  new_branch_commit <- upload_commit(
    repo    = "ChadGoymer/test-githapi",
    branch  = "test-upload-commit",
    message = "Commit made on new branch with upload_commit",
    path    = system.file("test-data/upload-tree", package = "githapi"),
    parents = "master")
  on.exit(delete_branches(repo = "ChadGoymer/test-githapi", branch = "test-upload-commit"))

  expect_is(new_branch_commit, "tbl")
  expect_identical(
    map_vec(new_branch_commit, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character",
      parent_sha      = "list",
      parent_url      = "list"))

  expect_identical(new_branch_commit$message, c("Commit made on new branch with upload_commit"))
})

# TEST: commit_exists -------------------------------------------------------------------------

test_that("commit_exists returns TRUE or FALSE depending on whether the commit exists in the repo", {
  expect_true(commit_exists("ChadGoymer/test-githapi", "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d"))
  expect_false(commit_exists("ChadGoymer/test-githapi", "0000000000000000000000000000000000000000"))
})
