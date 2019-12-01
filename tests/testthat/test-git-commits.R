context("git commits")

git_commits_branch <- str_c("test-git-commits-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
master_sha <- view_shas(refs = "master", repo = "ChadGoymer/test-githapi")[[1]]

setup({
  create_branches(
    branches = git_commits_branch,
    shas     = master_sha,
    repo     = "ChadGoymer/test-githapi")
})

teardown({
  delete_branches(
    branches = git_commits_branch,
    repo     = "ChadGoymer/test-githapi")
})


# TEST: view_commits --------------------------------------------------------------------------

test_that("view_commits returns a tibble of information about the commits", {
  commits <- view_commits(
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "ccb62ec75de7e40c689be427cd038c8a1a9d3c44"),
    repo = "ChadGoymer/test-githapi")

  expect_is(commits, "tbl")
  expect_identical(
    gh_map(commits, function(field) class(field)[[1]], simplify = TRUE),
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
    message = "This is created with create_commit()",
    tree    = "bbac77ba8fc1afa9a815a0cc8fc17e221cb0c027",
    parents = git_commits_branch,
    repo    = "ChadGoymer/test-githapi")

  expect_is(created_commit, "tbl")
  expect_identical(
    gh_map(created_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  viewed_commit <- view_commits(created_commit$sha, "ChadGoymer/test-githapi")
  expect_identical(created_commit, viewed_commit)

  orphan_commit <- create_commit(
    message = "This is an orphan created with create_commit()",
    tree    = "bbac77ba8fc1afa9a815a0cc8fc17e221cb0c027",
    repo    = "ChadGoymer/test-githapi")

  expect_is(orphan_commit, "tbl")
  expect_identical(
    gh_map(orphan_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  expect_null(orphan_commit$parent_sha[[1]])
  expect_null(orphan_commit$parent_url[[1]])
})

# TEST: upload_commit -------------------------------------------------------------------------

test_that("upload_commit uploads files and directory structure to github", {
  skip_on_travis()

  flat_commit <- upload_commit(
    branch  = git_commits_branch,
    message = "Test commit made with upload_commit",
    path    = system.file("test-data/upload-tree/test-dir", package = "githapi"),
    parents = git_commits_branch,
    repo    = "ChadGoymer/test-githapi")

  expect_is(flat_commit, "tbl")
  expect_identical(
    gh_map(flat_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  expect_identical(flat_commit$message, "Test commit made with upload_commit")

  master_files <- view_trees(flat_commit$sha, "ChadGoymer/test-githapi")
  expect_identical(master_files$path, c("file-in-dir-1.txt", "file-in-dir-2.txt"))

  recursive_commit <- upload_commit(
    branch  = git_commits_branch,
    message = "Recursive commit made with upload_commit",
    path    = system.file("test-data/upload-tree", package = "githapi"),
    parents = git_commits_branch,
    repo    = "ChadGoymer/test-githapi")

  expect_is(recursive_commit, "tbl")
  expect_identical(
    gh_map(recursive_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  expect_identical(recursive_commit$message, "Recursive commit made with upload_commit")

  master_files <- view_trees(recursive_commit$sha, "ChadGoymer/test-githapi")
  expect_identical(
    master_files$path,
    c("README.md",
      "test-dir",
      "test-dir/file-in-dir-1.txt",
      "test-dir/file-in-dir-2.txt",
      "test-file.txt"))

  temp_path <- file.path(tempdir(), "test-git-commits")
  if (!dir.exists(temp_path)) dir.create(temp_path)
  on.exit(unlink(temp_path, recursive = TRUE))

  download_files(
    paths    = "test-file.txt",
    location = temp_path,
    ref      = git_commits_branch,
    repo     = "ChadGoymer/test-githapi")

  temp_file <- file.path(temp_path, "test-file.txt")
  now <- as.character(Sys.time())
  writeLines(c("This file was updated:", now), temp_file)

  updated_commit <- upload_commit(
    branch    = git_commits_branch,
    message   = "Update commit made with upload_commit",
    path      = temp_path,
    parents   = git_commits_branch,
    repo      = "ChadGoymer/test-githapi",
    replace   = FALSE)

  expect_is(updated_commit, "tbl")
  expect_identical(
    gh_map(updated_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  expect_identical(updated_commit$message, "Update commit made with upload_commit")

  master_files <- view_trees(updated_commit$sha, "ChadGoymer/test-githapi")
  expect_identical(
    master_files$path,
    c("README.md",
      "test-dir",
      "test-dir/file-in-dir-1.txt",
      "test-dir/file-in-dir-2.txt",
      "test-file.txt"))

  file_contents <- read_files("test-file.txt", ref = git_commits_branch, repo = "ChadGoymer/test-githapi")
  expect_match(file_contents, now)

  new_branch_commit <- upload_commit(
    branch  = "test-upload-commit",
    message = "Commit made on new branch with upload_commit",
    path    = system.file("test-data/upload-tree", package = "githapi"),
    parents = git_commits_branch,
    repo    = "ChadGoymer/test-githapi")
  on.exit(delete_branches("test-upload-commit", "ChadGoymer/test-githapi"), add = TRUE)

  expect_is(new_branch_commit, "tbl")
  expect_identical(
    gh_map(new_branch_commit, function(field) class(field)[[1]], simplify = TRUE),
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

  expect_identical(new_branch_commit$message, "Commit made on new branch with upload_commit")
})

# TEST: commits_exist -------------------------------------------------------------------------

test_that("commits_exist returns TRUE or FALSE depending on whether the commit exists in the repo", {
  expect_true(commits_exist("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "ChadGoymer/test-githapi"))
  expect_false(commits_exist("0000000000000000000000000000000000000000", "ChadGoymer/test-githapi"))

  expect_identical(
    commits_exist(c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "0000000000000000000000000000000000000000"), "ChadGoymer/test-githapi"),
    c(`cbd94cf24a4c62761b3ae59ca3c69f868591cf7d` = TRUE, `0000000000000000000000000000000000000000` = FALSE))
})
