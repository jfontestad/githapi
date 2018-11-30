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
