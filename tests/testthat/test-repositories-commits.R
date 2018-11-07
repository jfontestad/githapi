context("repositories commits")

# TEST: view_commits --------------------------------------------------------------------------

test_that("view_commits returns a tibble describing the history of commits", {
  commits <- view_commits("ChadGoymer/test-githapi", n_max = 10)

  expect_is(commits, "tbl")
  expect_identical(
    sapply(commits, function(field) class(field)[[1]]),
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
      parent_sha      = "character",
      parent_url      = "character"))

  first_commits <- view_commits("ChadGoymer/test-githapi", ref = "310c21d3f1601a46e014e68e94814b23406bf574")

  expect_is(commits, "tbl")
  expect_identical(
    sapply(commits, function(field) class(field)[[1]]),
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
      parent_sha      = "character",
      parent_url      = "character"))
  expect_identical(
    first_commits$sha,
    c("310c21d3f1601a46e014e68e94814b23406bf574", "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d"))
})
