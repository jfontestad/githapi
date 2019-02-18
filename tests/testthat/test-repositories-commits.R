context("repositories commits")

# TEST: view_history --------------------------------------------------------------------------

test_that("view_history returns a tibble describing the history of commits", {
  commits <- view_history("ChadGoymer/test-githapi", n_max = 10)

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

  first_commits <- view_history("310c21d3f1601a46e014e68e94814b23406bf574", "ChadGoymer/test-githapi")

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
    first_commits$sha,
    c("310c21d3f1601a46e014e68e94814b23406bf574", "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d"))
})

# TEST: view_shas -----------------------------------------------------------------------------

test_that("view_shas returns a named character vector with the SHAs", {
  shas <- view_shas(c("0.0.0", "master"), "ChadGoymer/test-githapi")

  expect_is(shas, "character")
  expect_identical(names(shas), c("0.0.0", "master"))
  expect_identical(shas[["0.0.0"]], "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d")
})

# TEST: shas_exist ----------------------------------------------------------------------------

test_that("shas_exist returns TRUE is the commit exists and FALSE otherwise", {
  expect_true(shas_exist("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "ChadGoymer/test-githapi"))
  expect_false(shas_exist("0000000000000000000000000000000000000000", "ChadGoymer/test-githapi"))

  expect_identical(
    shas_exist(c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "0000000000000000000000000000000000000000"), "ChadGoymer/test-githapi"),
    c(`cbd94cf24a4c62761b3ae59ca3c69f868591cf7d` = TRUE, `0000000000000000000000000000000000000000` = FALSE))
})

# TEST: compare_commits -----------------------------------------------------------------------

test_that("compare_commits returns information on the differences between two commits", {
  comparison <- compare_commits(
    base = "0.0.0",
    head = "ccb62ec75de7e40c689be427cd038c8a1a9d3c44",
    repo = "ChadGoymer/test-githapi")

  expect_is(comparison, "tbl")
  expect_identical(
    map_vec(comparison, function(field) class(field)[[1]]),
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
    comparison$sha,
    c("310c21d3f1601a46e014e68e94814b23406bf574",
      "32d3c5c4f6aba7ae9679480407e1b9f94ad04843",
      "68f01be0dad53f366337c9d87fad939b2a2853c8",
      "ccb62ec75de7e40c689be427cd038c8a1a9d3c44"))
  expect_identical(
    comparison$message,
    c("trying to commit a new file",
      "trying to commit a new file",
      "trying to commit a new file",
      "Testing create_files() - added aaaa.txt"))
})

# TEST: compare_files -------------------------------------------------------------------------

test_that("compare_files returns a tibble of information of file differences between commits", {
  comparison <- compare_files(
    base = "0.0.0",
    head = "ccb62ec75de7e40c689be427cd038c8a1a9d3c44",
    repo = "ChadGoymer/test-githapi")

  expect_is(comparison, "tbl")

  expect_identical(
    map_vec(comparison, function(field) class(field)[[1]]),
    c(filename     = "character",
      sha          = "character",
      status       = "character",
      additions    = "integer",
      deletions    = "integer",
      changes      = "integer",
      raw_url      = "character",
      contents_url = "character",
      patch        = "character"))

  expect_identical(comparison$filename, c("aaaa.txt", "test-file.txt"))
  expect_identical(
    comparison$sha,
    c("a743ebf5f42459a2f59eb85b92184c6f0fef4634", "5c3ff51b2f189a42016557ef6c1c0c79acca8fc7"))
  expect_identical(comparison$status, c("added", "added"))
  expect_identical(comparison$additions, c(3L, 30L))
  expect_identical(comparison$deletions, c(0L, 0L))
  expect_identical(comparison$changes, c(3L, 30L))
})
