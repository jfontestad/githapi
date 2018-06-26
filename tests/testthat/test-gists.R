context("gists api")

#  FUNCTION: gh_gist --------------------------------------------------------------------------
test_that("gh_gist returns a list describing the gist", {
  gist <- gh_gist("806dca6b09a39e7b6326a0c8137583e6")
  expect_is(gist, "list")
  expect_named(
    gist,
    c("url", "forks_url", "commits_url", "id", "node_id", "git_pull_url", "git_push_url",
      "html_url","files", "public", "created_at", "updated_at", "description", "comments",
      "user", "comments_url", "owner", "forks", "history", "truncated"))
  expect_identical(gist$description, "An R script to test GitHub's Gist API")

  gist_sha <- gh_gist("806dca6b09a39e7b6326a0c8137583e6", "4a239d53a0e38a3dd2b70e0cd5c7cb316369ab9f")
  expect_is(gist_sha, "list")
  expect_named(
    gist_sha,
    c("url", "forks_url", "commits_url", "id", "node_id", "git_pull_url", "git_push_url",
      "html_url","files", "public", "created_at", "updated_at", "description", "comments",
      "user", "comments_url", "owner", "forks", "history", "truncated"))
  expect_identical(gist_sha$description, "An R script to test GitHub's Gist API")
})

#  FUNCTION: gh_gists -------------------------------------------------------------------------
test_that("gh_gists returns a tibble describing the gists", {
  auth_user_gists <- gh_gists()
  expect_is(auth_user_gists, "tbl")

  expect_identical(
    sapply(auth_user_gists, function(field) class(field)[[1]]),
    c(id          = "character",
      description = "character",
      owner_login = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct",
      comments    = "integer",
      files       = "list",
      public      = "logical",
      url         = "character"))

  expect_identical(
    sapply(auth_user_gists$files[[1]], function(field) class(field)[[1]]),
    c(filename = "character",
      language = "character",
      size     = "integer"))

  expect_true("An R script to test GitHub's Gist API" %in% auth_user_gists$description)

  user_gists <- gh_gists("ChadGoymer")
  expect_is(user_gists, "tbl")

  expect_identical(
    sapply(user_gists, function(field) class(field)[[1]]),
    c(id          = "character",
      description = "character",
      owner_login = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct",
      comments    = "integer",
      files       = "list",
      public      = "logical",
      url         = "character"))

  expect_true("An R script to test GitHub's Gist API" %in% user_gists$description)

  public_gists <- gh_gists("public", n_max = 10)
  expect_is(public_gists, "tbl")

  expect_identical(
    sapply(public_gists, function(field) class(field)[[1]]),
    c(id          = "character",
      description = "character",
      owner_login = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct",
      comments    = "integer",
      files       = "list",
      public      = "logical",
      url         = "character"))

  expect_identical(nrow(public_gists), 10L)

  starred_gists <- gh_gists("starred")
  expect_is(starred_gists, "tbl")

  expect_identical(
    sapply(starred_gists, function(field) class(field)[[1]]),
    c(id          = "character",
      description = "character",
      owner_login = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct",
      comments    = "integer",
      files       = "list",
      public      = "logical",
      url         = "character"))

  expect_true("An R script to test GitHub's Gist API" %in% starred_gists$description)
})

#  FUNCTION: gh_gist_commits ------------------------------------------------------------------
test_that("gh_gist_commits returns a tibble describing the gist commits", {
  git_commits <- gh_gist_commits("806dca6b09a39e7b6326a0c8137583e6")
  expect_is(git_commits, "tbl")

  expect_identical(
    sapply(git_commits, function(field) class(field)[[1]]),
    c(version           = "character",
      user_login        = "character",
      committed_at      = "POSIXct",
      changes_total     = "integer",
      changes_additions = "integer",
      changes_deletions = "integer",
      url               = "character"))

  expect_true("4a239d53a0e38a3dd2b70e0cd5c7cb316369ab9f" %in% git_commits$version)
})

#  FUNCTION: is_gist_starred ------------------------------------------------------------------
test_that("is_gist_starred returns TRUE if the gist has been starred, FALSE otherwise", {
  expect_true(is_gist_starred("806dca6b09a39e7b6326a0c8137583e6"))
  expect_false(is_gist_starred("8f4589b02a513914584b28593b9cdcda"))
})

#  FUNCTION: gh_gist_forks --------------------------------------------------------------------
test_that("gh_gist_forks returns a tibble describing the forks", {
  forks <- gh_gist_forks("109311bb0361f32d87a2", n_max = 10)
  expect_is(forks, "tbl")
  expect_identical(
    sapply(forks, function(field) class(field)[[1]]),
    c(id          = "character",
      description = "character",
      owner_login = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct",
      public      = "logical",
      comments    = "integer",
      url         = "character"))
})

#  FUNCTION: gh_gist_comment ------------------------------------------------------------------
test_that("gh_gist_comment returns a list describing the gist comment", {
  comment <- gh_gist_comment(2185561, "806dca6b09a39e7b6326a0c8137583e6")
  expect_is(comment, "list")
  expect_named(
    comment,
    c("url", "id", "node_id", "user", "author_association", "created_at", "updated_at", "body"))
  expect_identical(comment$body, "This is a comment about the test gist.")
})

#  FUNCTION: gh_gist_comments -----------------------------------------------------------------
test_that("gh_gist_comments returns a tibble describing the gist comments", {
  comments <- gh_gist_comments("806dca6b09a39e7b6326a0c8137583e6")
  expect_is(comments, "tbl")

  expect_identical(
    sapply(comments, function(field) class(field)[[1]]),
    c(id         = "integer",
      body       = "character",
      user_login = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      url        = "character"))

  expect_true("This is a comment about the test gist." %in% comments$body)
})

#  FUNCTION: gh_save_gist -------------------------------------------------------------------------
test_that("gh_save_gist downloads the specified files", {
  temp_path <- file.path(tempdir(), "test-gh_save_gist")
  on.exit(unlink(temp_path, recursive = TRUE))

  gh_save_gist("806dca6b09a39e7b6326a0c8137583e6", temp_path, files = "test-gist.R")
  expect_true(file.exists(file.path(temp_path, "test-gist.R")))
  expect_identical(
    readLines(file.path(temp_path, "test-gist.R")),
    c("test_gist <- function() {", "  \"this is a test gist\"", "}"))

  gh_save_gist("806dca6b09a39e7b6326a0c8137583e6", temp_path)
  expect_true(all(file.exists(file.path(temp_path, c("test-gist.R", "another-test-gist.R")))))
  expect_identical(
    readLines(file.path(temp_path, "another-test-gist.R")),
    c("test_gist <- function() {", "  \"this is another test gist\"", "}"))
})

#  FUNCTION: gh_source_gist -------------------------------------------------------------------
test_that("gh_source_gist sources a file in a gist", {
  gh_source_gist("test-gist.R", "806dca6b09a39e7b6326a0c8137583e6")
  expect_true(exists("test_gist"))
  expect_is(test_gist, "function")
  expect_identical(test_gist(), "this is a test gist")
})
