context("pull requests")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  create_repository(
    name        = str_c("test-pulls-", now),
    description = "This is a repository to test pull requests",
    auto_init   = TRUE)

  # TODO: replace with new `create_file()` function
  create_files(
    paths    = str_c("test-pulls-", now, ".txt"),
    contents = "This is a repository to test pull requests",
    messages = "Commit to test pull requests",
    branches = str_c("test-pulls-1-", now),
    parents  = "master",
    repo     = str_c("ChadGoymer/test-pulls-", now))

  create_files(
    paths    = str_c("test-pulls-", now, ".txt"),
    contents = "This is a repository to test pull requests",
    messages = "Commit to test pull requests",
    branches = str_c("test-pulls-2-", now),
    parents  = "master",
    repo     = str_c("ChadGoymer/test-pulls-", now))

  create_milestone(
    title       = str_c("test-pulls-", now),
    repo        = str_c("ChadGoymer/test-pulls-", now),
    description = "This is a milestone to test pull requests")

  create_label(
    name        = str_c("test-pulls-", now),
    repo        = str_c("ChadGoymer/test-pulls-", now),
    description = "This is a label to test pull requests")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-pulls-", now))

})))


# TEST: create_pull_request -------------------------------------------------------------------

test_that("create_pull_request creates a pull request and returns its properties", {

  pull_request <- create_pull_request(
    title = str_c("test pull request ", now),
    repo  = str_c("ChadGoymer/test-pulls-", now),
    head  = str_c("test-pulls-1-", now),
    base  = "master",
    body  = "This is a pull request to test create_pull_request()")

  expect_is(pull_request, "list")
  expect_identical(attr(pull_request, "status"), 201L)
  expect_identical(
    map_chr(pull_request, ~ class(.)[[1]]),
    c(number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      html_url   = "character",
      diff_url   = "character",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"))

  expect_identical(pull_request$title, str_c("test pull request ", now))
  expect_identical(pull_request$repository, str_c("ChadGoymer/test-pulls-", now))
  expect_identical(pull_request$head_ref, str_c("test-pulls-1-", now))
  expect_identical(pull_request$base_ref, "master")
  expect_identical(pull_request$body, "This is a pull request to test create_pull_request()")
  expect_identical(pull_request$state, "open")

  assigned_pull_request <- create_pull_request(
    title     = str_c("test assigned pull request ", now),
    repo      = str_c("ChadGoymer/test-pulls-", now),
    head      = str_c("test-pulls-2-", now),
    base      = "master",
    body      = "This is a pull request to test create_pull_request()",
    assignees = "ChadGoymer",
    labels    = str_c("test-pulls-", now),
    milestone = str_c("test-pulls-", now))

  expect_is(assigned_pull_request, "list")
  expect_identical(attr(assigned_pull_request, "status"), 201L)
  expect_identical(
    map_chr(assigned_pull_request, ~ class(.)[[1]]),
    c(number     = "integer",
      title      = "character",
      body       = "character",
      head_sha   = "character",
      head_ref   = "character",
      head_repo  = "character",
      base_sha   = "character",
      base_ref   = "character",
      merge_sha  = "character",
      assignees  = "character",
      reviewers  = "character",
      labels     = "character",
      milestone  = "character",
      state      = "character",
      repository = "character",
      html_url   = "character",
      diff_url   = "character",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      mergeable  = "logical",
      rebaseable = "logical",
      merged     = "logical",
      merged_by  = "character",
      merged_at  = "POSIXct",
      closed_at  = "POSIXct"))

  expect_identical(assigned_pull_request$title, str_c("test assigned pull request ", now))
  expect_identical(assigned_pull_request$repository, str_c("ChadGoymer/test-pulls-", now))
  expect_identical(assigned_pull_request$head_ref, str_c("test-pulls-2-", now))
  expect_identical(assigned_pull_request$base_ref, "master")
  expect_identical(assigned_pull_request$body, "This is a pull request to test create_pull_request()")
  expect_identical(assigned_pull_request$state, "open")
  expect_identical(assigned_pull_request$assignees, "ChadGoymer")
  expect_identical(assigned_pull_request$labels, str_c("test-pulls-", now))
  expect_identical(assigned_pull_request$milestone, str_c("test-pulls-", now))

  # TODO: Add a member to HairyCoos to test reviewers

})
