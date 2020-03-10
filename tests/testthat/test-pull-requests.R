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


# TEST: update_pull_request -------------------------------------------------------------------

test_that("update_pull_request updates a pull request and returns its properties", {

  pull_request <- update_pull_request(
    pull_request = str_c("test pull request ", now),
    repo         = str_c("ChadGoymer/test-pulls-", now),
    title        = str_c("test updated pull request ", now),
    body         = "This is an updated pull request to test create_pull_request()",
    state        = "closed",
    base         = "master")

  expect_is(pull_request, "list")
  expect_identical(attr(pull_request, "status"), 200L)
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

  expect_identical(pull_request$title, str_c("test updated pull request ", now))
  expect_identical(pull_request$repository, str_c("ChadGoymer/test-pulls-", now))
  expect_identical(pull_request$body, "This is an updated pull request to test create_pull_request()")
  expect_identical(pull_request$state, "closed")

  assigned_pull_request <- update_pull_request(
    pull_request = 2,
    repo         = str_c("ChadGoymer/test-pulls-", now),
    assignees = "ChadGoymer",
    labels    = str_c("test-pulls-", now),
    milestone = str_c("test-pulls-", now))

  expect_is(assigned_pull_request, "list")
  expect_identical(attr(assigned_pull_request, "status"), 200L)
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

test_that("update_pull_request throws an error if invalid arguments are supplied", {

  expect_error(
    update_pull_request(TRUE, repo = str_c("ChadGoymer/test-pulls-", now)),
    "'pull_request' must be either an integer or a string")

})


# TEST: view_pull_requests --------------------------------------------------------------------

test_that("view_issues returns a tibble of issue properties", {

  open_pull_requests <- view_pull_requests(
    repo = str_c("ChadGoymer/test-pulls-", now),
    base = "master")

  expect_is(open_pull_requests, "tbl")
  expect_identical(attr(open_pull_requests, "status"), 200L)
  expect_identical(
    map_chr(open_pull_requests, ~ class(.)[[1]]),
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

  expect_true(str_c("test assigned pull request ", now) %in% open_pull_requests$title)

  closed_pull_requests <- view_pull_requests(
    repo  = str_c("ChadGoymer/test-pulls-", now),
    head  = str_c("test-pulls-1-", now),
    state = "closed")

  expect_is(closed_pull_requests, "tbl")
  expect_identical(attr(closed_pull_requests, "status"), 200L)
  expect_identical(
    map_chr(closed_pull_requests, ~ class(.)[[1]]),
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

  expect_true(str_c("test updated pull request ", now) %in% closed_pull_requests$title)

})
