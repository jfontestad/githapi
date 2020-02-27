context("issues")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  create_repository(
    name        = str_c("test-issues-", now),
    description = "This is a repository to test issues")

  create_repository(
    name        = str_c("test-issues-", now),
    org         = "HairyCoos",
    description = "This is a repository to test issues")

  create_milestone(
    title       = str_c("test-issues-", now),
    repo        = str_c("ChadGoymer/test-issues-", now),
    description = "This is a milestone to test issues")

  create_label(
    name        = str_c("test-issues-", now),
    repo        = str_c("ChadGoymer/test-issues-", now),
    description = "This is a label to test issues")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-issues-", now))
  delete_repository(str_c("HairyCoos/test-issues-", now))

})))


# TEST: create_issue --------------------------------------------------------------------------

test_that("create_issue creates an issue and returns a list of the properties", {

  user_issue <- create_issue(
    title     = str_c("test user issue ", now),
    repo      = str_c("ChadGoymer/test-issues-", now),
    body      = "This is an issue to test create_issue()",
    assignees = "ChadGoymer",
    labels    = str_c("test-issues-", now),
    milestone = str_c("test-issues-", now))

  expect_is(user_issue, "list")
  expect_identical(attr(user_issue, "status"), 201L)
  expect_identical(
    map_chr(user_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(user_issue$title, str_c("test user issue ", now))
  expect_identical(user_issue$body, "This is an issue to test create_issue()")
  expect_identical(user_issue$assignees, "ChadGoymer")
  expect_identical(user_issue$labels, str_c("test-issues-", now))
  expect_identical(user_issue$milestone, str_c("test-issues-", now))
  expect_identical(user_issue$state, "open")
  expect_identical(user_issue$repository, str_c("ChadGoymer/test-issues-", now))
  expect_false(user_issue$pull_request)
  expect_identical(user_issue$creator, "ChadGoymer")

  org_issue <- create_issue(
    title     = str_c("test organization issue ", now),
    repo      = str_c("HairyCoos/test-issues-", now),
    body      = "This is an issue to test create_issue()",
    assignees = "ChadGoymer")

  expect_is(org_issue, "list")
  expect_identical(attr(org_issue, "status"), 201L)
  expect_identical(
    map_chr(org_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(org_issue$title, str_c("test organization issue ", now))
  expect_identical(org_issue$body, "This is an issue to test create_issue()")
  expect_identical(org_issue$assignees, "ChadGoymer")
  expect_identical(org_issue$state, "open")
  expect_identical(org_issue$repository, str_c("HairyCoos/test-issues-", now))
  expect_false(org_issue$pull_request)
  expect_identical(org_issue$creator, "ChadGoymer")

})
