context("milestones")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_repo <- create_repository(
    name        = paste0("test-milestones-", now),
    description = "This is a repository to test milestones")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(paste0("ChadGoymer/test-milestones-", now))

})))


# TEST: create_milestone ----------------------------------------------------------------------

test_that("create_milestone creates a milestone and returns a list of the properties", {

  simple_milestone <- create_milestone(
    title = paste("test simple milestone", now),
    repo  = paste0("ChadGoymer/test-milestones-", now))

  expect_is(simple_milestone, "list")
  expect_identical(attr(simple_milestone, "status"), 201L)
  expect_identical(
    map_chr(simple_milestone, ~ class(.)[[1]]),
    c(id            = "integer",
      number        = "integer",
      title         = "character",
      description   = "character",
      state         = "character",
      open_issues   = "integer",
      closed_issues = "integer",
      html_url      = "character",
      creator       = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct",
      due_on        = "POSIXct",
      closed_at     = "POSIXct"))

  expect_identical(simple_milestone$title, paste("test simple milestone", now))
  expect_identical(simple_milestone$state, "open")

  detailed_milestone <- create_milestone(
    title       = paste("test detailed milestone", now),
    repo        = paste0("ChadGoymer/test-milestones-", now),
    description = "This is a test milestone",
    due_on      = format(Sys.Date() + 1))

  expect_is(detailed_milestone, "list")
  expect_identical(attr(detailed_milestone, "status"), 201L)
  expect_identical(
    map_chr(detailed_milestone, ~ class(.)[[1]]),
    c(id            = "integer",
      number        = "integer",
      title         = "character",
      description   = "character",
      state         = "character",
      open_issues   = "integer",
      closed_issues = "integer",
      html_url      = "character",
      creator       = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct",
      due_on        = "POSIXct",
      closed_at     = "POSIXct"))

  expect_identical(detailed_milestone$title, paste("test detailed milestone", now))
  expect_identical(detailed_milestone$description, "This is a test milestone")
  expect_identical(format(detailed_milestone$due_on, "%Y-%m-%d"), format(Sys.Date() + 1))
  expect_identical(detailed_milestone$state, "open")

})
