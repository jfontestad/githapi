context("milestones")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_repo <- create_repository(
    name        = str_c("test-milestones-", now),
    description = "This is a repository to test milestones")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-milestones-", now))

})))


# TEST: create_milestone ----------------------------------------------------------------------

test_that("create_milestone creates a milestone and returns a list of the properties", {

  simple_milestone <- create_milestone(
    title = str_c("test simple milestone ", now),
    repo  = str_c("ChadGoymer/test-milestones-", now))

  expect_is(simple_milestone, "list")
  expect_identical(attr(simple_milestone, "status"), 201L)
  expect_identical(
    map_chr(simple_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(simple_milestone$title, str_c("test simple milestone ", now))
  expect_identical(simple_milestone$state, "open")

  detailed_milestone <- create_milestone(
    title       = str_c("test detailed milestone ", now),
    repo        = str_c("ChadGoymer/test-milestones-", now),
    description = "This is a test milestone",
    due_on      = format(Sys.Date() + 1))

  expect_is(detailed_milestone, "list")
  expect_identical(attr(detailed_milestone, "status"), 201L)
  expect_identical(
    map_chr(detailed_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(detailed_milestone$title, str_c("test detailed milestone ", now))
  expect_identical(detailed_milestone$description, "This is a test milestone")
  expect_identical(format(detailed_milestone$due_on, "%Y-%m-%d"), format(Sys.Date() + 1))
  expect_identical(detailed_milestone$state, "open")

})


# TEST: update_milestone ----------------------------------------------------------------------

test_that("update_milestone changes a milestone and returns a list of the properties", {

  updated_milestone <- update_milestone(
    milestone   = str_c("test simple milestone ", now),
    repo        = str_c("ChadGoymer/test-milestones-", now),
    title       = str_c("test updated milestone ", now),
    description = "This is an updated test milestone",
    due_on      = format(Sys.Date() + 28))

  expect_is(updated_milestone, "list")
  expect_identical(attr(updated_milestone, "status"), 200L)
  expect_identical(
    map_chr(updated_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(updated_milestone$title, str_c("test updated milestone ", now))
  expect_identical(updated_milestone$description, "This is an updated test milestone")
  expect_identical(format(updated_milestone$due_on, "%Y-%m-%d"), format(Sys.Date() + 28))

  closed_milestone <- update_milestone(
    milestone = str_c("test updated milestone ", now),
    repo      = str_c("ChadGoymer/test-milestones-", now),
    state     = "closed")

  expect_is(closed_milestone, "list")
  expect_identical(attr(closed_milestone, "status"), 200L)
  expect_identical(
    map_chr(closed_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(closed_milestone$state, "closed")

})


# TEST: view_milestones -----------------------------------------------------------------------

test_that("view_milestones returns a tibble of milestone properties", {

  milestones <- view_milestones(str_c("ChadGoymer/test-milestones-", now))

  expect_is(milestones, "tbl")
  expect_identical(attr(milestones, "status"), 200L)
  expect_identical(
    map_chr(milestones, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_true(str_c("test detailed milestone ", now) %in% milestones$title)

  closed_milestones <- view_milestones(
    repo  = str_c("ChadGoymer/test-milestones-", now),
    state = "closed")

  expect_is(closed_milestones, "tbl")
  expect_identical(attr(closed_milestones, "status"), 200L)
  expect_identical(
    map_chr(closed_milestones, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_true(str_c("test updated milestone ", now) %in% closed_milestones$title)

})


# TEST: view_milestone ------------------------------------------------------------------------

test_that("view_milestone returns a list of repository properties", {

  first_milestone <- view_milestone(1, repo = str_c("ChadGoymer/test-milestones-", now))

  expect_is(first_milestone, "list")
  expect_identical(attr(first_milestone, "status"), 200L)
  expect_identical(
    map_chr(first_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(first_milestone$number, 1L)

  named_milestone <- view_milestone(
    milestone = str_c("test detailed milestone ", now),
    repo      = str_c("ChadGoymer/test-milestones-", now))

  expect_is(named_milestone, "list")
  expect_identical(attr(named_milestone, "status"), 200L)
  expect_identical(
    map_chr(named_milestone, ~ class(.)[[1]]),
    c(number        = "integer",
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

  expect_identical(named_milestone$title, str_c("test detailed milestone ", now))

})

test_that("view_milestone throws an error if invalid arguments are supplied", {

  expect_error(
    view_milestone(TRUE, repo = str_c("ChadGoymer/test-milestones-", now)),
    "'milestone' must be either an integer or a string")

})


# TEST: browse_milestone ----------------------------------------------------------------------

test_that("browse_milestone opens the milestone's page in the browser", {

  skip_if(!interactive(), "browse_milestone must be tested manually")

  milestone_url <- browse_milestone(1, repo = str_c("ChadGoymer/test-milestones-", now))

  expect_is(milestone_url, "character")
  expect_identical(attr(milestone_url, "status"), 200L)
  expect_identical(
    as.character(milestone_url),
    str_c("https://github.com/ChadGoymer/test-milestones-", now, "/milestone/1"))

})


# TEST: delete_milestone ----------------------------------------------------------------------

test_that("delete_milestone removes a milestone and returns TRUE", {

  first_milestone <- delete_milestone(1, repo = str_c("ChadGoymer/test-milestones-", now))

  expect_is(first_milestone, "logical")
  expect_identical(attr(first_milestone, "status"), 204L)
  expect_identical(as.logical(first_milestone), TRUE)

  named_milestone <- delete_milestone(
    milestone = str_c("test detailed milestone ", now),
    repo      = str_c("ChadGoymer/test-milestones-", now))

  expect_is(named_milestone, "logical")
  expect_identical(attr(named_milestone, "status"), 204L)
  expect_identical(as.logical(named_milestone), TRUE)

})
