context("columns api")


setup(suppressMessages({
  test_project <- create_project(
    name = "Test columns",
    body = "A project to test columns functions",
    repo = "ChadGoymer/test-githapi")
}))

teardown(suppressMessages({
  delete_project(
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")
}))


# TEST: create_column ------------------------------------------------------------------------

test_that("create_columns creates a column and returns its properties", {

  column <- create_column(
    name    = "Test column",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 201L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(column$name, "Test column")

})
