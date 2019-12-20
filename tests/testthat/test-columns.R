context("columns api")


setup(suppressMessages({

  test_project <- create_project(
    name = "Test columns",
    body = "A project to test columns functions",
    repo = "ChadGoymer/test-githapi")

}))

teardown(suppressMessages(tryCatch({

  delete_project(
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

})))


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


# TEST: update_column ------------------------------------------------------------------------

test_that("update_column updates a column and returns a list of the new properties", {

  column <- update_column(
    column  = "Test column",
    name    = "Updated test column",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 200L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(column$name, "Updated test column")

})


# TEST: move_column ---------------------------------------------------------------------------

test_that("move_column changes the position of a column", {

  column2 <- create_column(
    name    = "Test column 2",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  first_column <- move_column(
    column   = "Test column 2",
    position = "first",
    project  = "Test columns",
    repo     = "ChadGoymer/test-githapi")

  expect_is(first_column, "list")
  expect_identical(attr(first_column, "status"), 201L)
  expect_identical(
    map_chr(first_column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  last_column <- move_column(
    column   = "Test column 2",
    position = "last",
    project  = "Test columns",
    repo     = "ChadGoymer/test-githapi")

  expect_is(last_column, "list")
  expect_identical(attr(last_column, "status"), 201L)
  expect_identical(
    map_chr(last_column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  after_column <- move_column(
    column  = "Updated test column",
    after   = "Test column 2",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(after_column, "list")
  expect_identical(attr(after_column, "status"), 201L)
  expect_identical(
    map_chr(after_column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

})

test_that("move_column throws an error in invalid arguments are supplied", {

  expect_error(
    move_column("Test column 2", project = "Test columns", repo = "ChadGoymer/test-githapi"),
    "Either 'position' or 'after' must be supplied")

})


# TEST: view_columns -------------------------------------------------------------------------

test_that("view_columns returns a tibble summarising the columns", {

  columns <- view_columns("Test columns", "ChadGoymer/test-githapi")

  expect_is(columns, "tbl")
  expect_identical(attr(columns, "status"), 200L)
  expect_identical(
    map_chr(columns, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_true("Updated test column" %in% columns$name)

})


# TEST: view_column --------------------------------------------------------------------------

test_that("view_column returns a list of column properties", {

  column <- view_column(
    column  = "Updated test column",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(column, "list")
  expect_identical(attr(column, "status"), 200L)
  expect_identical(
    map_chr(column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(column$name, "Updated test column")

  column_by_id <- view_column(column = column$id)

  expect_is(column_by_id, "list")
  expect_identical(attr(column_by_id, "status"), 200L)
  expect_identical(
    map_chr(column_by_id, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(column_by_id$id, column$id)

})

test_that("view_column can accept a column number", {

  columns <- view_columns("Test columns", "ChadGoymer/test-githapi")

  first_column <- view_column(
    columns$id[[1]],
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(first_column, "list")
  expect_identical(attr(first_column, "status"), 200L)
  expect_identical(
    map_chr(first_column, ~ class(.)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(first_column$id, columns$id[[1]])

})

test_that("view_column throws an error if invalid arguments are supplied", {

  expect_error(
    view_column(TRUE, "Test columns", "ChadGoymer/test-githapi"),
    "'column' must be either an integer or a string")

})


# TEST: delete_column ------------------------------------------------------------------------

test_that("delete_column deletes the columns and returns TRUE", {

  column <- delete_column(
    column  = "Updated test column",
    project = "Test columns",
    repo    = "ChadGoymer/test-githapi")

  expect_is(column, "logical")
  expect_identical(attr(column, "status"), 204L)
  expect_identical(as.logical(column), TRUE)

})
