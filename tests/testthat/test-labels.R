context("labels")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_repo <- create_repository(
    name        = paste0("test-labels-", now),
    description = "This is a repository to test labels")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(paste0("ChadGoymer/test-labels-", now))

})))


# TEST: create_label --------------------------------------------------------------------------

test_that("create_label creates a label and returns a list of the properties", {

  simple_label <- create_label(
    name  = "simple-label",
    repo  = paste0("ChadGoymer/test-labels-", now),
    color = "blue")

  expect_is(simple_label, "list")
  expect_identical(attr(simple_label, "status"), 201L)
  expect_identical(
    map_chr(simple_label, ~ class(.)[[1]]),
    c(name        = "character",
      color       = "character",
      description = "character"))

  expect_identical(simple_label$name, "simple-label")
  expect_identical(simple_label$color, "0000FF")

  detailed_label <- create_label(
    name        = "detailed-label",
    repo        = paste0("ChadGoymer/test-labels-", now),
    color       = "green",
    description = "This is a detailed label")

  expect_is(detailed_label, "list")
  expect_identical(attr(detailed_label, "status"), 201L)
  expect_identical(
    map_chr(detailed_label, ~ class(.)[[1]]),
    c(name        = "character",
      color       = "character",
      description = "character"))

  expect_identical(detailed_label$name, "detailed-label")
  expect_identical(detailed_label$color, "00FF00")
  expect_identical(detailed_label$description, "This is a detailed label")

})


# TEST: update_label --------------------------------------------------------------------------

test_that("update_label changes a label and returns a list of the properties", {

  updated_label <- update_label(
    label       = "simple-label",
    repo        = paste0("ChadGoymer/test-labels-", now),
    name        = "updated-label",
    color       = "pink",
    description = "This is an updated label")

  expect_is(updated_label, "list")
  expect_identical(attr(updated_label, "status"), 200L)
  expect_identical(
    map_chr(updated_label, ~ class(.)[[1]]),
    c(name        = "character",
      color       = "character",
      description = "character"))

  expect_identical(updated_label$name, "updated-label")
  expect_identical(updated_label$color, "FFC0CB")
  expect_identical(updated_label$description, "This is an updated label")

})


# TEST: view_labels ---------------------------------------------------------------------------

test_that("view_labels returns a tibble of label properties", {

  labels <- view_labels(paste0("ChadGoymer/test-labels-", now))

  expect_is(labels, "tbl")
  expect_identical(attr(labels, "status"), 200L)
  expect_identical(
    map_chr(labels, ~ class(.)[[1]]),
    c(name        = "character",
      color       = "character",
      description = "character"))

  expect_true("detailed-label" %in% labels$name)

})
