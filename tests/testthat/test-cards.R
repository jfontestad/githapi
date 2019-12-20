context("cards api")


setup(suppressMessages({

  test_project <- create_project(
    name = "Test cards",
    body = "A project to test card functions",
    repo = "ChadGoymer/test-githapi")

  test_column <- create_column(
    name    = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

}))

teardown(suppressMessages(tryCatch({

  delete_column(
    column  = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

  delete_column(
    column  = "Test cards 2",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

  delete_project(
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

})))


# TEST: create_card ------------------------------------------------------------------------

test_that("create_cards creates a card and returns its properties", {

  issue_card <- create_card(
    content_id   = 1,
    content_type = "Issue",
    column       = "Test cards",
    project      = "Test cards",
    repo         = "ChadGoymer/test-githapi")

  expect_is(issue_card, "list")
  expect_identical(attr(issue_card, "status"), 201L)
  expect_identical(
    map_chr(issue_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(issue_card$content_id, 1L)

  pull_card <- create_card(
    content_id   = 2,
    content_type = "PullRequest",
    column       = "Test cards",
    project      = "Test cards",
    repo         = "ChadGoymer/test-githapi")

  expect_is(pull_card, "list")
  expect_identical(attr(pull_card, "status"), 201L)
  expect_identical(
    map_chr(pull_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(pull_card$content_id, 2L)

  note_card <- create_card(
    note    = "Note Title\nThis is a note",
    column  = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

  expect_is(note_card, "list")
  expect_identical(attr(note_card, "status"), 201L)
  expect_identical(
    map_chr(note_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(note_card$note, "Note Title\nThis is a note")

})

test_that("create_card throws an error in invalid arguments are supplied", {

  expect_error(
    create_card(
      column  = "Test cards",
      project = "Test cards",
      repo    = "ChadGoymer/test-githapi"),
    "Either 'content_id' or 'note' must be supplied")

})
