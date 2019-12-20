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


# TEST: update_card ------------------------------------------------------------------------

suppressMessages({
  cards <- view_cards(
    column  = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")
})

issue_card_id <- filter(cards, .data$content_id == 1) %>% pull(id)
pull_card_id  <- filter(cards, .data$content_id == 2) %>% pull(id)
note_card_id  <- filter(cards, is.na(.data$content_id)) %>% pull(id)

test_that("update_card updates a card and returns a list of the new properties", {

  archived_card <- update_card(card = issue_card_id, archived = TRUE)

  expect_is(archived_card, "list")
  expect_identical(attr(archived_card, "status"), 200L)
  expect_identical(
    map_chr(archived_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_true(archived_card$archived)

  unarchived_card <- update_card(card = issue_card_id, archived = FALSE)

  expect_is(unarchived_card, "list")
  expect_identical(attr(unarchived_card, "status"), 200L)
  expect_identical(
    map_chr(unarchived_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_false(unarchived_card$archived)

  note_card <- update_card(card = note_card_id, note = "Note Title\nThis is an updated note")

  expect_is(note_card, "list")
  expect_identical(attr(note_card, "status"), 200L)
  expect_identical(
    map_chr(note_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(note_card$note, "Note Title\nThis is an updated note")

})


# TEST: move_card ---------------------------------------------------------------------------

test_that("move_card changes the position of a card", {

  first_card <- move_card(card = issue_card_id, position = "top")

  expect_is(first_card, "list")
  expect_identical(attr(first_card, "status"), 201L)
  expect_identical(
    map_chr(first_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  last_card <- move_card(card = note_card_id, position = "bottom")

  expect_is(last_card, "list")
  expect_identical(attr(last_card, "status"), 201L)
  expect_identical(
    map_chr(last_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  after_card <- move_card(card  = pull_card_id, after = note_card_id)

  expect_is(after_card, "list")
  expect_identical(attr(after_card, "status"), 201L)
  expect_identical(
    map_chr(after_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

})

test_that("move_card changes the column a card is in", {

  column2 <- create_column(
    name    = "Test cards 2",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

  column_card <- move_card(
    card     = issue_card_id,
    position = "top",
    column   = "Test cards 2",
    project  = "Test cards",
    repo     = "ChadGoymer/test-githapi")

  expect_is(column_card, "list")
  expect_identical(attr(column_card, "status"), 201L)
  expect_identical(
    map_chr(column_card, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

})

test_that("move_card throws an error in invalid arguments are supplied", {

  expect_error(
    move_card(issue_card_id, repo = "ChadGoymer/test-githapi"),
    "Either 'position' or 'after' must be supplied")

})


# TEST: view_cards -------------------------------------------------------------------------

test_that("view_cards returns a tibble summarising the cards", {

  cards <- view_cards(
    column  = "Test cards",
    project = "Test cards",
    repo    = "ChadGoymer/test-githapi")

  expect_is(cards, "tbl")
  expect_identical(attr(cards, "status"), 200L)
  expect_identical(
    map_chr(cards, ~ class(.)[[1]]),
    c(id         = "integer",
      content_id = "integer",
      note       = "character",
      archived   = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_true(2L %in% cards$content_id)
  expect_true("Note Title\nThis is an updated note" %in% cards$note)

})
