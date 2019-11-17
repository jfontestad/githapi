context("projects api")

#  FUNCTION: gh_project -----------------------------------------------------------------------
test_that("gh_project returns a list describing the project", {
  project <- gh_project(747228)
  expect_is(project, "list")
  expect_named(
    project,
    c("owner_url", "url", "html_url", "columns_url", "id", "node_id", "name", "body",
      "number", "state", "creator", "created_at", "updated_at"))
  expect_identical(project$name, "Prioritisation")
})

#  FUNCTION: gh_projects ----------------------------------------------------------------------
test_that("gh_projects returns a tibble describing the projects", {
  projects <- gh_projects("ChadGoymer/githapi")
  expect_is(projects, "tbl")

  expect_identical(
    sapply(projects, function(field) class(field)[[1]]),
    c(id            = "integer",
      number        = "integer",
      name          = "character",
      body          = "character",
      state         = "character",
      creator_login = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct",
      url           = "character"))

  expect_true("Prioritisation" %in% projects$name)
})

#  FUNCTION: gh_column ------------------------------------------------------------------------
test_that("gh_column returns a list describing the column", {
  column <- gh_column(1310204)
  expect_is(column, "list")
  expect_named(
    column,
    c("url", "project_url", "cards_url", "id", "node_id", "name", "created_at", "updated_at"))
  expect_identical(column$name, "Triage")
})

#  FUNCTION: gh_columns -----------------------------------------------------------------------
test_that("gh_columns returns a tibble describing the columns", {
  columns <- gh_columns(747228)
  expect_is(columns, "tbl")

  expect_identical(
    sapply(columns, function(field) class(field)[[1]]),
    c(id         = "integer",
      name       = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      url        = "character"))

  expect_true(all(c("Triage", "Sometime", "Soon", "Now", "Done") %in% columns$name))
})

#  FUNCTION: gh_card --------------------------------------------------------------------------
test_that("gh_card returns a list describing the card", {
  card <- gh_card(4211067)
  expect_is(card, "list")
  expect_named(
    card,
    c("url", "project_url", "id", "node_id", "note", "archived", "creator", "created_at",
      "updated_at", "column_url", "content_url"))
  expect_identical(
    card$content_url,
    "https://api.github.com/repos/ChadGoymer/githapi/issues/16")
})

#  FUNCTION: gh_cards -------------------------------------------------------------------------
test_that("gh_cards returns a tibble describing the cards", {
  cards <- gh_cards(1310204)
  expect_is(cards, "tbl")

  expect_identical(
    sapply(cards, function(field) class(field)[[1]]),
    c(id            = "integer",
      creator_login = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct",
      content_url   = "character",
      url           = "character"))

  expect_true("ChadGoymer" %in% cards$creator_login)
})
