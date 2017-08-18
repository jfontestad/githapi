context("projects api")

#  FUNCTION: gh_project -----------------------------------------------------------------------
test_that("gh_project returns a list describing the project", {
  project <- gh_project(747228)
  expect_is(project, "list")
  expect_identical(
    names(project),
    c("owner_url", "url", "html_url", "columns_url", "id", "name", "body",
      "number", "state", "creator", "created_at", "updated_at"))
  expect_identical(project$name, "githapi")
})

#  FUNCTION: gh_projects ----------------------------------------------------------------------
test_that("gh_projects returns a tibble describing the projects", {
  projects <- gh_projects("ChadGoymer/githapi")
  expect_is(projects, "tbl")
  expect_identical(
    names(projects),
    c("id", "number", "name", "body", "state", "creator_login", "created_at", "updated_at", "url"))
  expect_true("githapi" %in% projects$name)
})

#  FUNCTION: gh_column ------------------------------------------------------------------------
test_that("gh_column returns a list describing the column", {
  column <- gh_column(1310204)
  expect_is(column, "list")
  expect_identical(
    names(column),
    c("url", "project_url", "cards_url", "id", "name", "created_at", "updated_at"))
  expect_identical(column$name, "To do")
})

#  FUNCTION: gh_columns -----------------------------------------------------------------------
test_that("gh_columns returns a tibble describing the columns", {
  columns <- gh_columns(747228)
  expect_is(columns, "tbl")
  expect_identical(names(columns), c("id", "name", "created_at", "updated_at", "url"))
  expect_true(all(c("To do", "In progress", "Done") %in% columns$name))
})

#  FUNCTION: gh_cards -------------------------------------------------------------------------
test_that("gh_cards returns a tibble describing the cards", {
  cards <- gh_cards(1310204)
  expect_is(cards, "tbl")
  expect_identical(names(cards), c("id", "creator_login", "created_at", "updated_at", "url"))
  expect_true("ChadGoymer" %in% cards$creator_login)
})
