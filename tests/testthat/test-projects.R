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

#  FUNCTION: gh_columns -----------------------------------------------------------------------
test_that("gh_columns returns a tibble describing the columns", {
  columns <- gh_columns(747228)
  expect_is(columns, "tbl")
  expect_identical(names(columns), c("id", "name", "created_at", "updated_at", "url"))
  expect_true(all(c("To do", "In progress", "Done") %in% columns$name))
})

