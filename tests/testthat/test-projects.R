context("projects api")

#  FUNCTION: gh_projects ----------------------------------------------------------------------
test_that("gh_projects returns a tibble describing the projects", {
  projects <- gh_projects("ChadGoymer/githapi")
  expect_is(projects, "tbl")
  expect_identical(
    names(projects),
    c("id", "number", "name", "body", "state", "creator_login", "created_at", "updated_at", "url"))
  expect_true("githapi" %in% projects$name)
})
