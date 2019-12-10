context("projects api")


# TEST: create_project ------------------------------------------------------------------------

test_that("create_projects creates a project and returns its properties", {

  repo_project <- create_project(
    name = "Repo project",
    body = "This is a repo project",
    repo = "ChadGoymer/test-githapi")

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 201L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      permission = "character",
      private    = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      html_url   = "character"))

  expect_identical(repo_project$name, "Repo project")

  user_project <- create_project(
    name = "User project",
    body = "This is a user project")

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 201L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      permission = "character",
      private    = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      html_url   = "character"))

  expect_identical(user_project$name, "User project")

  org_project <- create_project(
    name = "Organisation project",
    body = "This is an organisation project",
    org  = "HairyCoos")

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 201L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      permission = "character",
      private    = "logical",
      creator    = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      html_url   = "character"))

  expect_identical(org_project$name, "Organisation project")

})

test_that("create_project throws an error if invalid arguments are supplied", {

  expect_error(
    create_project(name = 1, body = "This is an invalid project"),
    "'name' must be a string")

  expect_error(
    create_project(name = "invalid project", body = 1),
    "'body' must be a string")

  expect_error(
    create_project(name = "invalid project", body = "This is an invalid project", repo = 1),
    "'repo' must be a string in the format 'owner/repo'")

  expect_error(
    create_project(name = "invalid project", body = "This is an invalid project", org = 1),
    "'org' must be a string")

})
