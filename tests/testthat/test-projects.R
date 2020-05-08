context("projects")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-repo-", now),
    description = "This is a repository to test repositories")

  create_team(
    name        = str_c("Test projects ", now),
    description = "This was created to test team projects",
    org         = "HairyCoos")

}))

teardown(suppressMessages({

  delete_team(str_c("Test projects ", now), org = "HairyCoos")

}))


# TEST: create_project ------------------------------------------------------------------------

test_that("create_projects creates a project and returns its properties", {

  repo_project <- create_project(
    name = str_c("Repo project ", now),
    body = "This is a repo project",
    repo = str_c("ChadGoymer/test-repo-", now))

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 201L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(repo_project$name, str_c("Repo project ", now))

  user_project <- create_project(
    name = str_c("User project ", now),
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
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(user_project$name, str_c("User project ", now))

  org_project <- create_project(
    name = str_c("Organization project ", now),
    body = "This is an organization project",
    org  = "HairyCoos")

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 201L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"))

  expect_identical(org_project$name, str_c("Organization project ", now))

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


# TEST: update_project ------------------------------------------------------------------------

test_that("update_project updates a project and returns a list of the new properties", {

  repo_project <- update_project(
    project = str_c("Repo project ", now),
    name    = str_c("Updated repo project ", now),
    body    = "This is an updated repo project",
    repo    = str_c("ChadGoymer/test-repo-", now))

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(repo_project$name, str_c("Updated repo project ", now))

  user_project <- update_project(
    project = str_c("User project ", now),
    state   = "closed",
    user    = "ChadGoymer")

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(user_project$state, "closed")

  org_project <- update_project(
    project    = str_c("Organization project ", now),
    permission = "read",
    private    = FALSE,
    org        = "HairyCoos")

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"))

  expect_identical(org_project$org_permission, "read")
  expect_identical(org_project$private, FALSE)

  team_project <- update_project(
    project = str_c("Organization project ", now),
    team    = str_c("Test projects ", now),
    org     = "HairyCoos")

  expect_is(team_project, "list")
  expect_identical(attr(team_project, "status"), 204L)
  expect_identical(
    map_chr(team_project, ~ class(.)[[1]]),
    c(id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"))

  expect_identical(team_project$team_permission, "write")

  upd_team_project <- update_project(
    project    = str_c("Organization project ", now),
    team       = str_c("Test projects ", now),
    org        = "HairyCoos",
    permission = "read")

  expect_is(upd_team_project, "list")
  expect_identical(attr(upd_team_project, "status"), 204L)
  expect_identical(
    map_chr(upd_team_project, ~ class(.)[[1]]),
    c(id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"))

  expect_identical(upd_team_project$team_permission, "read")

})


# TEST: view_projects -------------------------------------------------------------------------

test_that("view_projects returns a tibble summarising the projects", {

  repo_projects <- view_projects(str_c("ChadGoymer/test-repo-", now), n_max = 10)

  expect_is(repo_projects, "tbl")
  expect_identical(attr(repo_projects, "status"), 200L)
  expect_identical(
    map_chr(repo_projects, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_true(str_c("Updated repo project ", now) %in% repo_projects$name)

  user_projects <- view_projects(user = "ChadGoymer", state = "closed", n_max = 10)

  expect_is(user_projects, "tbl")
  expect_identical(attr(user_projects, "status"), 200L)
  expect_identical(
    map_chr(user_projects, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_true(str_c("User project ", now) %in% user_projects$name)

  org_projects <- view_projects(org = "HairyCoos", n_max = 10)

  expect_is(org_projects, "tbl")
  expect_identical(attr(org_projects, "status"), 200L)
  expect_identical(
    map_chr(org_projects, ~ class(.)[[1]]),
    c(id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"))

  expect_true(str_c("Organization project ", now) %in% org_projects$name)

  team_projects <- view_projects(
    team  = str_c("Test projects ", now),
    org   = "HairyCoos",
    n_max = 10)

  expect_is(team_projects, "tbl")
  expect_identical(attr(team_projects, "status"), 200L)
  expect_identical(
    map_chr(team_projects, ~ class(.)[[1]]),
    c(id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"))

  expect_true(str_c("Organization project ", now) %in% team_projects$name)

})

test_that("view_projects throws an error if invalid arguments are supplied", {

  expect_error(
    view_projects(),
    "Must specify either 'repo', 'user' or 'org'!")

})


# TEST: view_project --------------------------------------------------------------------------

test_that("view_project returns a list of project properties", {

  repo_project <- view_project(
    project = str_c("Updated repo project ", now),
    repo    = str_c("ChadGoymer/test-repo-", now))

  expect_is(repo_project, "list")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    map_chr(repo_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(repo_project$name, str_c("Updated repo project ", now))

  user_project <- view_project(str_c("User project ", now), user = "ChadGoymer")

  expect_is(user_project, "list")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(
    map_chr(user_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(user_project$state, "closed")

  org_project <- view_project(str_c("Organization project ", now), org = "HairyCoos")

  expect_is(org_project, "list")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(
    map_chr(org_project, ~ class(.)[[1]]),
    c(id             = "integer",
      number         = "integer",
      name           = "character",
      body           = "character",
      state          = "character",
      private        = "logical",
      org_permission = "character",
      creator        = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"))

  expect_identical(org_project$org_permission, "read")
  expect_identical(org_project$private, FALSE)

  team_project <- view_project(
    project = str_c("Organization project ", now),
    team    = str_c("Test projects ", now),
    org     = "HairyCoos")

  expect_is(team_project, "list")
  expect_identical(attr(team_project, "status"), 200L)
  expect_identical(
    map_chr(team_project, ~ class(.)[[1]]),
    c(id              = "integer",
      number          = "integer",
      name            = "character",
      body            = "character",
      state           = "character",
      private         = "logical",
      org_permission  = "character",
      team_permission = "character",
      creator         = "character",
      html_url        = "character",
      created_at      = "POSIXct",
      updated_at      = "POSIXct"))

  expect_identical(team_project$team_permission, "read")

})

test_that("view_project can accept a project number", {

  projects <- view_projects(str_c("ChadGoymer/test-repo-", now))

  first_project <- view_project(projects$number[[1]], str_c("ChadGoymer/test-repo-", now))

  expect_is(first_project, "list")
  expect_identical(attr(first_project, "status"), 200L)
  expect_identical(
    map_chr(first_project, ~ class(.)[[1]]),
    c(id         = "integer",
      number     = "integer",
      name       = "character",
      body       = "character",
      state      = "character",
      creator    = "character",
      html_url   = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct"))

  expect_identical(first_project$number, projects$number[[1]])

})

test_that("view_project throws an error if invalid arguments are supplied", {

  expect_error(
    view_project(TRUE, str_c("ChadGoymer/test-repo-", now)),
    "'project' must be either an integer or a string")

  expect_error(
    view_project(str_c("Repo project ", now)),
    "Must specify either 'repo', 'user' or 'org'!")

})


# TEST: browse_project ------------------------------------------------------------------------

test_that("browse_project opens the project in the browser", {

  skip_if(!interactive(), "browse_project must be tested manually")

  repo_project <- browse_project(
    project = str_c("Updated repo project ", now),
    repo    = str_c("ChadGoymer/test-repo-", now))

  expect_is(repo_project, "character")
  expect_identical(attr(repo_project, "status"), 200L)
  expect_identical(
    dirname(repo_project),
    str_c("https://github.com/ChadGoymer/test-repo-", now, "/projects"))

  user_project <- browse_project(str_c("User project ", now), user = "ChadGoymer")

  expect_is(user_project, "character")
  expect_identical(attr(user_project, "status"), 200L)
  expect_identical(dirname(user_project), "https://github.com/users/ChadGoymer/projects")

  org_project <- browse_project(str_c("Organization project ", now), org = "HairyCoos")

  expect_is(org_project, "character")
  expect_identical(attr(org_project, "status"), 200L)
  expect_identical(dirname(org_project), "https://github.com/orgs/HairyCoos/projects")

  team_project <- browse_project(
    project = str_c("Organization project ", now),
    team    = str_c("Test projects ", now),
    org     = "HairyCoos")

  expect_is(team_project, "character")
  expect_identical(attr(team_project, "status"), 200L)
  expect_identical(dirname(team_project), "https://github.com/orgs/HairyCoos/projects")

})


# TEST: delete_project ------------------------------------------------------------------------

test_that("delete_project deletes the projects and returns TRUE", {

  repo_project <- delete_project(
    project = str_c("Updated repo project ", now),
    repo    = str_c("ChadGoymer/test-repo-", now))

  expect_is(repo_project, "logical")
  expect_identical(attr(repo_project, "status"), 204L)
  expect_identical(as.logical(repo_project), TRUE)

  user_project <- delete_project(str_c("User project ", now), user = "ChadGoymer")

  expect_is(user_project, "logical")
  expect_identical(attr(user_project, "status"), 204L)
  expect_identical(as.logical(user_project), TRUE)

  team_project <- delete_project(
    project = str_c("Organization project ", now),
    team    = str_c("Test projects ", now),
    org     = "HairyCoos")

  expect_is(team_project, "logical")
  expect_identical(attr(team_project, "status"), 204L)
  expect_identical(as.logical(team_project), TRUE)

  org_project <- delete_project(str_c("Organization project ", now), org = "HairyCoos")

  expect_is(org_project, "logical")
  expect_identical(attr(org_project, "status"), 204L)
  expect_identical(as.logical(org_project), TRUE)

})
