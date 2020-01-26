context("collaborators")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_project <- create_project(
    name = paste("Test collaborators", now),
    body = "A project to test collaborator functions",
    org  = "HairyCoos")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_project(
    project = paste("Test collaborators", now),
    org     = "HairyCoos")

})))


# TEST: update_collaborator -------------------------------------------------------------------

test_that("update_collaborator adds a collaborator to a repository or project", {

  repo_result <- update_collaborator(
    user = "ChadGoymer2",
    repo = "ChadGoymer/test-githapi")

  expect_is(repo_result, "logical")
  expect_identical(attr(repo_result, "status"), 201L)
  expect_identical(as.logical(repo_result), TRUE)

  updated_repo_result <- update_collaborator(
    user       = "ChadGoymer2",
    repo       = "ChadGoymer/test-githapi",
    permission = "admin")

  expect_is(updated_repo_result, "logical")
  expect_identical(attr(updated_repo_result, "status"), 201L)
  expect_identical(as.logical(updated_repo_result), TRUE)

  project_result <- update_collaborator(
    user    = "ChadGoymer2",
    project = paste("Test collaborators", now),
    org     = "HairyCoos")

  expect_is(project_result, "logical")
  expect_identical(attr(project_result, "status"), 204L)
  expect_identical(as.logical(project_result), TRUE)

  updated_project_result <- update_collaborator(
    user       = "ChadGoymer2",
    project    = paste("Test collaborators", now),
    org        = "HairyCoos",
    permission = "admin")

  expect_is(updated_project_result, "logical")
  expect_identical(attr(updated_project_result, "status"), 204L)
  expect_identical(as.logical(updated_project_result), TRUE)

})

test_that("update_collaborator throws an error in invalid arguments are supplied", {

  expect_error(
    update_collaborator(user = "ChadGoymer2"),
    "A 'repo' or 'project' must be specified when creating a collaborator")

})


# TEST: view_collaborators --------------------------------------------------------------------

test_that("view_collaborators returns a tibble summarising the collaborators", {

  repo_collaborators <- view_collaborators(repo = "ChadGoymer/test-githapi")

  expect_is(repo_collaborators, "tbl")
  expect_identical(attr(repo_collaborators, "status"), 200L)
  expect_identical(
    map_chr(repo_collaborators, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_true("ChadGoymer" %in% repo_collaborators$login)

  project_collaborators <- view_collaborators(project = paste("Test collaborators", now), org = "HairyCoos")

  expect_is(project_collaborators, "tbl")
  expect_identical(attr(project_collaborators, "status"), 200L)
  expect_identical(
    map_chr(project_collaborators, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_true("ChadGoymer" %in% project_collaborators$login)

  org_collaborators <- view_collaborators(org = "HairyCoos")

  expect_is(org_collaborators, "tbl")
  expect_identical(attr(org_collaborators, "status"), 200L)
  expect_identical(
    map_chr(org_collaborators, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  direct_collaborators <- view_collaborators(org = "HairyCoos", affiliation = "direct")

  expect_is(direct_collaborators, "tbl")
  expect_identical(attr(direct_collaborators, "status"), 200L)
  expect_identical(
    map_chr(direct_collaborators, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

})

test_that("view_collaborators throws an error in invalid arguments are supplied", {

  expect_error(
    view_collaborators(),
    "A 'repo', 'project' or 'org' must be specified when viewing collaborators")

})
