context("users api")


# TEST: view_users -------------------------------------------------------------------------

test_that("view_users returns a tibble summarising the users", {

  repo_users <- view_users(repo = "ChadGoymer/githapi")

  expect_is(repo_users, "tbl")
  expect_identical(attr(repo_users, "status"), 200L)
  expect_identical(
    map_chr(repo_users, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      type        = "character",
      site_admin  = "logical",
      html_url    = "character"))

  expect_true("ChadGoymer" %in% repo_users$login)

  org_users <- view_users(org = "HairyCoos")

  expect_is(org_users, "tbl")
  expect_identical(attr(org_users, "status"), 200L)
  expect_identical(
    map_chr(org_users, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      type        = "character",
      site_admin  = "logical",
      html_url    = "character"))

  expect_true("ChadGoymer" %in% org_users$login)

  team_users <- view_users(org = "HairyCoos", team = "HeadCoos")

  expect_is(team_users, "tbl")
  expect_identical(attr(team_users, "status"), 200L)
  expect_identical(
    map_chr(team_users, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      type        = "character",
      site_admin  = "logical",
      html_url    = "character"))

  expect_true("ChadGoymer" %in% team_users$login)

  all_users <- view_users(n_max = 10)

  expect_is(all_users, "tbl")
  expect_identical(attr(all_users, "status"), 200L)
  expect_identical(
    map_chr(all_users, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      type        = "character",
      site_admin  = "logical",
      html_url    = "character"))

})


# TEST: view_user -----------------------------------------------------------------------------

test_that("view_user returns a list of user properties", {

  user <- view_user("ChadGoymer")

  expect_is(user, "list")
  expect_identical(attr(user, "status"), 200L)
  expect_identical(
    map_chr(user, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      type        = "character",
      site_admin  = "logical",
      html_url    = "character"))

  expect_identical(user$login, "ChadGoymer")

})


# TEST: browse_user ---------------------------------------------------------------------------

test_that("browse_user opens the user's page in the browser", {

  skip_if(!interactive(), "browse_user must be tested manually")

  user <- browse_user("ChadGoymer")

  expect_is(user, "character")
  expect_identical(attr(user, "status"), 200L)
  expect_identical(as.character(user), "https://github.com/ChadGoymer")

})
