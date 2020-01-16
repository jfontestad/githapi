context("users")


# TEST: view_users -------------------------------------------------------------------------

test_that("view_users returns a tibble summarising the users", {

  org_users <- view_users(org = "HairyCoos")

  expect_is(org_users, "tbl")
  expect_identical(attr(org_users, "status"), 200L)
  expect_identical(
    map_chr(org_users, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_true("ChadGoymer" %in% org_users$login)

  team_users <- view_users(org = "HairyCoos", team = "HeadCoos")

  expect_is(team_users, "tbl")
  expect_identical(attr(team_users, "status"), 200L)
  expect_identical(
    map_chr(team_users, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_true("ChadGoymer" %in% team_users$login)

  all_users <- view_users(n_max = 10)

  expect_is(all_users, "tbl")
  expect_identical(attr(all_users, "status"), 200L)
  expect_identical(
    map_chr(all_users, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  admin_users <- view_users(org = "HairyCoos", role = "admin")

  expect_is(admin_users, "tbl")
  expect_identical(attr(admin_users, "status"), 200L)
  expect_identical(
    map_chr(admin_users, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_true("ChadGoymer" %in% admin_users$login)
  expect_true(nrow(admin_users) < nrow(org_users))

})


# TEST: view_user -----------------------------------------------------------------------------

test_that("view_user returns a list of user properties", {

  user <- view_user("ChadGoymer")

  expect_is(user, "list")
  expect_identical(attr(user, "status"), 200L)
  expect_identical(
    map_chr(user, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_identical(user$login, "ChadGoymer")

  auth_user <- view_user()

  expect_is(auth_user, "list")
  expect_identical(attr(auth_user, "status"), 200L)
  expect_identical(
    map_chr(auth_user, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_identical(auth_user$login, "ChadGoymer")

})


# TEST: browse_user ---------------------------------------------------------------------------

test_that("browse_user opens the user's page in the browser", {

  skip_if(!interactive(), "browse_user must be tested manually")

  user <- browse_user("ChadGoymer")

  expect_is(user, "character")
  expect_identical(attr(user, "status"), 200L)
  expect_identical(as.character(user), "https://github.com/ChadGoymer")

  auth_user <- browse_user()

  expect_is(auth_user, "character")
  expect_identical(attr(auth_user, "status"), 200L)
  expect_identical(as.character(auth_user), "https://github.com/ChadGoymer")

})


# TEST: update_user ---------------------------------------------------------------------------

test_that("update_user changes the user's properties", {

  original_user <- view_user("ChadGoymer")

  on.exit({
    update_user(
      name     = original_user$name,
      email    = original_user$email,
      blog     = original_user$blog,
      company  = original_user$company,
      location = original_user$location,
      hireable = FALSE,
      bio      = original_user$bio,)
  })

  updated_user <- update_user(
    name     = "Bob",
    email    = original_user$email,
    blog     = "https://acme.com/blog",
    company  = "ACME",
    location = "Nowhere",
    hireable = TRUE,
    bio      = "Blah Blah")

  expect_is(updated_user, "list")
  expect_identical(attr(updated_user, "status"), 200L)
  expect_identical(
    map_chr(updated_user, ~ class(.)[[1]]),
    c(id         = "integer",
      login      = "character",
      name       = "character",
      email      = "character",
      blog       = "character",
      company    = "character",
      location   = "character",
      hireable   = "logical",
      bio        = "character",
      site_admin = "logical",
      html_url   = "character"))

  expect_identical(updated_user$login, "ChadGoymer")
  expect_identical(updated_user$name, "Bob")
  expect_identical(updated_user$location, "Nowhere")
  expect_identical(updated_user$hireable, TRUE)

})
