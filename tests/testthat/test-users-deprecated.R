context("users api")

#  FUNCTION: gh_user --------------------------------------------------------------------------
test_that("gh_user returns a list describing the user", {
  user <- suppressWarnings(gh_user("ChadGoymer"))
  expect_identical(user$login, "ChadGoymer")
  expect_identical(user$name, "Chad Goymer")
  expect_identical(user$email, "chad.goymer@gmail.com")
})

test_that("gh_user returns an error is the specified user does not exist", {
  expect_error(
    suppressWarnings(gh_user("SomeNameThatDoesNotExist")),
    "Specified user does not exist in GitHub: 'SomeNameThatDoesNotExist'")
})

#  FUNCTION: gh_users -------------------------------------------------------------------------
test_that("gh_users returns a tibble describing all the users", {
  users <- suppressWarnings(gh_users(n_max = 100))
  expect_is(users, "tbl")
  expect_identical(nrow(users), 100L)

  expect_identical(
    sapply(users, function(field) class(field)[[1]]),
    c(login    = "character",
      type     = "character",
      html_url = "character",
      url      = "character"))
})

#  FUNCTION: gh_user_email --------------------------------------------------------------------
test_that("gh_user_email returns the authenticated user's email addresses", {
  email <- suppressWarnings(gh_user_email())
  expect_is(email, "tbl")

  expect_identical(
    sapply(email, function(field) class(field)[[1]]),
    c(email      = "character",
      primary    = "logical",
      verified   = "logical",
      visibility = "character"))

  expect_true("chad.goymer@gmail.com" %in% email$email)
})
