context("users api")

#  FUNCTION: gh_user --------------------------------------------------------------------------
test_that("gh_user returns a list describing the user", {
  user <- gh_user("ChadGoymer")
  expect_identical(user$login, "ChadGoymer")
  expect_identical(user$name, "Chad Goymer")
  expect_identical(user$email, "chad.goymer@gmail.com")
})

test_that("gh_user returns an error is the specified user does not exist", {
  expect_error(gh_user("SomeNameThatDoesNotExist"), "Specified user does not exist in GitHub: 'SomeNameThatDoesNotExist'")
})

#  FUNCTION: gh_users -------------------------------------------------------------------------
test_that("gh_users returns a tibble describing all the users", {
  users <- gh_users(page_size = 10L, max_pages  = 10L)
  expect_true(is_tibble(users))
  expect_identical(nrow(users), 100L)
  expect_named(users, c("login", "type", "html_url", "url"))
})

#  FUNCTION: gh_user_email --------------------------------------------------------------------
test_that("gh_user_email returns the authenticated user's email addresses", {
  email <- gh_user_email()
  expect_is(email, "tbl")
  expect_identical(names(email), c("email", "primary", "verified", "visibility"))
  expect_true("chad.goymer@gmail.com" %in% email$email)
})
