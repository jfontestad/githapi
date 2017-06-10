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
  users <- gh_users()
  expect_true(is_tibble(users))
  expect_identical(nrow(users), 1000L)
  expect_named(users, c("login", "type", "html_url", "url"))
})
