context("gists api")

#  FUNCTION: gh_gists -------------------------------------------------------------------------
test_that("gh_gists returns a tibble describing the gists", {
  auth_user_gists <- gh_gists()
  expect_is(auth_user_gists, "tbl")
  expect_identical(
    names(auth_user_gists),
    c("id", "description", "owner_login", "created_at", "updated_at", "comments",
      "filenames", "languages", "file_sizes", "public", "url"))
  expect_true("An R script to test GitHub's Gist API" %in% auth_user_gists$description)

  user_gists <- gh_gists("ChadGoymer")
  expect_is(user_gists, "tbl")
  expect_identical(
    names(user_gists),
    c("id", "description", "owner_login", "created_at", "updated_at", "comments",
      "filenames", "languages", "file_sizes", "public", "url"))
  expect_true("An R script to test GitHub's Gist API" %in% user_gists$description)

  public_gists <- gh_gists("public", n_max = 10)
  expect_is(public_gists, "tbl")
  expect_identical(
    names(public_gists),
    c("id", "description", "owner_login", "created_at", "updated_at", "comments",
      "filenames", "languages", "file_sizes", "public", "url"))
  expect_identical(nrow(public_gists), 10L)

  starred_gists <- gh_gists("starred")
  expect_is(starred_gists, "tbl")
  expect_identical(
    names(starred_gists),
    c("id", "description", "owner_login", "created_at", "updated_at", "comments",
      "filenames", "languages", "file_sizes", "public", "url"))
  expect_true("An R script to test GitHub's Gist API" %in% starred_gists$description)
})
