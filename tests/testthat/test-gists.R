context("gists api")

#  FUNCTION: gh_gist --------------------------------------------------------------------------
test_that("gh_gist returns a list describing the gist", {
  gist <- gh_gist("806dca6b09a39e7b6326a0c8137583e6")
  expect_is(gist, "list")
  expect_identical(
    names(gist),
    c("url", "forks_url", "commits_url", "id", "git_pull_url", "git_push_url", "html_url",
      "files", "public", "created_at", "updated_at", "description", "comments", "user",
      "comments_url", "owner", "forks", "history", "truncated"))
  expect_identical(gist$description, "An R script to test GitHub's Gist API")

  gist_sha <- gh_gist("806dca6b09a39e7b6326a0c8137583e6", "4a239d53a0e38a3dd2b70e0cd5c7cb316369ab9f")
  expect_is(gist_sha, "list")
  expect_identical(
    names(gist_sha),
    c("url", "forks_url", "commits_url", "id", "git_pull_url", "git_push_url", "html_url",
      "files", "public", "created_at", "updated_at", "description", "comments", "user",
      "comments_url", "owner", "forks", "history", "truncated"))
  expect_identical(gist_sha$description, "An R script to test GitHub's Gist API")
})

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
