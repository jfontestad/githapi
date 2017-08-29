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

#  FUNCTION: gh_gist_commits ------------------------------------------------------------------
test_that("gh_gist_commits returns a tibble describing the gist commits", {
  git_commits <- gh_gist_commits("806dca6b09a39e7b6326a0c8137583e6")
  expect_is(git_commits, "tbl")
  expect_identical(
    names(git_commits),
    c("version", "user_login", "committed_at", "change_status_total",
      "change_status_additions", "change_status_deletions", "url"))
  expect_true("4a239d53a0e38a3dd2b70e0cd5c7cb316369ab9f" %in% git_commits$version)
})

#  FUNCTION: is_gist_starred ------------------------------------------------------------------
test_that("is_gist_starred returns TRUE if the gist has been starred, FALSE otherwise", {
  is_starred <- is_gist_starred("806dca6b09a39e7b6326a0c8137583e6")
  expect_true(is_starred)

  not_starred <- is_gist_starred("8f4589b02a513914584b28593b9cdcda")
  expect_false(not_starred)
})
