context("pull request api")

#  FUNCTION: gh_pull_requests -----------------------------------------------------------------
test_that("gh_pull_requests returns a tibble describing the pull requests", {
  pulls <- gh_pull_requests("ChadGoymer/githapi", state = "all")
  expect_is(pulls, "tbl")
  expect_identical(
    names(pulls),
    c("id", "number", "title", "body", "user_login", "state", "created_at", "updated_at",
    "closed_at", "merged_at", "merge_commit_sha", "head_ref", "head_sha", "head_user_login",
    "head_repo_full_name", "locked", "url"))
  expect_true("Add Git Data Functions" %in% pulls$title)
})
