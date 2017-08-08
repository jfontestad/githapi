context("pull request api")

#  FUNCTION: gh_pull_request ------------------------------------------------------------------
test_that("gh_pull_request returns a list describing the pull request", {
  pull_request <- gh_pull_request(8, "ChadGoymer/githapi")
  expect_is(pull_request, "list")
  expect_identical(
    names(pull_request),
    c("url", "id", "html_url", "diff_url", "patch_url", "issue_url", "number", "state",
      "locked", "title", "user", "body", "created_at", "updated_at", "closed_at", "merged_at",
      "merge_commit_sha", "assignee", "assignees", "requested_reviewers", "milestone",
      "commits_url", "review_comments_url", "review_comment_url", "comments_url",
      "statuses_url", "head", "base", "_links", "merged", "mergeable", "rebaseable",
      "mergeable_state", "merged_by", "comments", "review_comments", "maintainer_can_modify",
      "commits", "additions", "deletions", "changed_files"))
  expect_identical(pull_request$number, 8L)
  expect_identical(pull_request$title, "Add Git Data Functions")
})

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
