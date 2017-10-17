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
      "statuses_url", "head", "base", "_links", "author_association", "merged", "mergeable",
      "rebaseable", "mergeable_state", "merged_by", "comments", "review_comments",
      "maintainer_can_modify", "commits", "additions", "deletions", "changed_files"))
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

#  FUNCTION: gh_pull_commits ------------------------------------------------------------------
test_that("gh_pull_commits returns a tibble describing the commits on a pull request", {
  commits <- gh_pull_commits(8, "ChadGoymer/githapi")
  expect_is(commits, "tbl")
  expect_identical(
    names(commits),
    c("sha", "author_login", "commit_date", "commit_message", "url", "parents_sha"))
  expect_true("8934516f37977847381604e432f1fa1bd2ad69fa" %in% commits$sha)
})

#  FUNCTION: gh_pull_files --------------------------------------------------------------------
test_that("gh_pull_files returns a tibble describing the files changed on a pull request", {
  files <- gh_pull_files(8, "ChadGoymer/githapi")
  expect_is(files, "tbl")
  expect_identical(
    names(files),
    c("sha", "filename", "status", "additions", "deletions",
      "changes", "blob_url", "contents_url", "patch"))
  expect_true("R/git-data.R" %in% files$filename)
})

#  FUNCTION: gh_pull_merged -------------------------------------------------------------------
test_that("gh_pull_merged returns TRUE if the pull request has been merged and FALSE otherwise", {
  merged <- gh_pull_merged(8, "ChadGoymer/githapi")
  expect_true(merged)

  # TODO: set up pull request for testing
  unmerged <- gh_pull_merged(24, "ChadGoymer/githapi")
  expect_false(unmerged)
})

#  FUNCTION: gh_pull_review -------------------------------------------------------------------
test_that("gh_pull_review returns a list describing the pull request review", {
  # TODO: Add a collaborator so they can be assigned to review
  # review <- gh_pull_review(1, 8, "ChadGoymer/githapi")
  # expect_is(review, "list")
})

#  FUNCTION: gh_pull_reviews ------------------------------------------------------------------
test_that("gh_pull_reviews returns a tibble describing the pull request reviews", {
  # TODO: Add a collaborator so they can be assigned to review
  # reviews <- gh_pull_reviews(13, "ChadGoymer/githapi")
  # expect_is(reviews, "tbl")
})

#  FUNCTION: gh_pull_comment ------------------------------------------------------------------
test_that("gh_pull_comment returns a list describing the pull request review comment", {
  # TODO: Add a collaborator so they can be assigned to review
  # comment <- gh_pull_comment(1, "ChadGoymer/githapi")
  # expect_is(reviews, "list")
})

#  FUNCTION: gh_pull_comments -----------------------------------------------------------------
test_that("gh_pull_comments returns a tibble describing the pull request comments", {
  # TODO: Add a collaborator so they can be assigned to review
  # repo_comments <- gh_pull_comments("ChadGoymer/githapi")
  # expect_is(repo_comments, "tbl")
  #
  # pull_comments <- gh_pull_comments("ChadGoymer/githapi", 13)
  # expect_is(pull_comments, "tbl")
  #
  # review_comments <- gh_pull_comments("ChadGoymer/githapi", 13, 1)
  # expect_is(review_comments, "tbl")
})

#  FUNCTION: gh_pull_review_requests ----------------------------------------------------------
test_that("gh_pull_review_requests returns a tibble describing the pull request review requests", {
  # TODO: Add a collaborator so they can be assigned to review
  # review_requests <- gh_pull_review_requests(13, "ChadGoymer/githapi")
  # expect_is(review_requests, "tbl")
})