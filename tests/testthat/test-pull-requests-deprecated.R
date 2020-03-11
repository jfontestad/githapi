context("pull request api")

#  FUNCTION: gh_pull_request ------------------------------------------------------------------
test_that("gh_pull_request returns a list describing the pull request", {
  pull_request <- suppressWarnings(gh_pull_request(8, "ChadGoymer/githapi"))
  expect_is(pull_request, "list")
  expect_named(
    pull_request,
    c("url", "id", "node_id", "html_url", "diff_url", "patch_url", "issue_url", "number",
      "state", "locked", "title", "user", "body", "created_at", "updated_at", "closed_at",
      "merged_at", "merge_commit_sha", "assignee", "assignees", "requested_reviewers",
      "requested_teams", "labels", "milestone", "draft", "commits_url", "review_comments_url",
      "review_comment_url", "comments_url", "statuses_url", "head", "base", "_links",
      "author_association", "merged", "mergeable", "rebaseable", "mergeable_state",
      "merged_by", "comments", "review_comments", "maintainer_can_modify", "commits",
      "additions", "deletions", "changed_files"))
  expect_identical(pull_request$number, 8L)
  expect_identical(pull_request$title, "Add Git Data Functions")
})

#  FUNCTION: gh_pull_requests -----------------------------------------------------------------
test_that("gh_pull_requests returns a tibble describing the pull requests", {
  pulls <- suppressWarnings(gh_pull_requests("ChadGoymer/githapi", state = "all"))
  expect_is(pulls, "tbl")

  expect_identical(
    sapply(pulls, function(field) class(field)[[1]]),
    c(id                  = "integer",
      number              = "integer",
      title               = "character",
      body                = "character",
      user_login          = "character",
      state               = "character",
      created_at          = "POSIXct",
      updated_at          = "POSIXct",
      closed_at           = "POSIXct",
      merged_at           = "POSIXct",
      merge_commit_sha    = "character",
      head_ref            = "character",
      head_sha            = "character",
      head_user_login     = "character",
      head_repo_full_name = "character",
      locked              = "logical",
      url                 = "character"))

  expect_true("Add Git Data Functions" %in% pulls$title)
})

#  FUNCTION: gh_pull_commits ------------------------------------------------------------------
test_that("gh_pull_commits returns a tibble describing the commits on a pull request", {
  commits <- suppressWarnings(gh_pull_commits(8, "ChadGoymer/githapi"))
  expect_is(commits, "tbl")

  expect_identical(
    sapply(commits, function(field) class(field)[[1]]),
    c(sha            = "character",
      author_login   = "character",
      commit_date    = "POSIXct",
      commit_message = "character",
      url            = "character",
      parents_sha    = "character"))

  expect_true("8934516f37977847381604e432f1fa1bd2ad69fa" %in% commits$sha)
})

#  FUNCTION: gh_pull_files --------------------------------------------------------------------
test_that("gh_pull_files returns a tibble describing the files changed on a pull request", {
  files <- suppressWarnings(gh_pull_files(8, "ChadGoymer/githapi"))
  expect_is(files, "tbl")

  expect_identical(
    sapply(files, function(field) class(field)[[1]]),
    c(sha          = "character",
      filename     = "character",
      status       = "character",
      additions    = "integer",
      deletions    = "integer",
      changes      = "integer",
      blob_url     = "character",
      contents_url = "character",
      patch        = "character"))

  expect_true("R/git-data.R" %in% files$filename)
})

#  FUNCTION: is_pull_merged -------------------------------------------------------------------
test_that("is_pull_merged returns TRUE if the pull request has been merged and FALSE otherwise", {
  expect_true(suppressWarnings(is_pull_merged(8, "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_pull_merged(24, "ChadGoymer/githapi")))
})

#  FUNCTION: gh_pull_review -------------------------------------------------------------------
test_that("gh_pull_review returns a list describing the pull request review", {
  # TODO: Add a collaborator so they can be assigned to review
  # review <- suppressWarnings(gh_pull_review(1, 8, "ChadGoymer/githapi"))
  # expect_is(review, "list")
})

#  FUNCTION: gh_pull_reviews ------------------------------------------------------------------
test_that("gh_pull_reviews returns a tibble describing the pull request reviews", {
  # TODO: Add a collaborator so they can be assigned to review
  # reviews <- suppressWarnings(gh_pull_reviews(13, "ChadGoymer/githapi"))
  # expect_is(reviews, "tbl")
})

#  FUNCTION: gh_pull_comment ------------------------------------------------------------------
test_that("gh_pull_comment returns a list describing the pull request review comment", {
  # TODO: Add a collaborator so they can be assigned to review
  # comment <- suppressWarnings(gh_pull_comment(1, "ChadGoymer/githapi"))
  # expect_is(reviews, "list")
})

#  FUNCTION: gh_pull_comments -----------------------------------------------------------------
test_that("gh_pull_comments returns a tibble describing the pull request comments", {
  # TODO: Add a collaborator so they can be assigned to review
  # repo_comments <- suppressWarnings(gh_pull_comments("ChadGoymer/githapi"))
  # expect_is(repo_comments, "tbl")
  #
  # pull_comments <- suppressWarnings(gh_pull_comments("ChadGoymer/githapi", 13))
  # expect_is(pull_comments, "tbl")
  #
  # review_comments <- suppressWarnings(gh_pull_comments("ChadGoymer/githapi", 13, 1))
  # expect_is(review_comments, "tbl")
})

#  FUNCTION: gh_pull_review_requests ----------------------------------------------------------
test_that("gh_pull_review_requests returns a tibble describing the pull request review requests", {
  # TODO: Add a collaborator so they can be assigned to review
  # review_requests <- suppressWarnings(gh_pull_review_requests(13, "ChadGoymer/githapi"))
  # expect_is(review_requests, "tbl")
})
