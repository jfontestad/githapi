context("issues api")

#  FUNCTION: gh_issue -------------------------------------------------------------------------
test_that("gh_issue returns a list of information about the issue", {
  issue_1 <- gh_issue(1, "ChadGoymer/githapi")
  expect_is(issue_1, "list")
  expect_identical(issue_1$title, "Test issue")
  expect_match(issue_1$body, "This issue has been created for testing the issues API")
  expect_identical(issue_1$user$login, "ChadGoymer")
})

#  FUNCTION: gh_issues ------------------------------------------------------------------------
test_that("gh_issues returns a tibble of information about all the issues", {
  issues <- gh_issues("ChadGoymer/githapi")
  expect_is(issues, "tbl")
  expect_identical(
    names(issues),
    c("number", "title", "body", "state", "user_login", "labels", "assignee_login",
      "milestone_number", "milestone_title", "created_at", "updated_at"))
  expect_true("Test issue" %in% issues$title)
  expect_true("This issue has been created for testing the issues API.\r\n" %in% issues$body)
  expect_true("ChadGoymer" %in% issues$user_login)
})

#  FUNCTION: gh_user_issues -------------------------------------------------------------------
test_that("gh_user_issues returns a tibble describing all the issues assigned to the user", {
  my_issues <- gh_user_issues()
  expect_is(my_issues, "tbl")
  expect_identical(
    names(my_issues),
    c("number", "title", "body", "state", "user_login", "labels", "assignee_login",
      "milestone_number", "milestone_title", "created_at", "updated_at", "repository_name"))
  expect_true("Test issue" %in% my_issues$title)
  expect_true("This issue has been created for testing the issues API.\r\n" %in% my_issues$body)
  expect_true("ChadGoymer" %in% my_issues$user_login)
})

#  FUNCTION: gh_assignees --------------------------------------------------------------------
test_that("gh_assignees returns a tibble describing all the assignees of issues", {
  assignees <- gh_assignees("ChadGoymer/githapi")
  expect_is(assignees, "tbl")
  expect_identical(names(assignees), c("login", "type", "site_admin"))
  expect_true("ChadGoymer" %in% assignees$login)
})

#  FUNCTION: gh_issue_comments ----------------------------------------------------------------
test_that("gh_issue_comments returns a tibble of comments", {
  issue_1_comments <- gh_issue_comments(1, "ChadGoymer/githapi")
  expect_is(issue_1_comments, "tbl")
  expect_identical(
    names(issue_1_comments),
    c("id", "body", "user_login", "created_at", "updated_at", "html_url"))
  expect_true("This is the first comment" %in% issue_1_comments$body)

  all_issue_comments <- gh_issue_comments(repo = "ChadGoymer/githapi")
  expect_is(all_issue_comments, "tbl")
  expect_identical(
    names(all_issue_comments),
    c("id", "body", "user_login", "created_at", "updated_at", "html_url"))
  expect_true("This is the first comment" %in% all_issue_comments$body)
})

#  FUNCTION: gh_issue_comment -----------------------------------------------------------------
test_that("gh_issue_comment returns a list describing the comment", {
  first_comment <- gh_issue_comment(316619966, "ChadGoymer/githapi")
  expect_is(first_comment, "list")
  expect_true(all(c("id", "user", "created_at", "updated_at", "body") %in% names(first_comment)))
  expect_identical(first_comment$id, 316619966L)
  expect_identical(first_comment$user$login, "ChadGoymer")
  expect_identical(first_comment$created_at, "2017-07-20T07:25:06Z")
  expect_identical(first_comment$body, "This is the first comment")
})
