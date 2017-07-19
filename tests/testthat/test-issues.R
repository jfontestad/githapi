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
