context("issues api")

#  FUNCTION: gh_issue -------------------------------------------------------------------------
test_that("gh_issue returns a list of information about the issue", {
  issue_1 <- suppressWarnings(gh_issue(1, "ChadGoymer/githapi"))
  expect_is(issue_1, "list")
  expect_identical(issue_1$title, "Test issue")
  expect_match(issue_1$body, "This issue has been created for testing the issues API")
  expect_identical(issue_1$user$login, "ChadGoymer")
})

#  FUNCTION: gh_issues ------------------------------------------------------------------------
test_that("gh_issues returns a tibble of information about all the issues", {
  issues <- suppressWarnings(gh_issues("ChadGoymer/githapi"))
  expect_is(issues, "tbl")

  expect_identical(
    sapply(issues, function(field) class(field)[[1]]),
    c(number           = "integer",
      title            = "character",
      body             = "character",
      state            = "character",
      user_login       = "character",
      labels           = "list",
      assignees        = "list",
      milestone_number = "integer",
      milestone_title  = "character",
      created_at       = "POSIXct",
      updated_at       = "POSIXct",
      closed_at        = "POSIXct"))

  expect_true("Test issue" %in% issues$title)
  expect_true("This issue has been created for testing the issues API.\r\n" %in% issues$body)
  expect_true("ChadGoymer" %in% issues$user_login)
})

#  FUNCTION: gh_user_issues -------------------------------------------------------------------
test_that("gh_user_issues returns a tibble describing all the issues assigned to the user", {
  my_issues <- suppressWarnings(gh_user_issues())
  expect_is(my_issues, "tbl")

  expect_identical(
    sapply(my_issues, function(field) class(field)[[1]]),
    c(number           = "integer",
      title            = "character",
      body             = "character",
      state            = "character",
      repository_name  = "character",
      labels           = "list",
      user_login       = "character",
      assignee_login   = "character",
      milestone_number = "integer",
      milestone_title  = "character",
      created_at       = "POSIXct",
      updated_at       = "POSIXct"))

  expect_true("Test issue" %in% my_issues$title)
  expect_true("This issue has been created for testing the issues API.\r\n" %in% my_issues$body)
  expect_true("ChadGoymer" %in% my_issues$user_login)
})

#  FUNCTION: gh_assignees --------------------------------------------------------------------
test_that("gh_assignees returns a tibble describing all the assignees of issues", {
  assignees <- suppressWarnings(gh_assignees("ChadGoymer/githapi"))
  expect_is(assignees, "tbl")

  expect_identical(
    sapply(assignees, function(field) class(field)[[1]]),
    c(login      = "character",
      type       = "character",
      site_admin = "logical"))

  expect_true("ChadGoymer" %in% assignees$login)
})

#  FUNCTION: gh_issue_comments ----------------------------------------------------------------
test_that("gh_issue_comments returns a tibble of comments", {
  issue_1_comments <- suppressWarnings(gh_issue_comments(1, "ChadGoymer/githapi"))
  expect_is(issue_1_comments, "tbl")

  expect_identical(
    sapply(issue_1_comments, function(field) class(field)[[1]]),
    c(id         = "integer",
      body       = "character",
      user_login = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      html_url   = "character"))

  expect_true("This is the first comment" %in% issue_1_comments$body)

  all_issue_comments <- suppressWarnings(gh_issue_comments(repo = "ChadGoymer/githapi"))
  expect_is(all_issue_comments, "tbl")

  expect_identical(
    sapply(all_issue_comments, function(field) class(field)[[1]]),
    c(id         = "integer",
      body       = "character",
      user_login = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      html_url   = "character"))

  expect_true("This is the first comment" %in% all_issue_comments$body)
})

#  FUNCTION: gh_issue_comment -----------------------------------------------------------------
test_that("gh_issue_comment returns a list describing the comment", {
  first_comment <- suppressWarnings(gh_issue_comment(316619966, "ChadGoymer/githapi"))
  expect_is(first_comment, "list")
  expect_true(all(c("id", "user", "created_at", "updated_at", "body") %in% names(first_comment)))
  expect_identical(first_comment$id, 316619966L)
  expect_identical(first_comment$user$login, "ChadGoymer")
  expect_identical(first_comment$created_at, "2017-07-20T07:25:06Z")
  expect_identical(first_comment$body, "This is the first comment")
})

#  FUNCTION: gh_label -------------------------------------------------------------------------
test_that("gh_label returns a list describing the label", {
  label <- suppressWarnings(gh_label("bug", "ChadGoymer/githapi"))
  expect_is(label, "list")
  expect_named(label, c("id", "node_id", "url", "name", "color", "default", "description"))
  expect_identical(label$name, "bug")
  expect_identical(label$color, "d73a4a")
})

#  FUNCTION: gh_labels ------------------------------------------------------------------------
test_that("gh_labels returns a tibble of information about the labels", {
  labels <- suppressWarnings(gh_labels("ChadGoymer/githapi"))
  expect_is(labels, "tbl")

  expect_identical(
    sapply(labels, function(field) class(field)[[1]]),
    c(id      = "integer",
      name    = "character",
      color   = "character",
      default = "logical",
      url     = "character"))

  expect_true("bug" %in% labels$name)
  expect_true("d73a4a" %in% labels$color)

  issue_labels <- suppressWarnings(gh_labels("ChadGoymer/githapi", issue = 1))
  expect_is(labels, "tbl")

  expect_identical(
    sapply(issue_labels, function(field) class(field)[[1]]),
    c(id      = "integer",
      name    = "character",
      color   = "character",
      default = "logical",
      url     = "character"))

  expect_true("test" %in% labels$name)
  expect_true("e39af9" %in% labels$color)

  milestone_labels <- suppressWarnings(gh_labels("ChadGoymer/githapi", milestone = 1))
  expect_is(labels, "tbl")

  expect_identical(
    sapply(milestone_labels, function(field) class(field)[[1]]),
    c(id      = "integer",
      name    = "character",
      color   = "character",
      default = "logical",
      url     = "character"))

  expect_true("feature" %in% labels$name)
  expect_true("25b24d" %in% labels$color)
})

#  FUNCTION: gh_milestone ---------------------------------------------------------------------
test_that("gh_milestone returns a list describing a milestone", {
  milestone <- suppressWarnings(gh_milestone(1, "ChadGoymer/githapi"))
  expect_is(milestone, "list")
  expect_named(
    milestone,
    c("url", "html_url", "labels_url", "id", "node_id", "number", "title", "description", "creator",
      "open_issues", "closed_issues", "state", "created_at", "updated_at", "due_on", "closed_at"))
  expect_identical(milestone$number, 1L)
  expect_identical(milestone$title, "v0.2.0")
})

#  FUNCTION: gh_milestones --------------------------------------------------------------------
test_that("gh_milestones returns a tibble describing the milestones", {
  milestones <- suppressWarnings(gh_milestones("ChadGoymer/githapi", state = "all"))
  expect_is(milestones, "tbl")

  expect_identical(
    sapply(milestones, function(field) class(field)[[1]]),
    c(id            = "integer",
      number        = "integer",
      title         = "character",
      description   = "character",
      creator_login = "character",
      open_issues   = "integer",
      closed_issues = "integer",
      state         = "character",
      created_at    = "POSIXct",
      updated_at    = "POSIXct",
      url           = "character"))

  expect_true(1L %in% milestones$number)
  expect_true("v0.2.0" %in% milestones$title)
})

#  FUNCTION: gh_event -------------------------------------------------------------------------
test_that("gh_event returns a list describing the event", {
  event <- gh_event(1227046339, "ChadGoymer/githapi")
  expect_is(event, "list")
  expect_named(
    event,
    c("id", "node_id", "url", "actor", "event", "commit_id", "commit_url", "created_at", "issue"))
  expect_identical(event$issue$number, 29L)
})

#  FUNCTION: gh_events ------------------------------------------------------------------------
test_that("gh_events returns a tibble describing the issue events", {
  events <- gh_events("ChadGoymer/githapi", n_max = 10)
  expect_is(events, "tbl")

  expect_identical(
    sapply(events, function(field) class(field)[[1]]),
    c(id           = "numeric",
      event        = "character",
      issue_number = "integer",
      issue_title  = "character",
      created_at   = "POSIXct",
      actor_login  = "character",
      commit_id    = "character",
      url          = "character"))
})
