context("issues")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  create_repository(
    name        = str_c("test-issues-", now),
    description = "This is a repository to test issues")

  create_repository(
    name        = str_c("test-issues-", now),
    org         = "HairyCoos",
    description = "This is a repository to test issues")

  create_milestone(
    title       = str_c("test-issues-", now),
    repo        = str_c("ChadGoymer/test-issues-", now),
    description = "This is a milestone to test issues")

  create_label(
    name        = str_c("test-issues-", now),
    repo        = str_c("ChadGoymer/test-issues-", now),
    description = "This is a label to test issues")

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-issues-", now))
  delete_repository(str_c("HairyCoos/test-issues-", now))

})))


# TEST: create_issue --------------------------------------------------------------------------

test_that("create_issue creates an issue and returns a list of the properties", {

  user_issue <- create_issue(
    title     = str_c("test user issue ", now),
    repo      = str_c("ChadGoymer/test-issues-", now),
    body      = "This is an issue to test create_issue()",
    assignees = "ChadGoymer",
    labels    = str_c("test-issues-", now),
    milestone = str_c("test-issues-", now))

  expect_is(user_issue, "list")
  expect_identical(attr(user_issue, "status"), 201L)
  expect_identical(
    map_chr(user_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(user_issue$title, str_c("test user issue ", now))
  expect_identical(user_issue$body, "This is an issue to test create_issue()")
  expect_identical(user_issue$assignees, "ChadGoymer")
  expect_identical(user_issue$labels, str_c("test-issues-", now))
  expect_identical(user_issue$milestone, str_c("test-issues-", now))
  expect_identical(user_issue$state, "open")
  expect_identical(user_issue$repository, str_c("ChadGoymer/test-issues-", now))
  expect_false(user_issue$pull_request)
  expect_identical(user_issue$creator, "ChadGoymer")

  org_issue <- create_issue(
    title     = str_c("test organization issue ", now),
    repo      = str_c("HairyCoos/test-issues-", now),
    body      = "This is an issue to test create_issue()",
    assignees = "ChadGoymer")

  expect_is(org_issue, "list")
  expect_identical(attr(org_issue, "status"), 201L)
  expect_identical(
    map_chr(org_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(org_issue$title, str_c("test organization issue ", now))
  expect_identical(org_issue$body, "This is an issue to test create_issue()")
  expect_identical(org_issue$assignees, "ChadGoymer")
  expect_identical(org_issue$state, "open")
  expect_identical(org_issue$repository, str_c("HairyCoos/test-issues-", now))
  expect_false(org_issue$pull_request)
  expect_identical(org_issue$creator, "ChadGoymer")

})


# TEST: update_issue --------------------------------------------------------------------------

test_that("update_issue changes a milestone and returns a list of the properties", {

  updated_issue <- update_issue(
    issue     = str_c("test user issue ", now),
    repo      = str_c("ChadGoymer/test-issues-", now),
    title     = str_c("test updated issue ", now),
    body      = "This is an issue to test update_issue()",
    assignees = NULL,
    labels    = NULL,
    milestone = NULL)

  expect_is(updated_issue, "list")
  expect_identical(attr(updated_issue, "status"), 200L)
  expect_identical(
    map_chr(updated_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(updated_issue$title, str_c("test updated issue ", now))
  expect_identical(updated_issue$body, "This is an issue to test update_issue()")
  expect_identical(updated_issue$assignees, character())
  expect_identical(updated_issue$labels, character())
  expect_identical(updated_issue$milestone, NA_character_)
  expect_identical(updated_issue$state, "open")
  expect_identical(updated_issue$repository, str_c("ChadGoymer/test-issues-", now))
  expect_false(updated_issue$pull_request)
  expect_identical(updated_issue$creator, "ChadGoymer")

  assigned_issue <- update_issue(
    issue     = str_c("test updated issue ", now),
    repo      = str_c("ChadGoymer/test-issues-", now),
    labels    = str_c("test-issues-", now),
    milestone = str_c("test-issues-", now))

  expect_is(assigned_issue, "list")
  expect_identical(attr(assigned_issue, "status"), 200L)
  expect_identical(
    map_chr(assigned_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(assigned_issue$labels, str_c("test-issues-", now))
  expect_identical(assigned_issue$milestone, str_c("test-issues-", now))

  closed_issue <- update_issue(
    issue = str_c("test organization issue ", now),
    repo  = str_c("HairyCoos/test-issues-", now),
    state = "closed")

  expect_is(closed_issue, "list")
  expect_identical(attr(closed_issue, "status"), 200L)
  expect_identical(
    map_chr(closed_issue, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(closed_issue$title, str_c("test organization issue ", now))
  expect_identical(closed_issue$body, "This is an issue to test create_issue()")
  expect_identical(closed_issue$assignees, "ChadGoymer")
  expect_identical(closed_issue$state, "closed")
  expect_identical(closed_issue$repository, str_c("HairyCoos/test-issues-", now))
  expect_false(closed_issue$pull_request)
  expect_identical(closed_issue$creator, "ChadGoymer")

})


# TEST: view_issues ---------------------------------------------------------------------------

test_that("view_issues returns a tibble of issue properties", {

  open_repo_issues <- view_issues(str_c("ChadGoymer/test-issues-", now))

  expect_is(open_repo_issues, "tbl")
  expect_identical(attr(open_repo_issues, "status"), 200L)
  expect_identical(
    map_chr(open_repo_issues, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "list",
      labels       = "list",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_true(str_c("test updated issue ", now) %in% open_repo_issues$title)

  filtered_repo_issues <- view_issues(
    repo      = str_c("ChadGoymer/test-issues-", now),
    since     = "2020-01-01 00:00:00",
    labels    = str_c("test-issues-", now),
    milestone = str_c("test-issues-", now))

  expect_is(filtered_repo_issues, "tbl")
  expect_identical(attr(filtered_repo_issues, "status"), 200L)
  expect_identical(
    map_chr(filtered_repo_issues, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "list",
      labels       = "list",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_true(str_c("test updated issue ", now) %in% filtered_repo_issues$title)

  org_issues <- view_issues(org = "HairyCoos", state = "closed")

  expect_is(org_issues, "tbl")
  expect_identical(attr(org_issues, "status"), 200L)
  expect_identical(
    map_chr(org_issues, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "list",
      labels       = "list",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_true(str_c("test organization issue ", now) %in% org_issues$title)

  user_issues <- view_issues()

  expect_is(user_issues, "tbl")
  expect_identical(attr(user_issues, "status"), 200L)
  expect_identical(
    map_chr(user_issues, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "list",
      labels       = "list",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_true(all(str_detect(user_issues$assignees, "ChadGoymer")))

})

test_that("view_issues throws an error if invalid arguments are supplied", {

  expect_error(
    view_issues(str_c("ChadGoymer/test-issues-", now), since = "Bob"),
    "'since' must be specified in the format 'YYYY-MM-DD hh:mm:ss'")

})


# TEST: view_issue ----------------------------------------------------------------------------

test_that("view_issue returns a list of issue properties", {

  user_issue_number <- view_issue(1, str_c("ChadGoymer/test-issues-", now))

  expect_is(user_issue_number, "list")
  expect_identical(attr(user_issue_number, "status"), 200L)
  expect_identical(
    map_chr(user_issue_number, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(user_issue_number$title, str_c("test updated issue ", now))

  user_issue_title <- view_issue(str_c("test updated issue ", now), str_c("ChadGoymer/test-issues-", now))

  expect_is(user_issue_title, "list")
  expect_identical(attr(user_issue_title, "status"), 200L)
  expect_identical(
    map_chr(user_issue_title, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(user_issue_title$title, str_c("test updated issue ", now))

  org_issue_number <- view_issue(1, str_c("HairyCoos/test-issues-", now))

  expect_is(org_issue_number, "list")
  expect_identical(attr(org_issue_number, "status"), 200L)
  expect_identical(
    map_chr(org_issue_number, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(org_issue_number$title, str_c("test organization issue ", now))

  org_issue_title <- view_issue(
    issue = str_c("test organization issue ", now),
    repo  = str_c("HairyCoos/test-issues-", now))

  expect_is(org_issue_title, "list")
  expect_identical(attr(org_issue_title, "status"), 200L)
  expect_identical(
    map_chr(org_issue_title, ~ class(.)[[1]]),
    c(number       = "integer",
      title        = "character",
      body         = "character",
      assignees    = "character",
      labels       = "character",
      milestone    = "character",
      state        = "character",
      repository   = "character",
      pull_request = "logical",
      html_url     = "character",
      creator      = "character",
      created_at   = "POSIXct",
      updated_at   = "POSIXct",
      closed_at    = "POSIXct"))

  expect_identical(org_issue_title$title, str_c("test organization issue ", now))

})

test_that("view_issue throws an error if invalid arguments are supplied", {

  expect_error(
    view_issue(TRUE, repo = str_c("ChadGoymer/test-issues-", now)),
    "'issue' must be either an integer or a string")

})


# TEST: browse_issue --------------------------------------------------------------------------

test_that("browse_issue opens the issue's page in the browser", {

  skip_if(!interactive(), "browse_issue must be tested manually")

  issue_url <- browse_issue(1, repo = str_c("ChadGoymer/test-issues-", now))

  expect_is(issue_url, "character")
  expect_identical(attr(issue_url, "status"), 200L)
  expect_identical(
    as.character(issue_url),
    str_c("https://github.com/ChadGoymer/test-issues-", now, "/issues/1"))

})
