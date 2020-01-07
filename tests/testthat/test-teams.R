context("teams api")


# TEST: create_team ------------------------------------------------------------------------

test_that("create_team creates a team and returns its properties", {

  first_team <- create_team(
    name         = "TestTeam",
    organization = "HairyCoos",
    description  = "This is a test team",
    repo_names   = "HairyCoos/test-repo")

  expect_is(first_team, "list")
  expect_identical(attr(first_team, "status"), 201L)
  expect_identical(
    map_chr(first_team, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"))

  expect_identical(first_team$name, "TestTeam")
  expect_identical(first_team$organization, "HairyCoos")
  expect_identical(first_team$description, "This is a test team")
  expect_identical(first_team$repos_count, 1L)

  maintainers_team <- create_team("TestTeam2", "HairyCoos", maintainers = "ChadGoymer")

  expect_is(maintainers_team, "list")
  expect_identical(attr(maintainers_team, "status"), 201L)
  expect_identical(
    map_chr(maintainers_team, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"))

  expect_identical(maintainers_team$name, "TestTeam2")
  expect_identical(maintainers_team$organization, "HairyCoos")
  expect_identical(maintainers_team$members_count, 2L)

  closed_team <- create_team("TestTeam3", "HairyCoos", privacy = "closed")

  expect_is(closed_team, "list")
  expect_identical(attr(closed_team, "status"), 201L)
  expect_identical(
    map_chr(closed_team, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"))

  expect_identical(closed_team$name, "TestTeam3")
  expect_identical(closed_team$organization, "HairyCoos")
  expect_identical(closed_team$privacy, "closed")

  parent_team <- create_team("TestTeam4", "HairyCoos", parent_team = "TestTeam3")

  expect_is(parent_team, "list")
  expect_identical(attr(parent_team, "status"), 201L)
  expect_identical(
    map_chr(parent_team, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"))

  expect_identical(parent_team$name, "TestTeam4")
  expect_identical(parent_team$organization, "HairyCoos")
  expect_identical(parent_team$parent, "TestTeam3")

})


# TEST: update_team ---------------------------------------------------------------------------

test_that("update_team changes the team's properties", {

  original_team <- view_team("TestTeam", "HairyCoos")

  updated_team <- update_team(
    team         = "TestTeam",
    name         = "FirstTeam",
    organization = "HairyCoos",
    description  = "This is a test team",
    privacy      = "closed",
    parent_team  = "TestTeam3")

  expect_is(updated_team, "list")
  expect_identical(attr(updated_team, "status"), 200L)
  expect_identical(
    map_chr(updated_team, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character",
      organization  = "character",
      members_count = "integer",
      repos_count   = "integer",
      created_at    = "POSIXct",
      updated_at    = "POSIXct"))

  expect_identical(updated_team$name, "FirstTeam")
  expect_identical(updated_team$description, "This is a test team")
  expect_identical(updated_team$privacy, "closed")
  expect_identical(updated_team$parent, "TestTeam3")

})


# TEST: view_teams ----------------------------------------------------------------------------

test_that("view_teams returns a tibble summarising the teams", {

  org_teams <- view_teams("HairyCoos")

  expect_is(org_teams, "tbl")
  expect_identical(attr(org_teams, "status"), 200L)
  expect_identical(
    map_chr(org_teams, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"))

  expect_true("FirstTeam" %in% org_teams$name)

  team_teams <- view_teams("HairyCoos", parent_team = "TestTeam3")

  expect_is(team_teams, "tbl")
  expect_identical(attr(team_teams, "status"), 200L)
  expect_identical(
    map_chr(team_teams, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"))

  expect_true("FirstTeam" %in% team_teams$name)

  user_teams <- view_teams()

  expect_is(user_teams, "tbl")
  expect_identical(attr(user_teams, "status"), 200L)
  expect_identical(
    map_chr(user_teams, ~ class(.)[[1]]),
    c(id            = "integer",
      name          = "character",
      slug          = "character",
      description   = "character",
      privacy       = "character",
      permission    = "character",
      parent        = "character",
      html_url      = "character"))

  expect_true("FirstTeam" %in% user_teams$name)

})
