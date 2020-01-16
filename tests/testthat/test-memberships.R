context("memberships")


# TEST: view_memberships ----------------------------------------------------------------------

test_that("view_memberships returns a tibble summarising the user's memberships", {

  memberships <- view_memberships()

  expect_is(memberships, "tbl")
  expect_identical(attr(memberships, "status"), 200L)
  expect_identical(
    map_chr(memberships, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_true("HairyCoos" %in% memberships$organization)

  hairy_coos <- filter(memberships, .data$organization == "HairyCoos")
  expect_identical(hairy_coos$user, "ChadGoymer")
  expect_identical(hairy_coos$state, "active")
  expect_identical(hairy_coos$role, "admin")

  active_memberships <- view_memberships(state = "active")

  expect_is(active_memberships, "tbl")
  expect_identical(attr(active_memberships, "status"), 200L)
  expect_identical(
    map_chr(active_memberships, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_true("HairyCoos" %in% active_memberships$organization)

  active_hairy_coos <- filter(active_memberships, .data$organization == "HairyCoos")
  expect_identical(active_hairy_coos$user, "ChadGoymer")
  expect_identical(active_hairy_coos$state, "active")
  expect_identical(active_hairy_coos$role, "admin")

  pending_memberships <- view_memberships(state = "pending")

  expect_is(pending_memberships, "tbl")
  expect_identical(attr(pending_memberships, "status"), 200L)
  expect_identical(
    map_chr(pending_memberships, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

})


# TEST: view_membership -----------------------------------------------------------------------

test_that("view_membership returns a list of membership properties", {

  user_membership <- view_membership("ChadGoymer", "HairyCoos")

  expect_is(user_membership, "list")
  expect_identical(attr(user_membership, "status"), 200L)
  expect_identical(
    map_chr(user_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_identical(user_membership$user, "ChadGoymer")
  expect_identical(user_membership$organization, "HairyCoos")
  expect_identical(user_membership$role, "admin")
  expect_identical(user_membership$state, "active")

  team_membership <- view_membership("ChadGoymer", "HairyCoos", "HeadCoos")

  expect_is(team_membership, "list")
  expect_identical(attr(team_membership, "status"), 200L)
  expect_identical(
    map_chr(team_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"))

  expect_identical(team_membership$user, "ChadGoymer")
  expect_identical(team_membership$organization, "HairyCoos")
  expect_identical(team_membership$team, "HeadCoos")
  expect_identical(team_membership$role, "maintainer")
  expect_identical(team_membership$state, "active")

})
