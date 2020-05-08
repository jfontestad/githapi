context("memberships")


# TEST: update_membership ---------------------------------------------------------------------

test_that("update_membership returns a list of membership properties", {

  org_membership <- update_membership("ChadGoymer2", "HairyCoos")

  expect_is(org_membership, "list")
  expect_identical(attr(org_membership, "status"), 200L)
  expect_identical(
    map_chr(org_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_identical(org_membership$user, "ChadGoymer2")
  expect_identical(org_membership$organization, "HairyCoos")
  expect_identical(org_membership$role, "member")
  expect_identical(org_membership$state, "pending")

  org_role_membership <- update_membership("ChadGoymer2", "HairyCoos", role = "admin")

  expect_is(org_role_membership, "list")
  expect_identical(attr(org_role_membership, "status"), 200L)
  expect_identical(
    map_chr(org_role_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_identical(org_role_membership$user, "ChadGoymer2")
  expect_identical(org_role_membership$organization, "HairyCoos")
  expect_identical(org_role_membership$role, "admin")
  expect_identical(org_role_membership$state, "pending")


  team_membership <- update_membership("ChadGoymer2", "HairyCoos", "HeadCoos")

  expect_is(team_membership, "list")
  expect_identical(attr(team_membership, "status"), 200L)
  expect_identical(
    map_chr(team_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"))

  expect_identical(team_membership$user, "ChadGoymer2")
  expect_identical(team_membership$organization, "HairyCoos")
  expect_identical(team_membership$team, "HeadCoos")
  expect_identical(team_membership$role, "member")
  expect_identical(team_membership$state, "pending")

  team_role_membership <- update_membership("ChadGoymer2", "HairyCoos", "HeadCoos", role = "maintainer")

  expect_is(team_role_membership, "list")
  expect_identical(attr(team_role_membership, "status"), 200L)
  expect_identical(
    map_chr(team_role_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"))

  expect_identical(team_role_membership$user, "ChadGoymer2")
  expect_identical(team_role_membership$organization, "HairyCoos")
  expect_identical(team_role_membership$team, "HeadCoos")
  expect_identical(team_role_membership$role, "maintainer")
  expect_identical(team_role_membership$state, "pending")

})


# TEST: view_memberships ----------------------------------------------------------------------

test_that("view_memberships returns a tibble summarising the user's memberships", {

  memberships <- view_memberships(n_max = 10)

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

  active_memberships <- view_memberships(state = "active", n_max = 10)

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

  pending_memberships <- view_memberships(state = "pending", n_max = 10)

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

  user_membership <- view_membership("ChadGoymer2", "HairyCoos")

  expect_is(user_membership, "list")
  expect_identical(attr(user_membership, "status"), 200L)
  expect_identical(
    map_chr(user_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      role         = "character",
      state        = "character"))

  expect_identical(user_membership$user, "ChadGoymer2")
  expect_identical(user_membership$organization, "HairyCoos")
  expect_identical(user_membership$role, "admin")
  expect_identical(user_membership$state, "pending")

  team_membership <- view_membership("ChadGoymer2", "HairyCoos", "HeadCoos")

  expect_is(team_membership, "list")
  expect_identical(attr(team_membership, "status"), 200L)
  expect_identical(
    map_chr(team_membership, ~ class(.)[[1]]),
    c(user         = "character",
      organization = "character",
      team         = "character",
      role         = "character",
      state        = "character"))

  expect_identical(team_membership$user, "ChadGoymer2")
  expect_identical(team_membership$organization, "HairyCoos")
  expect_identical(team_membership$team, "HeadCoos")
  expect_identical(team_membership$role, "maintainer")
  expect_identical(team_membership$state, "pending")

})


# TEST: delete_membership ---------------------------------------------------------------------

test_that("delete_membership removes users from an organization or team", {

  team_membership <- delete_membership("ChadGoymer2", "HairyCoos", "HeadCoos")

  expect_is(team_membership, "logical")
  expect_identical(attr(team_membership, "status"), 204L)
  expect_identical(as.logical(team_membership), TRUE)

  org_membership <- delete_membership("ChadGoymer2", "HairyCoos")

  expect_is(org_membership, "logical")
  expect_identical(attr(org_membership, "status"), 204L)
  expect_identical(as.logical(org_membership), TRUE)

})
