context("organizations api")


# TEST: view_organizations --------------------------------------------------------------------

test_that("view_organizations returns a tibble summarising the organizations", {

  user_organizations <- view_organizations(user = "ChadGoymer")

  expect_is(user_organizations, "tbl")
  expect_identical(attr(user_organizations, "status"), 200L)
  expect_identical(
    map_chr(user_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

  expect_true("HairyCoos" %in% user_organizations$login)

  auth_organizations <- view_organizations(user = NULL)

  expect_is(auth_organizations, "tbl")
  expect_identical(attr(auth_organizations, "status"), 200L)
  expect_identical(
    map_chr(auth_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

  expect_true("HairyCoos" %in% auth_organizations$login)

  all_organizations <- view_organizations(n_max = 10)

  expect_is(all_organizations, "tbl")
  expect_identical(attr(all_organizations, "status"), 200L)
  expect_identical(
    map_chr(all_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

})


# TEST: view_organization ---------------------------------------------------------------------

test_that("view_organization returns a list of organization properties", {

  organization <- view_organization("HairyCoos")

  expect_is(organization, "list")
  expect_identical(attr(organization, "status"), 200L)
  expect_identical(
    map_chr(organization, ~ class(.)[[1]]),
    c(id                                       = "integer",
      login                                    = "character",
      name                                     = "character",
      description                              = "character",
      company                                  = "character",
      blog                                     = "character",
      location                                 = "character",
      email                                    = "character",
      is_verified                              = "logical",
      has_organization_projects                = "logical",
      has_repository_projects                  = "logical",
      public_repos                             = "integer",
      public_gists                             = "integer",
      html_url                                 = "character",
      created_at                               = "POSIXct",
      total_private_repos                      = "integer",
      owned_private_repos                      = "integer",
      private_gists                            = "integer",
      disk_usage                               = "numeric",
      collaborators                            = "integer",
      billing_email                            = "character",
      plan_name                                = "character",
      plan_space                               = "integer",
      plan_private_repos                       = "integer",
      default_repository_permission            = "character",
      two_factor_requirement_enabled           = "logical",
      members_can_create_repositories          = "logical"))

  expect_identical(organization$login, "HairyCoos")

})


# TEST: browse_organization -------------------------------------------------------------------

test_that("browse_organization opens the organization's page in the browser", {

  skip_if(!interactive(), "browse_organization must be tested manually")

  organization <- browse_organization("HairyCoos")

  expect_is(organization, "character")
  expect_identical(attr(organization, "status"), 200L)
  expect_identical(as.character(organization), "https://github.com/HairyCoos")

})


# TEST: update_organization -------------------------------------------------------------------

test_that("update_organization changes the organization's properties", {

  original_organization <- view_organization("HairyCoos")

  on.exit({
    update_organization(
      organization                    = "HairyCoos",
      name                            = original_organization$name,
      description                     = original_organization$description,
      email                           = original_organization$email,
      location                        = original_organization$location,
      company                         = original_organization$company,
      billing_email                   = original_organization$billing_email,
      has_organization_projects       = original_organization$has_organization_projects,
      has_repository_projects         = original_organization$has_repository_projects,
      default_repository_permission   = original_organization$default_repository_permission,
      members_can_create_repositories = original_organization$members_can_create_repositories)
  })

  updated_organization <- update_organization(
    organization                    = "HairyCoos",
    name                            = "ACME",
    description                     = "ACME Trading Co",
    email                           = original_organization$email,
    location                        = "The desert",
    company                         = "ACME",
    billing_email                   = original_organization$billing_email,
    has_organization_projects       = FALSE,
    has_repository_projects         = FALSE,
    default_repository_permission   = "write",
    members_can_create_repositories = FALSE)

  expect_is(updated_organization, "list")
  expect_identical(attr(updated_organization, "status"), 200L)
  expect_identical(
    map_chr(updated_organization, ~ class(.)[[1]]),
    c(id                                       = "integer",
      login                                    = "character",
      name                                     = "character",
      description                              = "character",
      company                                  = "character",
      blog                                     = "character",
      location                                 = "character",
      email                                    = "character",
      is_verified                              = "logical",
      has_organization_projects                = "logical",
      has_repository_projects                  = "logical",
      public_repos                             = "integer",
      public_gists                             = "integer",
      html_url                                 = "character",
      created_at                               = "POSIXct",
      total_private_repos                      = "integer",
      owned_private_repos                      = "integer",
      private_gists                            = "integer",
      disk_usage                               = "numeric",
      collaborators                            = "integer",
      billing_email                            = "character",
      plan_name                                = "character",
      plan_space                               = "integer",
      plan_private_repos                       = "integer",
      default_repository_permission            = "character",
      two_factor_requirement_enabled           = "logical",
      members_can_create_repositories          = "logical"))

  expect_identical(updated_organization$login, "HairyCoos")
  expect_identical(updated_organization$name, "ACME")
  expect_identical(updated_organization$description, "ACME Trading Co")
  expect_identical(updated_organization$location, "The desert")
  expect_identical(updated_organization$company, "ACME")
  expect_false(updated_organization$has_organization_projects)
  expect_false(updated_organization$has_repository_projects)
  expect_identical(updated_organization$default_repository_permission, "write")
  expect_false(updated_organization$members_can_create_repositories)

})


# TEST: view_memberships ----------------------------------------------------------------------

test_that("view_memberships returns a tibble summarising the user's memberships", {

  memberships <- view_memberships()

  expect_is(memberships, "tbl")
  expect_identical(attr(memberships, "status"), 200L)
  expect_identical(
    map_chr(memberships, ~ class(.)[[1]]),
    c(organization = "character",
      user         = "character",
      state        = "character",
      role         = "character"))

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
    c(organization = "character",
      user         = "character",
      state        = "character",
      role         = "character"))

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
    c(organization = "character",
      user         = "character",
      state        = "character",
      role         = "character"))

})


# TEST: view_membership -----------------------------------------------------------------------

test_that("view_membership returns a list of membership properties", {

  user_membership <- view_membership("HairyCoos", "ChadGoymer")

  expect_is(user_membership, "list")
  expect_identical(attr(user_membership, "status"), 200L)
  expect_identical(
    map_chr(user_membership, ~ class(.)[[1]]),
    c(organization = "character",
      user         = "character",
      state        = "character",
      role         = "character"))

  expect_identical(user_membership$organization, "HairyCoos")
  expect_identical(user_membership$user, "ChadGoymer")
  expect_identical(user_membership$state, "active")
  expect_identical(user_membership$role, "admin")

  auth_membership <- view_membership("HairyCoos")

  expect_is(auth_membership, "list")
  expect_identical(attr(auth_membership, "status"), 200L)
  expect_identical(
    map_chr(auth_membership, ~ class(.)[[1]]),
    c(organization = "character",
      user         = "character",
      state        = "character",
      role         = "character"))

  expect_identical(auth_membership$organization, "HairyCoos")
  expect_identical(auth_membership$user, "ChadGoymer")
  expect_identical(auth_membership$state, "active")
  expect_identical(auth_membership$role, "admin")

})
