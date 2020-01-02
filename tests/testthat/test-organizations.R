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
      members_can_create_repositories          = "logical",
      members_can_create_public_repositories   = "logical",
      members_can_create_private_repositories  = "logical",
      members_can_create_internal_repositories = "logical"))

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
