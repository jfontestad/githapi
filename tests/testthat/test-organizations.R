context("organizations")


# TEST: update_organization ----------------------------------------------------

test_that("update_organization changes the organization's properties", {

  org <- view_organization("HairyCoos")

  on.exit({
    update_organization(
      org                             = "HairyCoos",
      name                            = org$name,
      description                     = org$description,
      email                           = org$email,
      location                        = org$location,
      company                         = org$company,
      billing_email                   = org$billing_email,
      has_organization_projects       = org$has_organization_projects,
      has_repository_projects         = org$has_repository_projects,
      default_repository_permission   = org$default_repository_permission,
      members_can_create_repositories = org$members_can_create_repositories
    )
  })

  updated_organization <- update_organization(
    org                             = "HairyCoos",
    name                            = "ACME",
    description                     = "ACME Trading Co",
    email                           = org$email,
    location                        = "The desert",
    company                         = "ACME",
    billing_email                   = org$billing_email,
    has_organization_projects       = FALSE,
    has_repository_projects         = FALSE,
    default_repository_permission   = "write",
    members_can_create_repositories = FALSE
  )

  expect_is(updated_organization, "list")
  expect_identical(attr(updated_organization, "status"), 200L)
  expect_identical(
    map_chr(updated_organization, ~ class(.)[[1]]),
    c(
      id                                       = "integer",
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
      html_url                                 = "character",
      created_at                               = "POSIXct"
    )
  )

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


# TEST: view_organizations -----------------------------------------------------

test_that("view_organizations returns a tibble summarising the organizations", {

  user_organizations <- view_organizations(user = "ChadGoymer", n_max = 10)

  expect_is(user_organizations, "tbl")
  expect_identical(attr(user_organizations, "status"), 200L)
  expect_identical(
    map_chr(user_organizations, ~ class(.)[[1]]),
    c(
      id          = "integer",
      login       = "character",
      description = "character"
    )
  )

  expect_true("HairyCoos" %in% user_organizations$login)

  auth_organizations <- view_organizations(user = NULL, n_max = 10)

  expect_is(auth_organizations, "tbl")
  expect_identical(attr(auth_organizations, "status"), 200L)
  expect_identical(
    map_chr(auth_organizations, ~ class(.)[[1]]),
    c(
      id          = "integer",
      login       = "character",
      description = "character"
    )
  )

  expect_true("HairyCoos" %in% auth_organizations$login)

  all_organizations <- view_organizations(n_max = 10)

  expect_is(all_organizations, "tbl")
  expect_identical(attr(all_organizations, "status"), 200L)
  expect_identical(
    map_chr(all_organizations, ~ class(.)[[1]]),
    c(
      id          = "integer",
      login       = "character",
      description = "character"
    )
  )

})


# TEST: view_organization ------------------------------------------------------

test_that("view_organization returns a list of organization properties", {

  organization <- view_organization("HairyCoos")

  expect_is(organization, "list")
  expect_identical(attr(organization, "status"), 200L)
  expect_identical(
    map_chr(organization, ~ class(.)[[1]]),
    c(
      id                                       = "integer",
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
      html_url                                 = "character",
      created_at                               = "POSIXct"
    )
  )

  expect_identical(organization$login, "HairyCoos")

})


# TEST: browse_organization ----------------------------------------------------

test_that("browse_organization opens the organization's page in the browser", {

  skip_if(!interactive(), "browse_organization must be tested manually")

  organization <- browse_organization("HairyCoos")

  expect_is(organization, "character")
  expect_identical(attr(organization, "status"), 200L)
  expect_identical(as.character(organization), "https://github.com/HairyCoos")

})
