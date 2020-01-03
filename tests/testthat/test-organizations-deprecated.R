context("organizations api")

#  FUNCTION: gh_organization ------------------------------------------------------------------
test_that("gh_organization returns a list describing the organization", {
  tidyverse <- suppressWarnings(gh_organization("tidyverse"))
  expect_is(tidyverse, "list")
  expect_named(
    tidyverse,
    c("login", "id", "node_id", "url", "repos_url", "events_url", "hooks_url", "issues_url",
      "members_url", "public_members_url", "avatar_url", "description", "name", "company",
      "blog", "location", "email", "is_verified", "has_organization_projects",
      "has_repository_projects", "public_repos", "public_gists", "followers", "following",
      "html_url", "created_at", "updated_at", "type"))
  expect_identical(tidyverse$login, "tidyverse")
})

#  FUNCTION: gh_organizations -----------------------------------------------------------------
test_that("gh_organizations returns a tibble describing the organizations", {
  hadley_orgs <- suppressWarnings(gh_organizations("hadley"))
  expect_is(hadley_orgs, "tbl")

  expect_identical(
    sapply(hadley_orgs, function(field) class(field)[[1]]),
    c(id          = "integer",
      name        = "character",
      description = "character",
      url         = "character"))

  expect_true("tidyverse" %in% hadley_orgs$name)
})

#  FUNCTION: is_member ------------------------------------------------------------------------
test_that("gh_member returns TRUE if user is a member and FALSE otherwise", {
  expect_true(is_member("hadley", "tidyverse"))
  expect_false(is_member("doesnotexist", "tidyverse"))
})

#  FUNCTION: gh_members -----------------------------------------------------------------------
test_that("gh_members returns a tibble describing the members", {
  tidy_members <- suppressWarnings(gh_members("tidyverse"))
  expect_is(tidy_members, "tbl")

  expect_identical(
    sapply(tidy_members, function(field) class(field)[[1]]),
    c(id         = "integer",
      login      = "character",
      type       = "character",
      site_admin = "logical",
      url        = "character"))

  expect_true("hadley" %in% tidy_members$login)
})
