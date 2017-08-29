context("organizations api")

#  FUNCTION: gh_organization ------------------------------------------------------------------
test_that("gh_organization returns a list describing the organization", {
  tidyverse <- gh_organization("tidyverse")
  expect_is(tidyverse, "list")
  expect_identical(
    names(tidyverse),
    c("login", "id", "url", "repos_url", "events_url", "hooks_url", "issues_url",
      "members_url", "public_members_url", "avatar_url", "description", "name", "company",
      "blog", "location", "email", "has_organization_projects", "has_repository_projects",
      "public_repos", "public_gists", "followers", "following", "html_url", "created_at",
      "updated_at", "type"))
  expect_identical(tidyverse$login, "tidyverse")
})

#  FUNCTION: gh_organizations -----------------------------------------------------------------
test_that("gh_organizations returns a tibble describing the organizations", {
  hadley_orgs <- gh_organizations("hadley")
  expect_is(hadley_orgs, "tbl")
  expect_identical(names(hadley_orgs), c("id", "name", "description", "url"))
  expect_true("tidyverse" %in% hadley_orgs$name)
})

#  FUNCTION: is_member ------------------------------------------------------------------------
test_that("gh_member returns TRUE if user is a member and FALSE otherwise", {
  expect_true(is_member("hadley", "tidyverse"))
  expect_false(is_member("doesnotexist", "tidyverse"))
})

#  FUNCTION: gh_members -----------------------------------------------------------------------
test_that("gh_members returns a tibble describing the members", {
  tidy_members <- gh_members("tidyverse")
  expect_is(tidy_members, "tbl")
  expect_identical(names(tidy_members), c("id", "login", "type", "site_admin", "url"))
  expect_true("hadley" %in% tidy_members$login)
})

#  FUNCTION: gh_membership --------------------------------------------------------------------
#  TODO: set up an organization so it can be tested

#  FUNCTION: gh_memberships -------------------------------------------------------------------
#  TODO: set up an organization so it can be tested

#  FUNCTION: gh_team --------------------------------------------------------------------------
#  TODO: set up an organization so it can be tested

#  FUNCTION: gh_teams -------------------------------------------------------------------------
#  TODO: set up an organization so it can be tested

#  FUNCTION: is_manager -----------------------------------------------------------------------
#  TODO: set up an organization so it can be tested
