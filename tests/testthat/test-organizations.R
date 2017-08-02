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
