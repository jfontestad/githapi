context("organizations api")

#  FUNCTION: gh_organizations -----------------------------------------------------------------
test_that("gh_organizations returns a tibble describing the organizations", {
  hadley_orgs <- gh_organizations("hadley")
  expect_is(hadley_orgs, "tbl")
  expect_identical(names(hadley_orgs), c("id", "name", "description", "url"))
  expect_true("tidyverse" %in% hadley_orgs$name)
})
