context("repositories api")

#  FUNCTION: gh_repo --------------------------------------------------------------------------
test_that("gh_repo returns a list describing the repository", {
  repo <- gh_repo("ChadGoymer/githapi")
  expect_is(repo, "list")
  expect_identical(repo$name, "githapi")
  expect_identical(repo$owner$login, "ChadGoymer")
})

test_that("gh_repo returns an error is the specified repo does not exist", {
  expect_error(gh_repo("SomeNameThatDoesNotExist/repo"), "Specified repo does not exist in GitHub: 'SomeNameThatDoesNotExist/repo'")
})

#  FUNCTION: gh_repos -------------------------------------------------------------------------
test_that("gh_repos returns a tibble describing all the repositories a user has", {
  repos <- gh_repos("ChadGoymer")
  expect_true(is_tibble(repos))
  expect_true("githapi" %in% repos$name)
  expect_identical(repos$name, sort(repos$name))

  repos <- gh_repos("ChadGoymer", sort = "updated")
  expect_identical(repos$updated_at, sort(repos$updated_at, decreasing = TRUE))
})

test_that("gh_repos returns a tibble describing all the repositories an org has", {
  repos <- gh_repos("tidyverse")
  expect_true(is_tibble(repos))
  expect_true(all(c("dplyr", "tidyr") %in% repos$name))
})

test_that("gh_repos returns an error is the specified owner does not exist", {
  expect_error(gh_repos("SomeNameThatDoesNotExist"), "Specified owner does not exist in GitHub: 'SomeNameThatDoesNotExist'")
})

#  FUNCTION: gh_tags ----------------------------------------------------------------------
test_that("gh_tags returns a tibble describing all the tags", {
  tags <- gh_tags("ChadGoymer/githapi")
  expect_true(is_tibble(tags))
  expect_true("0.0.0" %in% tags$name)
  expect_identical(names(tags), c("name", "sha", "url", "zipball_url", "tarball_url"))
})

test_that("gh_tags returns an error is the specified repo does not exist", {
  expect_error(
    gh_tags("SomeNameThatDoesNotExist/repo"),
    "Specified repo does not exist in GitHub: 'SomeNameThatDoesNotExist/repo'")
})

#  FUNCTION: gh_branch ------------------------------------------------------------------------
test_that("gh_branch returns a list describing the branch", {
  branch <- gh_branch("master", "ChadGoymer/githapi")
  expect_is(branch, "list")
  expect_identical(branch$name, "master")
})

test_that("gh_branch returns an error is the specified branch or repo does not exist", {
  expect_error(
    gh_branch("no_branch", "ChadGoymer/githapi"),
    "Specified branch or repo does not exist in GitHub: 'no_branch', 'ChadGoymer/githapi'")
  expect_error(
    gh_branch("master", "SomeNameThatDoesNotExist/repo"),
    "Specified branch or repo does not exist in GitHub: 'master', 'SomeNameThatDoesNotExist/repo'")
})

#  FUNCTION: gh_branches ------------------------------------------------------------------
test_that("gh_branches returns a tibble describing all the branches", {
  branches <- gh_branches("ChadGoymer/githapi")
  expect_true(is_tibble(branches))
  expect_true("master" %in% branches$name)
  expect_identical(names(branches), c("name", "sha", "url"))

  branches_ext <- gh_branches("ChadGoymer/githapi", extended = TRUE)
  expect_true(is_tibble(branches_ext))
  expect_identical(
    names(branches_ext),
    c("name", "sha", "date", "author_name", "author_email", "committer_name",
      "committer_email", "message", "tree_sha", "tree_url", "url"))
})

test_that("gh_branches returns an error is the specified repo does not exist", {
  expect_error(
    gh_branches("SomeNameThatDoesNotExist/repo"),
    "Specified repo does not exist in GitHub: 'SomeNameThatDoesNotExist/repo'")
})
