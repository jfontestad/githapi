context("repositories api")

#  FUNCTION: gh_repo --------------------------------------------------------------------------
test_that("gh_repo returns a list describing the repository", {
  repo <- gh_repo("ChadGoymer/githapi")
  expect_is(repo, "list")
  expect_identical(repo$name, "githapi")
  expect_identical(repo$owner$login, "ChadGoymer")
})

test_that("gh_repo returns an error is the specified repo does not exist", {
  expect_error(gh_repo("SomeNameThatDoesNotExist/repo"))
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
  expect_error(gh_repos("SomeNameThatDoesNotExist"))
})

#  FUNCTION: gh_tags ----------------------------------------------------------------------
test_that("gh_tags returns a tibble describing all the tags", {
  tags <- gh_tags("ChadGoymer/githapi")
  expect_true(is_tibble(tags))
  expect_true("0.0.0" %in% tags$name)
  expect_identical(names(tags), c("name", "sha", "url", "zipball_url", "tarball_url"))
})

test_that("gh_tags returns an error is the specified repo does not exist", {
  expect_error(gh_tags("SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_branch ------------------------------------------------------------------------
test_that("gh_branch returns a list describing the branch", {
  branch <- gh_branch("master", "ChadGoymer/githapi")
  expect_is(branch, "list")
  expect_identical(branch$name, "master")
})

test_that("gh_branch returns an error is the specified branch or repo does not exist", {
  expect_error(gh_branch("no_branch", "ChadGoymer/githapi"))
  expect_error(gh_branch("master", "SomeNameThatDoesNotExist/repo"))
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
  expect_error(gh_branches("SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_commit ------------------------------------------------------------------------
test_that("gh_commit returns a list describing the commit", {
  commit_master <- gh_commit("master", "ChadGoymer/githapi")
  expect_is(commit_master, "list")
  expect_true(all(c("sha", "commit", "author", "committer", "files") %in% names(commit_master)))

  commit_d9fe50f <- gh_commit("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi")
  expect_identical(commit_d9fe50f$sha, "d9fe50f8e31d7430df2c5b02442dffb68c854f08")
  expect_identical(commit_d9fe50f$commit$message, "Initial commit")
  expect_identical(commit_d9fe50f$author$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$committer$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$files[[1]]$filename, "README.md")
})

test_that("gh_commit returns an error is the specified commit or repo does not exist", {
  expect_error(gh_commit("no_commit", "ChadGoymer/githapi"))
  expect_error(gh_commit("master", "SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_commits -------------------------------------------------------------------
test_that("gh_commits returns a tibble describing all the commits on a branch", {
  commits <- gh_commits("master", "ChadGoymer/githapi")
  expect_true(is_tibble(commits))
  expect_true("d9fe50f8e31d7430df2c5b02442dffb68c854f08" %in% commits$sha)
  expect_identical(
    names(commits),
    c("sha", "date", "author_name", "author_email", "committer_name", "committer_email",
      "message", "tree_sha", "tree_url", "url"))
})

test_that("gh_commits returns the number of commits specified with extended results", {
  commits_ext <- gh_commits("master", "ChadGoymer/githapi", limit = 2, extended = TRUE)
  expect_true(is_tibble(commits_ext))
  expect_identical(nrow(commits_ext), 2L)
  expect_identical(
    names(commits_ext),
    c("sha", "date", "author_name", "author_email", "committer_name", "committer_email",
      "message", "tree_sha", "tree_url", "url", "files"))
})

test_that("gh_commits returns an error is the specified repo does not exist", {
  expect_error(gh_commits("master", "SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_compare_commits ---------------------------------------------------------------
test_that("gh_compare_commits returns information on the differences between two commits", {
  comparison <- gh_compare_commits(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi")

  expect_true(is.tibble(comparison))

  expect_identical(
    names(comparison),
    c("sha", "date", "author_name", "author_email",
      "committer_name", "committer_email", "message", "url"))

  expect_identical(
    comparison$sha,
    c("2a1e6b031d75bb97d94ab9ee26272a052da83339", "d8a62ccdc3df3e002dbac55390772424b136844a"))
})

#  FUNCTION: gh_compare_files -----------------------------------------------------------------
test_that("gh_compare_files returns a tibble of information of file differences between commits", {
  comparison <- gh_compare_files(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi")

  expect_true(is.tibble(comparison))

  expect_identical(
    names(comparison),
    c("filename", "status", "additions", "deletions", "changes", "contents_url"))

  expect_identical(
    comparison$filename,
    c(".Rbuildignore",
      ".gitignore",
      "DESCRIPTION",
      "NAMESPACE",
      "R/githapi.R",
      "R/repositories.R",
      "R/utilities.R",
      "githapi.Rproj",
      "man/gh_branch.Rd",
      "man/gh_branches.Rd",
      "man/githapi.Rd",
      "tests/testthat.R",
      "tests/testthat/test-repositories.R"))
})
