context("git data api")

#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
test_that("gh_git_blob returns the contents of a file in the repository", {
  readme <- gh_git_blob("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "ChadGoymer/githapi")
  expect_true(is.string(readme))
  expect_identical(
    readme,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_git_commit --------------------------------------------------------------------
test_that("gh_git_commit returns a list of information about a commit", {
  git_commit <- gh_git_commit("7ca61bb71f877f462c0b6132759d7c5e507c921f", "ChadGoymer/githapi")
  expect_is(git_commit, "list")
  expect_identical(
    names(git_commit),
    c("sha", "url", "html_url", "author", "committer", "tree", "message", "parents"))
  expect_identical(git_commit$sha, "7ca61bb71f877f462c0b6132759d7c5e507c921f")
  expect_identical(git_commit$author$name, "Chad Goymer")
  expect_identical(git_commit$message, "removed reference to github_url\n\nAlso added gh_readme and gh_commit_sha")
})

#  FUNCTION: gh_git_reference -----------------------------------------------------------------
test_that("gh_git_reference returns information about a git reference", {
  head_master <- gh_git_reference("heads/master", "ChadGoymer/githapi")
  expect_is(head_master, "list")
  expect_identical(head_master$ref, "refs/heads/master")

  tag_v0.1.0 <- gh_git_reference("tags/v0.1.0", "ChadGoymer/githapi")
  expect_is(tag_v0.1.0, "list")
  expect_identical(tag_v0.1.0$ref, "refs/tags/v0.1.0")
})

#  FUNCTION: gh_git_references ----------------------------------------------------------------
test_that("gh_git_references returns a tibble of information about references", {
  references <- gh_git_references("ChadGoymer/githapi")
  expect_is(references, "tbl")
  expect_identical(
    names(references),
    c("name", "type", "object_type", "object_sha", "ref", "url"))
  expect_true(all(c("master", "v0.0.0") %in% references$name))
  expect_true("ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964" %in% references$object_sha)
})
