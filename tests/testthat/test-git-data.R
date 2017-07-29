context("git data api")

#  FUNCTION: gh_blob --------------------------------------------------------------------------
test_that("gh_blob returns the contents of a file in the repository", {
  readme <- gh_blob("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "ChadGoymer/githapi")
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
