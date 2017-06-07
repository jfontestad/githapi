context("repositories api")

#  FUNCTION: gh_branch ------------------------------------------------------------------------
test_that("gh_branch returns information about a specific branch in the repository",
{
  branch <- gh_branch("master", "ChadGoymer/githapi")
  expect_identical(branch$name, "master")
})

#  FUNCTION: gh_branches ------------------------------------------------------------------
test_that("gh_branches returns a tibble of information about the branches",
{
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
