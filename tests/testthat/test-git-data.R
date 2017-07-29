context("git data api")

#  FUNCTION: gh_blob --------------------------------------------------------------------------
test_that("gh_blob returns the contents of a file in the repository", {
  readme <- gh_blob("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "ChadGoymer/githapi")
  expect_true(is.string(readme))
  expect_identical(
    readme,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

