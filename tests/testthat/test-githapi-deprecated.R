context("githapi")


# TEST: gh_get --------------------------------------------------------------------------------

test_that("gh_get returns a parsed list by default", {
  repo_info <- gh_get(file.path(getOption("github.api"), "repos/ChadGoymer/githapi"))
  expect_is(repo_info, "list")
  expect_identical(repo_info$name, "githapi")
})

test_that("gh_get returns raw text when accept = raw", {
  readme <- gh_get(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/readme"),
    accept = "raw")
  expect_true(is_character(readme) && identical(length(readme), 1L))
  expect_match(readme, "^githapi")
})
