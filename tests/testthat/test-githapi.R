context("github api")

#  FUNCTION: gh_url ----------------------------------------------------------------------------
test_that("gh_url returns a valid URL for the GitHub API", {
  expect_identical(
    gh_url("repos"),
    file.path(getOption("github.api"), "repos"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/refs", "heads/master"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/refs", "heads/master")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1)),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1, type = "bob"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1, type = "bob")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list()),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384"))
})

# FUNCTION: gh_get ---------------------------------------------------------------------------
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

# FUNCTION: gh_page ---------------------------------------------------------------------------
test_that("gh_page returns a list of specified length", {
  commits <- gh_page(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits"),
    n_max = 20)
  expect_is(commits, "list")
  expect_identical(length(commits), 20L)
})
