context("github api")

#  FUNCTION: gh_url ----------------------------------------------------------------------------
test_that("A suitable URL can be built for the GitHub API", {
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
test_that("getting something from github returns the correct result", {
  repo_info <- gh_get(file.path(getOption("github.api"), "repos/ChadGoymer/githapi")) %>%
    fromJSON(simplifyVector = FALSE)
  expect_identical(repo_info$name, "githapi")
  expect_identical(repo_info$owner$login, "ChadGoymer")

  test_readme <- gh_get(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/readme"))
  expect_match(test_readme, "^# githapi\nUser-friendly access to the GitHub API for R")
})

#  FUNCTION: gh_json --------------------------------------------------------------------------
test_that("gh_json returns the github response parsed into a list", {
  response <- gh_json(file.path(getOption("github.api"), "repos/ChadGoymer/githapi"))
  expect_is(response, "list")
  expect_identical(response$name, "githapi")
})

# FUNCTION: gh_page ---------------------------------------------------------------------------
test_that("paging something from github returns the correct result", {
  test_commits <- gh_page(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits"),
    n_max = 20)
  expect_is(test_commits, "list")
  expect_identical(length(test_commits), 20L)
})

#  FUNCTION: gh_tibble ------------------------------------------------------------------------
test_that("gh_tibble returns the github response parsed into a tibble", {
  test_commits <- gh_tibble(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits"),
    n_max = 20)
  expect_is(test_commits, "tbl")
  expect_identical(nrow(test_commits), 20L)
})
