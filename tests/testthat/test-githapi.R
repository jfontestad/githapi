context("github api")

# TEST: gh_url --------------------------------------------------------------------------------

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

# TEST: gh_page -------------------------------------------------------------------------------

test_that("gh_page returns a list of specified length", {
  commits <- gh_page(
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits"),
    n_max = 20)
  expect_is(commits, "list")
  expect_identical(length(commits), 20L)
})

# TEST: gh_request ----------------------------------------------------------------------------

test_that("gh_request can GET, POST and DELETE a tag in the specified repository", {
  created_tag <- gh_request(
    "POST", "https://api.github.com/repos/ChadGoymer/githapi/git/refs",
    payload = list(
      ref = "refs/tags/zzz",
      sha = "099944f501b2c2fba940f807b1028dbc5349f29c"))
  expect_is(created_tag, "list")
  expect_identical(created_tag$ref, "refs/tags/zzz")

  viewed_tag <- gh_request("GET", "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz")
  expect_is(viewed_tag, "list")
  expect_identical(viewed_tag$ref, "refs/tags/zzz")

  gh_request("DELETE", "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz")
  expect_error(
    gh_request("GET", "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz"))
})
