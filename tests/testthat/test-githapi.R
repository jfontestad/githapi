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
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits"))
  expect_identical(
    test_commits[[length(test_commits)]]$sha,
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08")
  expect_identical(test_commits[[length(test_commits)]]$commit$author$name, "Chad Goymer")
  expect_identical(test_commits[[length(test_commits)]]$commit$author$date, "2017-06-05T07:18:30Z")
  expect_identical(test_commits[[length(test_commits)]]$commit$message, "Initial commit")

  # TODO: Add a way to test paging - increase branches or commits in test repo
})

#  FUNCTION: gh_post --------------------------------------------------------------------------
# test_that("gh_post send a message to github", {
#   file.path(getOption("github.api"), "repos/ChadGoymer/githapi-test/git/blobs") %>%
#     gh_post(content = "This is a blob", encoding = "utf-8")
#
#   body_tag <- list(
#     tag = "0.0.1",
#     message = "initial version\n",
#     object = "eb1d2ae3a6a43b144655a472cc20f1e4aea41d21",
#     type = "commit",
#     tagger = list(
#       name = "Chad Goymer",
#       email = "chad.goymer@gmail.com",
#       date = "2017-05-30T18:39:35"))
#
#   file.path(getOption("github.api"), "repos/ChadGoymer/githapi-test/git/tags") %>%
#     gh_post(body_tag)
# })
