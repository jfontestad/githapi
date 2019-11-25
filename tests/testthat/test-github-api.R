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


# TEST: gh_request ----------------------------------------------------------------------------

test_that("gh_request can GET, POST and DELETE a tag in the specified repository", {

  created_tag <- gh_request(
    url     = "https://api.github.com/repos/ChadGoymer/githapi/git/refs",
    type    = "POST",
    payload = list(
      ref = "refs/tags/zzz",
      sha = "099944f501b2c2fba940f807b1028dbc5349f29c"))
  expect_is(created_tag, "list")
  expect_identical(created_tag$ref, "refs/tags/zzz")
  expect_identical(created_tag$object$sha, "099944f501b2c2fba940f807b1028dbc5349f29c")

  expect_identical(attr(created_tag, "url"), "https://api.github.com/repos/ChadGoymer/githapi/git/refs")
  expect_identical(attr(created_tag, "request"), "POST")
  expect_identical(attr(created_tag, "status"), 201L)
  expect_true(length(attr(created_tag, "header")) > 1)

  viewed_tag <- gh_request("https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz", "GET")
  expect_is(viewed_tag, "list")
  expect_identical(viewed_tag$ref, "refs/tags/zzz")
  expect_identical(viewed_tag$object$sha, "099944f501b2c2fba940f807b1028dbc5349f29c")

  expect_identical(attr(viewed_tag, "url"), "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz")
  expect_identical(attr(viewed_tag, "request"), "GET")
  expect_identical(attr(viewed_tag, "status"), 200L)
  expect_true(length(attr(viewed_tag, "header")) > 1)

  updated_tag <- gh_request(
    url     = "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz",
    type    = "PATCH",
    payload = list(sha = "ec311b3fa4ce294fadfc01dbfcfb35db3c88feda"))
  expect_is(updated_tag, "list")
  expect_identical(updated_tag$ref, "refs/tags/zzz")
  expect_identical(updated_tag$object$sha, "ec311b3fa4ce294fadfc01dbfcfb35db3c88feda")

  expect_identical(attr(updated_tag, "url"), "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz")
  expect_identical(attr(updated_tag, "request"), "PATCH")
  expect_identical(attr(updated_tag, "status"), 200L)
  expect_true(length(attr(updated_tag, "header")) > 1)

  deleted_tag <- gh_request("https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz", "DELETE")
  expect_is(deleted_tag, "list")

  expect_identical(attr(deleted_tag, "url"), "https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz")
  expect_identical(attr(deleted_tag, "request"), "DELETE")
  expect_identical(attr(deleted_tag, "status"), 204L)
  expect_true(length(attr(deleted_tag, "header")) > 1)

  expect_error(gh_request("https://api.github.com/repos/ChadGoymer/githapi/git/refs/tags/zzz", "GET"))

})
