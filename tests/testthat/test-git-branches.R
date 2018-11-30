context("git branches")

# TEST: view_branches, create_branches & delete_branches --------------------------------------

test_that("create_branches creates some branches, view_branches retreives them and delete_branches deletes them", {
  all_branches <- view_branches(repo = "ChadGoymer/test-githapi")

  expect_is(all_branches, "tbl")
  expect_identical(
    map_vec(all_branches, function(field) class(field)[[1]]),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_true("test-branch" %in% all_branches$name)

  created_branches <- create_branches(
    branches = c("aaa", "bbb"),
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
    repo = "ChadGoymer/test-githapi")

  expect_is(created_branches, "tbl")
  expect_identical(
    map_vec(created_branches, function(field) class(field)[[1]]),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(created_branches$ref, c("refs/heads/aaa", "refs/heads/bbb"))
  expect_identical(
    created_branches$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  viewed_branches <- view_branches("ChadGoymer/test-githapi", c("aaa", "bbb"))

  expect_is(viewed_branches, "tbl")
  expect_identical(
    map_vec(viewed_branches, function(field) class(field)[[1]]),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(viewed_branches$name, c("aaa", "bbb"))
  expect_identical(
    viewed_branches$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  updated_branches <- update_branches(
    branches = c("aaa", "bbb"),
    shas = c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"),
    repo = "ChadGoymer/test-githapi")

  expect_is(updated_branches, "tbl")
  expect_identical(
    map_vec(updated_branches, function(field) class(field)[[1]]),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(updated_branches$ref, c("refs/heads/aaa", "refs/heads/bbb"))
  expect_identical(
    updated_branches$object_sha,
    c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"))

  delete_results <- delete_branches("ChadGoymer/test-githapi", c("aaa", "bbb"))

  expect_identical(delete_results, list(aaa = TRUE, bbb = TRUE))
  expect_error(view_branches("ChadGoymer/test-githapi", "aaa"), "Not Found")
  expect_error(view_branches("ChadGoymer/test-githapi", "bbb"), "Not Found")
})
