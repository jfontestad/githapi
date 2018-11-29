context("git trees")

# TEST: view_trees ----------------------------------------------------------------------------

test_that("view_trees returns a tibble of information about the files in commits", {
  trees <- view_trees(
    repo = "ChadGoymer/test-githapi",
    shas = c("3f5c0749c85cc4a3cbd240762b61276ad2bfbba2", "3d1b16c8e39c0776010ab8c6dc6d304ff75b1a61"))

  expect_is(trees, "tbl")
  expect_identical(
    map_vec(trees, function(field) class(field)[[1]]),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  expect_identical(trees$path, c("README.md", "test-file.txt", "README.md"))
  expect_identical(
    trees$sha,
    c("72b5faa9dc9e4bba87108bf302a5b453e985feec",
      "fea7e317e8d40f5de939e9d183a964a72f14b2c1",
      "72b5faa9dc9e4bba87108bf302a5b453e985feec"))
})
