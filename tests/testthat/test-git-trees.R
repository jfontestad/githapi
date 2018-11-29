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

# TEST: create_tree ---------------------------------------------------------------------------

test_that("create_tree creates a new tree of the specified structure", {
  created_one_tree_one_object <- create_tree(
    repo  = "ChadGoymer/test-githapi",
    paths = "README-copy.md",
    modes = "100644",
    types = "blob",
    shas  = "72b5faa9dc9e4bba87108bf302a5b453e985feec")

  expect_is(created_one_tree_one_object, "tbl")
  expect_identical(
    map_vec(created_one_tree_one_object, function(field) class(field)[[1]]),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  viewed_one_tree_one_object <- view_trees("ChadGoymer/test-githapi", created_one_tree_one_object$tree_sha)

  expect_identical(viewed_one_tree_one_object$path, "README-copy.md")
  expect_identical(viewed_one_tree_one_object$type, "blob")
  expect_identical(viewed_one_tree_one_object$sha, "72b5faa9dc9e4bba87108bf302a5b453e985feec")

  created_one_tree_two_objects <- create_tree(
    repo  = "ChadGoymer/test-githapi",
    paths = c("test-root.txt", "test-dir"),
    modes = c("100644", "040000"),
    types = c("blob", "tree"),
    shas  = c("fea7e317e8d40f5de939e9d183a964a72f14b2c1", created_one_tree_one_object$tree_sha))

  expect_is(created_one_tree_two_objects, "tbl")
  expect_identical(
    map_vec(created_one_tree_two_objects, function(field) class(field)[[1]]),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  viewed_one_tree_two_objects <- view_trees(
    repo = "ChadGoymer/test-githapi",
    created_one_tree_two_objects$tree_sha[[1]])

  expect_true(all(c("test-root.txt", "test-dir") %in% viewed_one_tree_two_objects$path))

  expect_identical(
    viewed_one_tree_two_objects %>% filter(path == "test-root.txt") %>% pull(type),
    "blob")
  expect_identical(
    viewed_one_tree_two_objects %>% filter(path == "test-dir") %>% pull(type),
    "tree")
  expect_identical(
    viewed_one_tree_two_objects %>% filter(path == "test-root.txt") %>% pull(sha),
    "fea7e317e8d40f5de939e9d183a964a72f14b2c1")
  expect_identical(
    viewed_one_tree_two_objects %>% filter(path == "test-dir") %>% pull(sha),
    created_one_tree_one_object$tree_sha)
})
