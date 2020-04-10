context("git trees")

# TEST: view_trees ----------------------------------------------------------------------------

test_that("view_trees returns a tibble of information about the files in commits", {
  trees <- suppressWarnings(view_trees(
    shas = c("3f5c0749c85cc4a3cbd240762b61276ad2bfbba2", "3d1b16c8e39c0776010ab8c6dc6d304ff75b1a61"),
    repo = "ChadGoymer/test-githapi"))

  expect_is(trees, "tbl")
  expect_identical(
    gh_map(trees, function(field) class(field)[[1]], simplify = TRUE),
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
  created_one_tree_one_object <- suppressWarnings(create_tree(
    paths = "README-copy.md",
    modes = "100644",
    types = "blob",
    shas  = "72b5faa9dc9e4bba87108bf302a5b453e985feec",
    repo  = "ChadGoymer/test-githapi"))

  expect_is(created_one_tree_one_object, "tbl")
  expect_identical(
    gh_map(created_one_tree_one_object, function(field) class(field)[[1]], simplify = TRUE),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  viewed_one_tree_one_object <- suppressWarnings(view_trees(created_one_tree_one_object$tree_sha, "ChadGoymer/test-githapi"))

  expect_identical(viewed_one_tree_one_object$path, "README-copy.md")
  expect_identical(viewed_one_tree_one_object$type, "blob")
  expect_identical(viewed_one_tree_one_object$sha, "72b5faa9dc9e4bba87108bf302a5b453e985feec")

  created_one_tree_two_objects <- suppressWarnings(create_tree(
    paths = c("test-root.txt", "test-dir"),
    modes = c("100644", "040000"),
    types = c("blob", "tree"),
    shas  = c("fea7e317e8d40f5de939e9d183a964a72f14b2c1", created_one_tree_one_object$tree_sha),
    repo  = "ChadGoymer/test-githapi"))

  expect_is(created_one_tree_two_objects, "tbl")
  expect_identical(
    gh_map(created_one_tree_two_objects, function(field) class(field)[[1]], simplify = TRUE),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  viewed_one_tree_two_objects <- suppressWarnings(view_trees(
    created_one_tree_two_objects$tree_sha[[1]],
    repo = "ChadGoymer/test-githapi"))

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

# TEST: upload_tree ---------------------------------------------------------------------------

test_that("upload_tree uploads files and directory structure to github", {
  skip_on_travis()

  flat_tree <- suppressWarnings(upload_tree(
    path = system.file("test-data/upload-tree/test-dir", package = "githapi"),
    repo = "ChadGoymer/test-githapi"))

  expect_is(flat_tree, "tbl")
  expect_identical(
    gh_map(flat_tree, function(field) class(field)[[1]], simplify = TRUE),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  expect_identical(flat_tree$path, c("file-in-dir-1.txt", "file-in-dir-2.txt"))
  expect_identical(flat_tree$type, c("blob", "blob"))

  recursive_tree <- suppressWarnings(upload_tree(
    path = system.file("test-data/upload-tree", package = "githapi"),
    repo = "ChadGoymer/test-githapi"))

  expect_is(recursive_tree, "tbl")
  expect_identical(
    gh_map(recursive_tree, function(field) class(field)[[1]], simplify = TRUE),
    c(tree_sha = "character",
      tree_url = "character",
      path     = "character",
      type     = "character",
      sha      = "character",
      size     = "integer",
      url      = "character"))

  expect_identical(recursive_tree$path, c("README.md", "test-dir", "test-file.txt"))
  expect_identical(recursive_tree$type, c("blob", "tree", "blob"))
})

# TEST: trees_exist ---------------------------------------------------------------------------

test_that("trees_exist returns TRUE or FALSE depending on whether the tree exists in the repo", {
  expect_true(suppressWarnings(trees_exist("3f5c0749c85cc4a3cbd240762b61276ad2bfbba2", "ChadGoymer/test-githapi")))
  expect_false(suppressWarnings(trees_exist("0000000000000000000000000000000000000000", "ChadGoymer/test-githapi")))

  expect_identical(
    suppressWarnings(trees_exist(c("3f5c0749c85cc4a3cbd240762b61276ad2bfbba2", "0000000000000000000000000000000000000000"), "ChadGoymer/test-githapi")),
    c(`3f5c0749c85cc4a3cbd240762b61276ad2bfbba2` = TRUE, `0000000000000000000000000000000000000000` = FALSE))
})
