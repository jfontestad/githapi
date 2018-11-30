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
})
