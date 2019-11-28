context("git branches")

# TEST: view_branches, create_branches & delete_branches --------------------------------------

test_that("create_branches creates some branches, view_branches retreives them and delete_branches deletes them", {
  all_branches <- view_branches("ChadGoymer/test-githapi")

  expect_is(all_branches, "tbl")
  expect_identical(
    gh_map(all_branches, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_true("test-branch" %in% all_branches$name)

  new_branches <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S-") %>% str_c(1:2)
  created_branches <- create_branches(
    branches = new_branches,
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
    repo = "ChadGoymer/test-githapi")

  expect_is(created_branches, "tbl")
  expect_identical(
    gh_map(created_branches, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(created_branches$ref, str_c("refs/heads/", new_branches))
  expect_identical(
    created_branches$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  viewed_branches <- view_branches(new_branches, "ChadGoymer/test-githapi")

  expect_is(viewed_branches, "tbl")
  expect_identical(
    gh_map(viewed_branches, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(viewed_branches$name, new_branches)
  expect_identical(
    viewed_branches$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  updated_branches <- update_branches(
    branches = new_branches,
    shas = c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"),
    repo = "ChadGoymer/test-githapi")

  expect_is(updated_branches, "tbl")
  expect_identical(
    gh_map(updated_branches, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(updated_branches$ref, str_c("refs/heads/", new_branches))
  expect_identical(
    updated_branches$object_sha,
    c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"))

  delete_results <- delete_branches(new_branches, "ChadGoymer/test-githapi")

  expect_identical(delete_results, list(TRUE, TRUE) %>% set_names(new_branches))
  expect_error(suppressWarnings(view_branches(new_branches[[1]], "ChadGoymer/test-githapi")), "Not Found")
  expect_error(suppressWarnings(view_branches(new_branches[[2]], "ChadGoymer/test-githapi")), "Not Found")
})

test_that("veiwing tags that do not exist throws an appropriate error", {

  no_repo_error_msg <- tryCatch(view_branches("ChadGoymer/no-repo"), error = function(e) e$message)

  expect_match(no_repo_error_msg, "In view_branches\\(\\): GitHub GET request failed")
  expect_match(no_repo_error_msg, "\\[Status\\]  404")

  no_branch_error_msg <- tryCatch(
    suppressWarnings(view_branches("no-branch", "ChadGoymer/test-githapi")),
    error = function(e) e$message)

  expect_match(no_branch_error_msg, "'no-branch': \\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] GitHub GET request failed")
  expect_match(no_branch_error_msg, "\\[Status\\]  404")

})

# TEST: branches_exist ------------------------------------------------------------------------

test_that("branches_exist returns TRUE or FALSE depending on whether the branch exists in the repo", {
  expect_true(branches_exist("master", "ChadGoymer/test-githapi"))
  expect_false(branches_exist("no-such-branch", "ChadGoymer/test-githapi"))

  expect_identical(
    branches_exist(c("master", "no-such-branch"), "ChadGoymer/test-githapi"),
    c(master = TRUE, `no-such-branch` = FALSE))
})
