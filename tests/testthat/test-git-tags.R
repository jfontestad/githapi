context("git tags")

# TEST: view_tags, create_tags & delete_tags --------------------------------------------------

test_that("create_tags creates some tags, view_tags retreives them and delete_tags deletes them", {

  all_tags <- view_tags("ChadGoymer/test-githapi")

  expect_is(all_tags, "tbl")
  expect_identical(
    gh_map(all_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_true("0.0.0" %in% all_tags$name)
  expect_identical(
    filter(all_tags, name == "0.0.0") %>% pull(object_sha),
    "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d")

  created_tags <- create_tags(
    tags = c("aaa", "bbb"),
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
    repo = "ChadGoymer/test-githapi")

  expect_is(created_tags, "tbl")
  expect_identical(
    gh_map(created_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(created_tags$ref, c("refs/tags/aaa", "refs/tags/bbb"))
  expect_identical(
    created_tags$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  viewed_tags <- view_tags(c("aaa", "bbb"), "ChadGoymer/test-githapi")

  expect_is(viewed_tags, "tbl")
  expect_identical(
    gh_map(viewed_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(viewed_tags$name, c("aaa", "bbb"))
  expect_identical(
    viewed_tags$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  updated_tags <- update_tags(
    tags = c("aaa", "bbb"),
    shas = c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"),
    repo = "ChadGoymer/test-githapi")

  expect_is(updated_tags, "tbl")
  expect_identical(
    gh_map(updated_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(updated_tags$ref, c("refs/tags/aaa", "refs/tags/bbb"))
  expect_identical(
    updated_tags$object_sha,
    c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"))

  delete_results <- delete_tags(c("aaa", "bbb"), "ChadGoymer/test-githapi")

  expect_identical(delete_results, list(aaa = TRUE, bbb = TRUE))
  expect_error(suppressWarnings(view_tags("aaa", "ChadGoymer/test-githapi")), "Not Found")
  expect_error(suppressWarnings(view_tags("bbb", "ChadGoymer/test-githapi")), "Not Found")

})

test_that("veiwing tags that do not exist throws an appropriate error", {

  no_repo_error_msg <- tryCatch(view_tags("ChadGoymer/no-repo"), error = function(e) e$message)

  expect_match(no_repo_error_msg, "In view_tags\\(\\): GitHub GET request failed")
  expect_match(no_repo_error_msg, "\\[Status\\]  404")

  no_tag_error_msg <- tryCatch(
    suppressWarnings(view_tags("no-tag", "ChadGoymer/no-repo")),
    error = function(e) e$message)

  expect_match(no_tag_error_msg, "'no-tag': \\[[0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\] GitHub GET request failed")
  expect_match(no_tag_error_msg, "\\[Status\\]  404")

})

# TEST: tags_exist ----------------------------------------------------------------------------

test_that("tags_exist returns TRUE or FALSE depending on whether the tag exists in the repo", {

  expect_true(tags_exist("0.0.0", "ChadGoymer/test-githapi"))
  expect_false(tags_exist("no-such-tag", "ChadGoymer/test-githapi"))

  expect_identical(
    tags_exist(c("0.0.0", "no-such-tag"), "ChadGoymer/test-githapi"),
    c(`0.0.0` = TRUE, `no-such-tag` = FALSE))

})
