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

  new_tags <- format(Sys.time(), "%Y-%m-%d-%H-%M-%S-") %>% str_c(1:2)
  created_tags <- suppressWarnings(create_tags(
    tags = new_tags,
    shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
    repo = "ChadGoymer/test-githapi"))

  expect_is(created_tags, "tbl")
  expect_identical(
    gh_map(created_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(created_tags$ref, str_c("refs/tags/", new_tags))
  expect_identical(
    created_tags$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  viewed_tags <- suppressWarnings(view_tags(new_tags, "ChadGoymer/test-githapi"))

  expect_is(viewed_tags, "tbl")
  expect_identical(
    gh_map(viewed_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(viewed_tags$name, new_tags)
  expect_identical(
    viewed_tags$object_sha,
    c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"))

  updated_tags <- suppressWarnings(update_tags(
    tags = new_tags,
    shas = c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"),
    repo = "ChadGoymer/test-githapi"))

  expect_is(updated_tags, "tbl")
  expect_identical(
    gh_map(updated_tags, function(field) class(field)[[1]], simplify = TRUE),
    c(name        = "character",
      ref         = "character",
      url         = "character",
      object_sha  = "character",
      object_type = "character",
      object_url  = "character"))

  expect_identical(updated_tags$ref, str_c("refs/tags/", new_tags))
  expect_identical(
    updated_tags$object_sha,
    c("32d3c5c4f6aba7ae9679480407e1b9f94ad04843", "68f01be0dad53f366337c9d87fad939b2a2853c8"))

  delete_results <- suppressWarnings(delete_tags(new_tags, "ChadGoymer/test-githapi"))

  expect_identical(delete_results, list(TRUE, TRUE) %>% set_names(new_tags))
  expect_error(suppressWarnings(view_tags(new_tags[[1]], "ChadGoymer/test-githapi")), "Not Found")
  expect_error(suppressWarnings(view_tags(new_tags[[2]], "ChadGoymer/test-githapi")), "Not Found")

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

  expect_true(suppressWarnings(tags_exist("0.0.0", "ChadGoymer/test-githapi")))
  expect_false(suppressWarnings(tags_exist("no-such-tag", "ChadGoymer/test-githapi")))

  expect_identical(
    suppressWarnings(tags_exist(c("0.0.0", "no-such-tag"), "ChadGoymer/test-githapi")),
    c(`0.0.0` = TRUE, `no-such-tag` = FALSE))

})
