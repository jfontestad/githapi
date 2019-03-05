context("repositories releases")

# TEST: view_releases, create_releases & delete_releases --------------------------------------

test_that("create_releases creates some releases, view_releases retreives them and delete_releases deletes them", {
  all_releases <- view_releases("ChadGoymer/test-githapi")

  expect_is(all_releases, "tbl")
  expect_identical(
    map(all_releases, function(field) class(field)[[1]], simplify = TRUE),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))

  expect_true("0.0.0" %in% all_releases$tag_name)
  expect_identical(
    filter(all_releases, name == "Initial release") %>% pull(target_commitish),
    "cbd94cf24a4c62761b3ae59ca3c69f868591cf7d")

  latest_release <- view_releases("latest", repo = "ChadGoymer/test-githapi")

  expect_is(latest_release, "tbl")
  expect_identical(
    map(latest_release, function(field) class(field)[[1]], simplify = TRUE),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))

  latest_full_release <- filter(all_releases, draft == FALSE, prerelease == FALSE) %>% head(1)
  expect_identical(latest_release, latest_full_release)

  created_releases <- create_releases(
    tags    = c("aaa", "bbb"),
    commits = c("310c21d3f1601a46e014e68e94814b23406bf574", "32d3c5c4f6aba7ae9679480407e1b9f94ad04843"),
    names   = c("AAA", "BBB"),
    bodies  = c("Created for testing: aaa", "Created for testing: bbb"),
    repo    = "ChadGoymer/test-githapi")

  expect_is(created_releases, "tbl")
  expect_identical(
    map(created_releases, function(field) class(field)[[1]], simplify = TRUE),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))

  expect_identical(created_releases$tag_name, c("aaa", "bbb"))
  expect_identical(
    created_releases$target_commitish,
    c("310c21d3f1601a46e014e68e94814b23406bf574", "32d3c5c4f6aba7ae9679480407e1b9f94ad04843"))

  viewed_releases <- view_releases(c("aaa", "bbb"), "ChadGoymer/test-githapi")

  expect_is(viewed_releases, "tbl")
  expect_identical(
    map(viewed_releases, function(field) class(field)[[1]], simplify = TRUE),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))

  expect_identical(viewed_releases$name, c("AAA", "BBB"))
  expect_identical(
    viewed_releases$body,
    c("Created for testing: aaa", "Created for testing: bbb"))

  updated_releases <- update_releases(
    tags    = c("aaa", "bbb"),
    names   = c("AAA updated", "BBB updated"),
    bodies  = c("Updated for testing: aaa", "Updated for testing: bbb"),
    repo    = "ChadGoymer/test-githapi")

  expect_is(updated_releases, "tbl")
  expect_identical(
    map(updated_releases, function(field) class(field)[[1]], simplify = TRUE),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))

  expect_identical(updated_releases$name, c("AAA updated", "BBB updated"))
  expect_identical(
    updated_releases$body,
    c("Updated for testing: aaa", "Updated for testing: bbb"))

  delete_results <- delete_releases(c("aaa", "bbb"), "ChadGoymer/test-githapi")

  expect_identical(delete_results, list(aaa = TRUE, bbb = TRUE))
  expect_error(suppressWarnings(view_releases("aaa", "ChadGoymer/test-githapi")), "Not Found")
  expect_error(suppressWarnings(view_releases("bbb", "ChadGoymer/test-githapi")), "Not Found")
})
