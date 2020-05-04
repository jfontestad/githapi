context("tags")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-tags-", now),
    description = "This is a repository to test tags",
    auto_init   = TRUE)

  create_branch(
    name = str_c("test-tags-1-", now),
    ref  = "master",
    repo = str_c("ChadGoymer/test-tags-", now))

  create_file(
    content = "This is a commit to test tags",
    path    = str_c("test-tags-", now, ".txt"),
    branch  = str_c("test-tags-1-", now),
    message = "Commit to test tags",
    repo    = str_c("ChadGoymer/test-tags-", now))

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-tags-", now))

}))


# TEST: create_tag ----------------------------------------------------------------------------

test_that("create_tag creates a tag and returns a list of the properties", {

  branch_sha <- view_sha(
    ref  = str_c("test-tags-1-", now),
    repo = str_c("ChadGoymer/test-tags-", now))

  branch_tag <- create_tag(
    name = str_c("test-tags-1-", now),
    ref  = branch_sha,
    repo = str_c("ChadGoymer/test-tags-", now))

  expect_is(branch_tag, "list")
  expect_identical(attr(branch_tag, "status"), 201L)
  expect_identical(
    map_chr(branch_tag, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(branch_tag$name, str_c("test-tags-1-", now))
  expect_identical(branch_tag$sha, as.character(branch_sha))

  master_sha <- gh_url("repos", str_c("ChadGoymer/test-tags-", now), "commits/heads/master") %>%
    gh_request("GET", accept = "application/vnd.github.VERSION.sha")

  master_tag <- create_tag(
    name = str_c("test-tags-2-", now),
    ref  = "master",
    repo = str_c("ChadGoymer/test-tags-", now))

  expect_is(master_tag, "list")
  expect_identical(attr(master_tag, "status"), 201L)
  expect_identical(
    map_chr(master_tag, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(master_tag$name, str_c("test-tags-2-", now))
  expect_identical(master_tag$sha, as.character(master_sha))

})


# TEST: update_tag ----------------------------------------------------------------------------

test_that("update_tag updates a tag and returns a list of the properties", {

  update_sha <- gh_url("repos", str_c("ChadGoymer/test-tags-", now), "commits/heads", str_c("test-tags-1-", now)) %>%
    gh_request("GET", accept = "application/vnd.github.VERSION.sha")

  updated_tag <- update_tag(
    tag  = str_c("test-tags-2-", now),
    ref  = str_c("test-tags-1-", now),
    repo = str_c("ChadGoymer/test-tags-", now))

  expect_is(updated_tag, "list")
  expect_identical(attr(updated_tag, "status"), 200L)
  expect_identical(
    map_chr(updated_tag, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(updated_tag$name, str_c("test-tags-2-", now))
  expect_identical(updated_tag$sha, as.character(update_sha))

})


# TEST: .view_tags -----------------------------------------------------------------------------

test_that(".view_tags returns a tibble of tag properties", {

  all_tags <- .view_tags(str_c("ChadGoymer/test-tags-", now))

  expect_is(all_tags, "tbl")
  expect_identical(attr(all_tags, "status"), 200L)
  expect_identical(
    map_chr(all_tags, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_true(all(str_c("test-tags-", 1:2, "-", now) %in% all_tags$name))

})


# TEST: view_tag ------------------------------------------------------------------------------

test_that("view_tag returns a list of tag properties", {

  tag <- view_tag(
    tag  = str_c("test-tags-1-", now),
    repo = str_c("ChadGoymer/test-tags-", now))

  expect_is(tag, "list")
  expect_identical(attr(tag, "status"), 200L)
  expect_identical(
    map_chr(tag, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(tag$name, str_c("test-tags-1-", now))

})


# TEST: delete_tag ----------------------------------------------------------------------------

test_that("delete_tag deletes a tag", {

  deleted_tag <- delete_tag(
    tag  = str_c("test-tags-1-", now),
    repo = str_c("ChadGoymer/test-tags-", now))

  expect_is(deleted_tag, "logical")
  expect_identical(attr(deleted_tag, "status"), 204L)
  expect_identical(as.logical(deleted_tag), TRUE)

})
