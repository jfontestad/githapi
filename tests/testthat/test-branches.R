context("branches")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_repo <- create_repository(
    name        = str_c("test-branches-", now),
    description = "This is a repository to test branches",
    auto_init   = TRUE)

  # TODO: replace with new `create_file()` function
  suppressWarnings({
    create_files(
      paths    = str_c("test-branches-", now, ".txt"),
      contents = "This is a commit to test branches",
      messages = "Commit to test branches",
      branches = str_c("test-branches-1-", now),
      parents  = "master",
      repo     = str_c("ChadGoymer/test-branches-", now))
  })

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-branches-", now))

})))


# TEST: create_branch -------------------------------------------------------------------------

test_that("create_branch creates a branch and returns a list of the properties", {

  master_sha <- gh_url("repos", str_c("ChadGoymer/test-branches-", now), "commits/heads/master") %>%
    gh_request("GET", accept = "application/vnd.github.VERSION.sha")

  new_branch <- create_branch(
    name = str_c("test-branches-2-", now),
    repo = str_c("ChadGoymer/test-branches-", now),
    ref  = "master")

  expect_is(new_branch, "list")
  expect_identical(attr(new_branch, "status"), 201L)
  expect_identical(
    map_chr(new_branch, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(new_branch$name, str_c("test-branches-2-", now))
  expect_identical(new_branch$sha, as.character(master_sha))

})


# TEST: update_branch -------------------------------------------------------------------------

test_that("update_branch updates a branch and returns a list of the properties", {

  update_sha <- gh_url("repos", str_c("ChadGoymer/test-branches-", now), "commits/heads", str_c("test-branches-1-", now)) %>%
    gh_request("GET", accept = "application/vnd.github.VERSION.sha")

  updated_branch <- update_branch(
    branch = str_c("test-branches-2-", now),
    repo   = str_c("ChadGoymer/test-branches-", now),
    ref    = str_c("test-branches-1-", now))

  expect_is(updated_branch, "list")
  expect_identical(attr(updated_branch, "status"), 200L)
  expect_identical(
    map_chr(updated_branch, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(updated_branch$name, str_c("test-branches-2-", now))
  expect_identical(updated_branch$sha, as.character(update_sha))

})


# TEST: view_branches -------------------------------------------------------------------------

# TODO: Uncomment in version 1.0
# test_that("view_branches returns a tibble of branch properties", {
#
#   all_branches <- view_branches(str_c("ChadGoymer/test-branches-", now))
#
#   expect_is(all_branches, "tbl")
#   expect_identical(attr(all_branches, "status"), 200L)
#   expect_identical(
#     map_chr(all_branches, ~ class(.)[[1]]),
#     c(name = "character",
#       ref  = "character",
#       sha  = "character"))
#
#   expect_true(all(str_c("test-branches-", 1:2, "-", now) %in% all_branches$name))
#
# })


# TEST: view_branch ---------------------------------------------------------------------------

test_that("view_branch returns a list of branch properties", {

  branch <- view_branch(
    branch = str_c("test-branches-1-", now),
    repo   = str_c("ChadGoymer/test-branches-", now))

  expect_is(branch, "list")
  expect_identical(attr(branch, "status"), 200L)
  expect_identical(
    map_chr(branch, ~ class(.)[[1]]),
    c(name = "character",
      ref  = "character",
      sha  = "character"))

  expect_identical(branch$name, str_c("test-branches-1-", now))

})


# TEST: delete_branch -------------------------------------------------------------------------

test_that("delete_branch deletes a branch", {

  deleted_branch <- delete_branch(
    branch = str_c("test-branches-1-", now),
    repo   = str_c("ChadGoymer/test-branches-", now))

  expect_is(deleted_branch, "logical")
  expect_identical(attr(deleted_branch, "status"), 204L)
  expect_identical(as.logical(deleted_branch), TRUE)

})
