context("releases")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  test_repo <- create_repository(
    name        = str_c("test-releases-", now),
    description = "This is a repository to test releases",
    auto_init   = TRUE)

  create_branch(
    name = str_c("test-releases-", now),
    ref  = "master",
    repo = str_c("ChadGoymer/test-releases-", now))

  create_file(
    content = "This is a commit to test releases",
    path    = str_c("test-releases-", now, ".txt"),
    branch  = str_c("test-releases-", now),
    message = "Commit to test releases",
    repo    = str_c("ChadGoymer/test-releases-", now))

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-releases-", now))

}))


# TEST: create_release ----------------------------------------------------------------------------

test_that("create_release creates a release and returns a list of the properties", {

  master_release <- create_release(
    tag  = str_c("master-release-", now),
    repo = str_c("ChadGoymer/test-releases-", now),
    name = str_c("Master release ", now),
    body = "This is a release created by create_release()")

  expect_is(master_release, "list")
  expect_identical(attr(master_release, "status"), 201L)
  expect_identical(
    map_chr(master_release, ~ class(.)[[1]]),
    c(id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"))

  expect_identical(master_release$tag, str_c("master-release-", now))
  expect_identical(master_release$name, str_c("Master release ", now))
  expect_identical(master_release$body, "This is a release created by create_release()")
  expect_identical(master_release$commit, "master")
  expect_false(master_release$draft)
  expect_false(master_release$prerelease)
  expect_identical(master_release$author_login, "ChadGoymer")


  branch_release <- create_release(
    tag  = str_c("branch-release-", now),
    repo = str_c("ChadGoymer/test-releases-", now),
    name = str_c("Branch release ", now),
    body = "This is a release created by create_release()",
    ref  = str_c("test-releases-", now))

  expect_is(branch_release, "list")
  expect_identical(attr(branch_release, "status"), 201L)
  expect_identical(
    map_chr(branch_release, ~ class(.)[[1]]),
    c(id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"))

  expect_identical(branch_release$tag, str_c("branch-release-", now))
  expect_identical(branch_release$name, str_c("Branch release ", now))
  expect_identical(branch_release$body, "This is a release created by create_release()")
  expect_identical(branch_release$commit, str_c("test-releases-", now))
  expect_false(branch_release$draft)
  expect_false(branch_release$prerelease)
  expect_identical(branch_release$author_login, "ChadGoymer")


  draft_release <- create_release(
    tag        = str_c("draft-release-", now),
    repo       = str_c("ChadGoymer/test-releases-", now),
    name       = str_c("Draft release ", now),
    body       = "This is a release created by create_release()",
    draft      = TRUE,
    prerelease = TRUE)

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 201L)
  expect_identical(
    map_chr(draft_release, ~ class(.)[[1]]),
    c(id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "character",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"))

  expect_identical(draft_release$tag, str_c("draft-release-", now))
  expect_identical(draft_release$name, str_c("Draft release ", now))
  expect_identical(draft_release$body, "This is a release created by create_release()")
  expect_identical(draft_release$commit, "master")
  expect_true(draft_release$draft)
  expect_true(draft_release$prerelease)
  expect_identical(draft_release$author_login, "ChadGoymer")

})
