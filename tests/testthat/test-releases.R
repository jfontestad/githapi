context("releases")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-releases-", now),
    description = "This is a repository to test releases",
    auto_init   = TRUE)

  create_file(
    content = "This is a commit to test releases",
    path    = str_c("test-releases-", now, ".txt"),
    branch  = str_c("test-releases-", now),
    message = "Commit to test releases",
    repo    = str_c("ChadGoymer/test-releases-", now),
    parent  = "master")

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


# TEST: update_release ----------------------------------------------------------------------------

draft_release_id <- suppressMessages({
  .view_releases(str_c("ChadGoymer/test-releases-", now)) %>%
    filter(.data$name == str_c("Draft release ", now)) %>%
    pull(id)
})

test_that("update_release updates a release and returns a list of the properties", {

  master_release <- update_release(
    release = str_c("master-release-", now),
    repo    = str_c("ChadGoymer/test-releases-", now),
    tag     = str_c("updated-master-release-", now),
    name    = str_c("Updated master release ", now),
    body    = "This release has been updated by update_release()")

  expect_is(master_release, "list")
  expect_identical(attr(master_release, "status"), 200L)
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

  expect_identical(master_release$tag, str_c("updated-master-release-", now))
  expect_identical(master_release$name, str_c("Updated master release ", now))
  expect_identical(master_release$body, "This release has been updated by update_release()")
  expect_identical(master_release$commit, "master")
  expect_false(master_release$draft)
  expect_false(master_release$prerelease)
  expect_identical(master_release$author_login, "ChadGoymer")


  branch_release <- update_release(
    release = str_c("branch-release-", now),
    repo    = str_c("ChadGoymer/test-releases-", now),
    tag     = str_c("updated-branch-release-", now),
    name    = str_c("Updated branch release ", now),
    body    = "This release has been updated by update_release()",
    ref     = str_c("test-releases-", now))

  expect_is(branch_release, "list")
  expect_identical(attr(branch_release, "status"), 200L)
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

  expect_identical(branch_release$tag, str_c("updated-branch-release-", now))
  expect_identical(branch_release$name, str_c("Updated branch release ", now))
  expect_identical(branch_release$body, "This release has been updated by update_release()")
  expect_identical(branch_release$commit, str_c("test-releases-", now))
  expect_false(branch_release$draft)
  expect_false(branch_release$prerelease)
  expect_identical(branch_release$author_login, "ChadGoymer")


  draft_release <- update_release(
    release    = draft_release_id,
    tag        = str_c("updated-draft-release-", now),
    repo       = str_c("ChadGoymer/test-releases-", now),
    name       = str_c("Updated draft release ", now),
    body       = "This release has been updated by update_release()",
    draft      = FALSE,
    prerelease = FALSE)

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 200L)
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

  expect_identical(draft_release$tag, str_c("updated-draft-release-", now))
  expect_identical(draft_release$name, str_c("Updated draft release ", now))
  expect_identical(draft_release$body, "This release has been updated by update_release()")
  expect_identical(draft_release$commit, "master")
  expect_false(draft_release$draft)
  expect_false(draft_release$prerelease)
  expect_identical(draft_release$author_login, "ChadGoymer")

})


# TEST: .view_releases -----------------------------------------------------------------------------

test_that(".view_releases returns a tibble of release properties", {

  all_releases <- .view_releases(str_c("ChadGoymer/test-releases-", now))

  expect_is(all_releases, "tbl")
  expect_identical(attr(all_releases, "status"), 200L)
  expect_identical(
    map_chr(all_releases, ~ class(.)[[1]]),
    c(id           = "integer",
      tag          = "character",
      name         = "character",
      body         = "character",
      commit       = "character",
      draft        = "logical",
      prerelease   = "logical",
      author_login = "character",
      assets       = "list",
      html_url     = "character",
      created_at   = "POSIXct",
      published_at = "POSIXct"))

  expect_true(str_c("updated-master-release-", now) %in% all_releases$tag)
  expect_true(str_c("updated-branch-release-", now) %in% all_releases$tag)
  expect_true(str_c("updated-draft-release-", now) %in% all_releases$tag)

})


# TEST: view_release ------------------------------------------------------------------------------

test_that("view_release returns a list of release properties", {

  master_release <- view_release(
    release = str_c("updated-master-release-", now),
    repo    = str_c("ChadGoymer/test-releases-", now))

  expect_is(master_release, "list")
  expect_identical(attr(master_release, "status"), 200L)
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

  expect_identical(master_release$tag, str_c("updated-master-release-", now))
  expect_identical(master_release$name, str_c("Updated master release ", now))
  expect_identical(master_release$body, "This release has been updated by update_release()")
  expect_identical(master_release$commit, "master")
  expect_false(master_release$draft)
  expect_false(master_release$prerelease)
  expect_identical(master_release$author_login, "ChadGoymer")


  draft_release <- view_release(
    release = draft_release_id,
    repo    = str_c("ChadGoymer/test-releases-", now))

  expect_is(draft_release, "list")
  expect_identical(attr(draft_release, "status"), 200L)
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

  expect_identical(draft_release$tag, str_c("updated-draft-release-", now))
  expect_identical(draft_release$name, str_c("Updated draft release ", now))
  expect_identical(draft_release$body, "This release has been updated by update_release()")
  expect_identical(draft_release$commit, "master")
  expect_false(draft_release$draft)
  expect_false(draft_release$prerelease)
  expect_identical(draft_release$author_login, "ChadGoymer")

})

test_that("view_release throws as error if invalid arguments are supplied", {

  expect_error(
    view_release(
      release = list(1),
      repo    = str_c("ChadGoymer/test-releases-", now)),
    "'release' must be either an integer or a valid git reference")

})


# TEST: browse_release ------------------------------------------------------------------------

test_that("browse_release opens the release's page in the browser", {

  skip_if(!interactive(), "browse_release must be tested manually")

  master_release <- browse_release(
    release = str_c("updated-master-release-", now),
    repo    = str_c("ChadGoymer/test-releases-", now))

  expect_is(master_release, "character")
  expect_identical(attr(master_release, "status"), 200L)
  expect_identical(
    dirname(master_release),
    str_c("https://github.com/ChadGoymer/test-releases-", now, "/releases/tag"))


  draft_release <- browse_release(
    release = draft_release_id,
    repo    = str_c("ChadGoymer/test-releases-", now))

  expect_is(draft_release, "character")
  expect_identical(attr(draft_release, "status"), 200L)
  expect_identical(
    dirname(draft_release),
    str_c("https://github.com/ChadGoymer/test-releases-", now, "/releases/tag"))

})

test_that("browse_release throws as error if invalid arguments are supplied", {

  expect_error(
    browse_release(
      release = list(1),
      repo    = str_c("ChadGoymer/test-releases-", now)),
    "'release' must be either an integer or a valid git reference")

})


# TEST: delete_release ------------------------------------------------------------------------

test_that("delete_release deletes a release", {

  deleted_release <- delete_release(
    release = str_c("updated-master-release-", now),
    repo    = str_c("ChadGoymer/test-releases-", now))

  expect_is(deleted_release, "logical")
  expect_identical(attr(deleted_release, "status"), 204L)
  expect_identical(as.logical(deleted_release), TRUE)

})
