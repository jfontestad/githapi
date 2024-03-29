context("statuses")


# SETUP ------------------------------------------------------------------------

suffix <- sample(letters, 10, replace = TRUE) %>% str_c(collapse = "")

setup(suppressMessages({

  create_repository(
    name        = str_c("test-statuses-", suffix),
    description = "This is a repository to test statuses",
    auto_init   = TRUE
  )

  Sys.sleep(1)

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-statuses-", suffix))

}))


# TEST: create_status ----------------------------------------------------------

test_that("create_status creates a status and returns the properties", {

  pending_status <- create_status(
    state       = "pending",
    ref         = "main",
    repo        = str_c("ChadGoymer/test-statuses-", suffix),
    description = "This is a pending status",
    target_url  = "https://goymer.me/githapi",
    context     = "test/pending"
  )

  expect_is(pending_status, "list")
  expect_identical(attr(pending_status, "status"), 201L)
  expect_identical(
    map_chr(pending_status, ~ class(.)[[1]]),
    c(
      id          = "character",
      state       = "character",
      description = "character",
      target_url  = "character",
      context     = "character",
      creator     = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(pending_status$state, "pending")
  expect_identical(pending_status$description, "This is a pending status")
  expect_identical(pending_status$target_url, "https://goymer.me/githapi")
  expect_identical(pending_status$context, "test/pending")
  expect_identical(pending_status$creator, "ChadGoymer")


  success_status <- create_status(
    state       = "success",
    ref         = "main",
    repo        = str_c("ChadGoymer/test-statuses-", suffix),
    description = "This is a success status",
    target_url  = "https://goymer.me/githapi",
    context     = "test/success"
  )

  expect_is(success_status, "list")
  expect_identical(attr(success_status, "status"), 201L)
  expect_identical(
    map_chr(success_status, ~ class(.)[[1]]),
    c(
      id          = "character",
      state       = "character",
      description = "character",
      target_url  = "character",
      context     = "character",
      creator     = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(success_status$state, "success")
  expect_identical(success_status$description, "This is a success status")
  expect_identical(success_status$target_url, "https://goymer.me/githapi")
  expect_identical(success_status$context, "test/success")
  expect_identical(success_status$creator, "ChadGoymer")

})


# TEST: view_statuses ----------------------------------------------------------

test_that("view_statuses returns a tibble of status properties", {

  statuses <- view_statuses(
    ref  = "main",
    repo = str_c("ChadGoymer/test-statuses-", suffix)
  )

  expect_is(statuses, "tbl")
  expect_identical(attr(statuses, "status"), 200L)
  expect_identical(
    map_chr(statuses, ~ class(.)[[1]]),
    c(
      id          = "character",
      state       = "character",
      description = "character",
      target_url  = "character",
      context     = "character",
      creator     = "character",
      created_at  = "POSIXct",
      updated_at  = "POSIXct"
    )
  )

  expect_identical(statuses$state, c("success", "pending"))
  expect_identical(
    statuses$description,
    c("This is a success status", "This is a pending status")
  )
  expect_identical(statuses$target_url, rep("https://goymer.me/githapi", 2))
  expect_identical(statuses$context, c("test/success", "test/pending"))
  expect_identical(statuses$creator, rep("ChadGoymer", 2))

})


# TEST: view_status ------------------------------------------------------------

test_that("view_status returns the combined status", {

  status <- view_status(
    ref  = "main",
    repo = str_c("ChadGoymer/test-statuses-", suffix)
  )

  expect_is(status, "character")
  expect_identical(attr(status, "status"), 200L)
  expect_identical(as.character(status), "pending")

})
