context("organizations api")


# TEST: view_organizations --------------------------------------------------------------------

test_that("view_organizations returns a tibble summarising the organizations", {

  user_organizations <- view_organizations(user = "ChadGoymer")

  expect_is(user_organizations, "tbl")
  expect_identical(attr(user_organizations, "status"), 200L)
  expect_identical(
    map_chr(user_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

  expect_true("HairyCoos" %in% user_organizations$login)

  auth_organizations <- view_organizations(user = NULL)

  expect_is(auth_organizations, "tbl")
  expect_identical(attr(auth_organizations, "status"), 200L)
  expect_identical(
    map_chr(auth_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

  expect_true("HairyCoos" %in% auth_organizations$login)

  all_organizations <- view_organizations(n_max = 10)

  expect_is(all_organizations, "tbl")
  expect_identical(attr(all_organizations, "status"), 200L)
  expect_identical(
    map_chr(all_organizations, ~ class(.)[[1]]),
    c(id          = "integer",
      login       = "character",
      description = "character"))

})
