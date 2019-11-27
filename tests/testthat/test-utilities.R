context("utilities")


# TEST: is_sha --------------------------------------------------------------------------------

test_that("is_sha returns TRUE with a valid SHA and FALSE otherwise", {

  expect_true(is_sha("ba6320999105647364736246ce9c83718b18ce98"))

  expect_false(is_sha(NA))
  expect_false(is_sha(NULL))
  expect_false(is_sha(1))
  expect_false(is_sha("bob"))
  expect_false(is_sha("pq8knu9l4k9whpqin601y35o6miq05l9svihshuy"))

})


# TEST: is_repo -------------------------------------------------------------------------------

test_that("is_repo returns TRUE with a valid repo and FALSE otherwise", {

  expect_true(is_repo("project/name"))

  expect_false(is_repo(NA))
  expect_false(is_repo(NULL))
  expect_false(is_repo(1))
  expect_false(is_repo("bob"))
  expect_false(is_repo("github/project/name"))

})
