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



# TEST: as.datetime ---------------------------------------------------------------------------

test_that("as.datetime converts a datetime string from UTC to local timezone", {

  bst <- as.character(as.datetime("2019-08-01T10:00:00Z"))
  expect_identical(bst, "2019-08-01 11:00:00")

  gmt <- as.character(as.datetime("2019-11-01T10:00:00Z"))
  expect_identical(gmt, "2019-11-01 10:00:00")

})


# TEST: property_names ------------------------------------------------------------------------

test_that("property_names returns appropriate names", {

  expect_identical(property_names(list("single_name")), "single_name")

  expect_identical(property_names(list("first_name", "second_name")), c("first_name", "second_name"))
  expect_identical(property_names(list(c("first_name", "second_name"))), "first_name_second_name")

  expect_identical(property_names(list(name = "first_name", "second_name")), c("name", "second_name"))
  expect_identical(property_names(list(name = c("first_name", "second_name"))), "name")

})


# TEST: select_properties ---------------------------------------------------------------------

test_that("select_properties returns a list of converted properties", {

  entity <- list(
    version      = "182.0.1.9757",
    release_type = "Test",
    release_date = "2019-01-01T00:00:00Z",
    console_exe  = ".\\TycheConsole.exe",
    tester_exe   = ".\\TycheTester.exe",
    breaking     = FALSE,
    is_hidden    = FALSE,
    created_by   = list(first_name = "Bob", last_name = "Smith"),
    created_at   = "2019-01-05T07:54:41Z",
    updated_by   = list(first_name = "Jane"),
    updated_at   = "2019-02-01T07:13:41Z")

  properties <- list(
    version          = c("version",                  as = "factor"),
    release_type     = c("release_type",             as = "factor"),
    release_date     = c("release_date",             as = "datetime"),
    location         = c("location",                 as = "character"),
    console_exe      = c("console_exe",              as = "character"),
    tester_exe       = c("tester_exe",               as = "character"),
    breaking         = c("breaking",                 as = "logical"),
    is_hidden        = c("is_hidden",                as = "logical"),
    created_by_first = c("created_by", "first_name", as = "character"),
    created_by_last  = c("created_by", "last_name",  as = "character"),
    created_at       = c("created_at",               as = "datetime"),
    updated_by_first = c("updated_by", "first_name", as = "character"),
    updated_by_last  = c("updated_by", "last_name",  as = "character"),
    updated_at       = c("updated_at",               as = "datetime"))

  selected_properties <- select_properties(entity, properties)

  expected_result <- list(
    version          = as.factor("182.0.1.9757"),
    release_type     = as.factor("Test"),
    release_date     = as.POSIXct("2019-01-01 00:00:00"),
    location         = NA_character_,
    console_exe      = ".\\TycheConsole.exe",
    tester_exe       = ".\\TycheTester.exe",
    breaking         = FALSE,
    is_hidden        = FALSE,
    created_by_first = "Bob",
    created_by_last  = "Smith",
    created_at       = as.POSIXct("2019-01-05 07:54:41"),
    updated_by_first = "Jane",
    updated_by_last  = NA_character_,
    updated_at       = as.POSIXct("2019-02-01 07:13:41"))

  expect_identical(selected_properties, expected_result)

})

test_that("select_properties returns a list of empty vectors for an empty entity", {

  properties <- list(
    version          = c("version",                  as = "factor"),
    release_type     = c("release_type",             as = "factor"),
    release_date     = c("release_date",             as = "datetime"),
    location         = c("location",                 as = "character"),
    console_exe      = c("console_exe",              as = "character"),
    tester_exe       = c("tester_exe",               as = "character"),
    breaking         = c("breaking",                 as = "logical"),
    is_hidden        = c("is_hidden",                as = "logical"),
    created_by_first = c("created_by", "first_name", as = "character"),
    created_by_last  = c("created_by", "last_name",  as = "character"),
    created_at       = c("created_at",               as = "datetime"),
    updated_by_first = c("updated_by", "first_name", as = "character"),
    updated_by_last  = c("updated_by", "last_name",  as = "character"),
    updated_at       = c("updated_at",               as = "datetime"))

  empty_result <- select_properties(list(), properties)

  expected_empty_result <- list(
    version          = as.factor(character()),
    release_type     = as.factor(character()),
    release_date     = as.POSIXct(character()),
    location         = character(),
    console_exe      = character(),
    tester_exe       = character(),
    breaking         = logical(),
    is_hidden        = logical(),
    created_by_first = character(),
    created_by_last  = character(),
    created_at       = as.POSIXct(character()),
    updated_by_first = character(),
    updated_by_last  = character(),
    updated_at       = as.POSIXct(character()))

  expect_identical(empty_result, expected_empty_result)

})


# TEST: bind_properties -----------------------------------------------------------------------

test_that("bind_properties extracts properties from entities and binds them into a data.frame", {

  entities <- list(
    list(
      version      = "182.0.1.9757",
      release_type = "Test",
      release_date = "2019-01-01T00:00:00Z",
      console_exe  = ".\\TycheConsole.exe",
      tester_exe   = ".\\TycheTester.exe",
      breaking     = FALSE,
      is_hidden    = FALSE,
      created_by   = list(first_name = "Bob", last_name = "Smith"),
      created_at   = "2019-01-05T07:54:41Z",
      updated_by   = list(first_name = "Jane"),
      updated_at   = "2019-02-01T07:13:41Z"),
    list(
      version      = "182.0.2.4242",
      release_type = "Production",
      release_date = "2019-04-01T00:00:00Z",
      location     = "C:\\Tyche\\182.0.2.4242",
      console_exe  = ".\\TycheConsole.exe",
      tester_exe   = NULL,
      breaking     = TRUE,
      is_hidden    = FALSE,
      created_by   = list(first_name = "Jane", last_name = "Jones"),
      created_at   = "2019-04-05T07:54:41Z",
      updated_by   = NULL,
      updated_at   = "2019-05-01T07:13:41Z"))

  properties <- list(
    version          = c("version",                  as = "factor"),
    release_type     = c("release_type",             as = "factor"),
    release_date     = c("release_date",             as = "datetime"),
    location         = c("location",                 as = "character"),
    console_exe      = c("console_exe",              as = "character"),
    tester_exe       = c("tester_exe",               as = "character"),
    breaking         = c("breaking",                 as = "logical"),
    is_hidden        = c("is_hidden",                as = "logical"),
    created_by_first = c("created_by", "first_name", as = "character"),
    created_by_last  = c("created_by", "last_name",  as = "character"),
    created_at       = c("created_at",               as = "datetime"),
    updated_by_first = c("updated_by", "first_name", as = "character"),
    updated_by_last  = c("updated_by", "last_name",  as = "character"),
    updated_at       = c("updated_at",               as = "datetime"))

  result <- bind_properties(entities, properties)

  expected_result <- tibble(
    version          = as.factor(c("182.0.1.9757", "182.0.2.4242")),
    release_type     = as.factor(c("Test", "Production")),
    release_date     = as.POSIXct(c("2019-01-01 00:00:00", "2019-04-01 01:00:00")),
    location         = c(NA_character_, "C:\\Tyche\\182.0.2.4242"),
    console_exe      = c(".\\TycheConsole.exe", ".\\TycheConsole.exe"),
    tester_exe       = c(".\\TycheTester.exe", NA_character_),
    breaking         = c(FALSE, TRUE),
    is_hidden        = c(FALSE, FALSE),
    created_by_first = c("Bob", "Jane"),
    created_by_last  = c("Smith", "Jones"),
    created_at       = as.POSIXct(c("2019-01-05 07:54:41", "2019-04-05 08:54:41")),
    updated_by_first = c("Jane", NA_character_),
    updated_by_last  = c(NA_character_, NA_character_),
    updated_at       = as.POSIXct(c("2019-02-01 07:13:41", "2019-05-01 08:13:41")))

  expect_identical(result, expected_result)
})

test_that("bind_properties returns an empty data.frame for an empty list of entities", {

  properties <- list(
    version          = c("version",                  as = "factor"),
    release_type     = c("release_type",             as = "factor"),
    release_date     = c("release_date",             as = "datetime"),
    location         = c("location",                 as = "character"),
    console_exe      = c("console_exe",              as = "character"),
    tester_exe       = c("tester_exe",               as = "character"),
    breaking         = c("breaking",                 as = "logical"),
    is_hidden        = c("is_hidden",                as = "logical"),
    created_by_first = c("created_by", "first_name", as = "character"),
    created_by_last  = c("created_by", "last_name",  as = "character"),
    created_at       = c("created_at",               as = "datetime"),
    updated_by_first = c("updated_by", "first_name", as = "character"),
    updated_by_last  = c("updated_by", "last_name",  as = "character"),
    updated_at       = c("updated_at",               as = "datetime"))

  empty_result <- bind_properties(list(), properties)

  expected_empty_result <- tibble(
    version          = as.factor(character()),
    release_type     = as.factor(character()),
    release_date     = as.POSIXct(character()),
    location         = character(),
    console_exe      = character(),
    tester_exe       = character(),
    breaking         = logical(),
    is_hidden        = logical(),
    created_by_first = character(),
    created_by_last  = character(),
    created_at       = as.POSIXct(character()),
    updated_by_first = character(),
    updated_by_last  = character(),
    updated_at       = as.POSIXct(character()))

  expect_identical(empty_result, expected_empty_result)

})
