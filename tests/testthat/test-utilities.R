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

  summer <- as.character(as.datetime("2019-08-01T10:00:00Z"))
  local_summer <- as.POSIXct("2019-08-01 10:00:00", tz = "UTC") %>% format(tz = "")

  expect_identical(summer, local_summer)

  winter <- as.character(as.datetime("2019-11-01T10:00:00Z"))
  local_winter <- as.POSIXct("2019-11-01 10:00:00", tz = "UTC") %>% format(tz = "")

  expect_identical(winter, local_winter)

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
    id                      = 3534197,
    number                  = 1,
    name                    = "Work in Progress",
    body                    = "All my work in progress",
    state                   = "open",
    organization_permission = NA,
    private                 = NA,
    creator                 = list(login = "ChadGoymer"),
    created_at              = "2019-11-16T07:20:09Z",
    updated_at              = "2019-12-10T19:07:53Z",
    html_url                = "https://github.com/users/ChadGoymer/projects/1")

  properties <- list(
    id         = c("id",                      as = "integer"),
    number     = c("number",                  as = "integer"),
    name       = c("name",                    as = "character"),
    body       = c("body",                    as = "character"),
    state      = c("state",                   as = "character"),
    permission = c("organization_permission", as = "character"),
    private    = c("private",                 as = "logical"),
    creator    = c("creator", "login",        as = "character"),
    created_at = c("created_at",              as = "datetime"),
    updated_at = c("updated_at",              as = "datetime"),
    html_url   = c("html_url",                as = "character"))

  selected_properties <- select_properties(entity, properties)

  expected_result <- list(
    id         = as.integer(3534197),
    number     = as.integer(1),
    name       = "Work in Progress",
    body       = "All my work in progress",
    state      = "open",
    permission = as.character(NA),
    private    = NA,
    creator    = "ChadGoymer",
    created_at = as.POSIXct("2019-11-16 07:20:09", tz = "UTC") %>% format(tz = "") %>% as.POSIXct(),
    updated_at = as.POSIXct("2019-12-10 19:07:53", tz = "UTC") %>% format(tz = "") %>% as.POSIXct(),
    html_url   = "https://github.com/users/ChadGoymer/projects/1")

  expect_identical(selected_properties, expected_result)

})

test_that("select_properties returns a list of empty vectors for an empty entity", {

  properties <- list(
    id         = c("id",                      as = "integer"),
    number     = c("number",                  as = "integer"),
    name       = c("name",                    as = "character"),
    body       = c("body",                    as = "character"),
    state      = c("state",                   as = "character"),
    permission = c("organization_permission", as = "character"),
    private    = c("private",                 as = "logical"),
    creator    = c("creator", "login",        as = "character"),
    created_at = c("created_at",              as = "datetime"),
    updated_at = c("updated_at",              as = "datetime"),
    html_url   = c("html_url",                as = "character"))

  empty_result <- select_properties(list(), properties)

  expected_empty_result <- list(
    id         = integer(),
    number     = integer(),
    name       = character(),
    body       = character(),
    state      = character(),
    permission = character(),
    private    = logical(),
    creator    = character(),
    created_at = as.POSIXct(character()),
    updated_at = as.POSIXct(character()),
    html_url   = character())

  expect_identical(empty_result, expected_empty_result)

})


# TEST: bind_properties -----------------------------------------------------------------------

test_that("bind_properties extracts properties from entities and binds them into a data.frame", {

  entities <- list(
    list(
      id                      = 3534197,
      number                  = 1,
      name                    = "Work in Progress",
      body                    = "All my work in progress",
      state                   = "open",
      organization_permission = NA,
      private                 = NA,
      creator                 = list(login = "ChadGoymer"),
      created_at              = "2019-11-16T07:20:09Z",
      updated_at              = "2019-12-10T19:07:53Z",
      html_url                = "https://github.com/users/ChadGoymer/projects/1"),
    list(
      id                      = 747228,
      number                  = 1,
      name                    = "Prioritisation",
      body                    = "All tasks in priority order",
      state                   = "open",
      organization_permission = NA,
      private                 = NA,
      creator                 = list(login = "ChadGoymer"),
      created_at              = "2017-07-11T11:24:23Z",
      updated_at              = "2019-12-07T12:53:24Z",
      html_url                = "https://github.com/ChadGoymer/githapi/projects/1")
  )

  properties <- list(
    id         = c("id",                      as = "integer"),
    number     = c("number",                  as = "integer"),
    name       = c("name",                    as = "character"),
    body       = c("body",                    as = "character"),
    state      = c("state",                   as = "character"),
    permission = c("organization_permission", as = "character"),
    private    = c("private",                 as = "logical"),
    creator    = c("creator", "login",        as = "character"),
    created_at = c("created_at",              as = "datetime"),
    updated_at = c("updated_at",              as = "datetime"),
    html_url   = c("html_url",                as = "character"))

  result <- bind_properties(entities, properties)

  expected_result <- tibble(
    id         = as.integer(c(3534197, 747228)),
    number     = as.integer(c(1, 1)),
    name       = c("Work in Progress", "Prioritisation"),
    body       = c("All my work in progress", "All tasks in priority order"),
    state      = c("open", "open"),
    permission = as.character(c(NA, NA)),
    private    = c(NA, NA),
    creator    = c("ChadGoymer", "ChadGoymer"),
    created_at = as.POSIXct(c("2019-11-16 07:20:09", "2017-07-11 11:24:23"), tz = "UTC") %>% format(tz = "") %>% as.POSIXct(),
    updated_at = as.POSIXct(c("2019-12-10 19:07:53", "2019-12-07 12:53:24"), tz = "UTC") %>% format(tz = "") %>% as.POSIXct(),
    html_url   = c("https://github.com/users/ChadGoymer/projects/1", "https://github.com/ChadGoymer/githapi/projects/1"))

  expect_identical(result, expected_result)

})

test_that("bind_properties returns an empty data.frame for an empty list of entities", {

  properties <- list(
    id         = c("id",                      as = "integer"),
    number     = c("number",                  as = "integer"),
    name       = c("name",                    as = "character"),
    body       = c("body",                    as = "character"),
    state      = c("state",                   as = "character"),
    permission = c("organization_permission", as = "character"),
    private    = c("private",                 as = "logical"),
    creator    = c("creator", "login",        as = "character"),
    created_at = c("created_at",              as = "datetime"),
    updated_at = c("updated_at",              as = "datetime"),
    html_url   = c("html_url",                as = "character"))

  empty_result <- bind_properties(list(), properties)

  expected_empty_result <- tibble(
    id         = integer(),
    number     = integer(),
    name       = character(),
    body       = character(),
    state      = character(),
    permission = character(),
    private    = logical(),
    creator    = character(),
    created_at = as.POSIXct(character()),
    updated_at = as.POSIXct(character()),
    html_url   = character())

  expect_identical(empty_result, expected_empty_result)

})
