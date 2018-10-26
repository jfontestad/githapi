context("utilities")

test_that("select_fields returns the specified fields with correct names", {
  data <- list(
    list(name = "A", is_ok = TRUE, date = "2018-06-21 08:56:23", value = 23, author = list(
      name = "Bob Wilson", email = "bob.wilson@acme.com")),
    list(name = "B", is_ok = FALSE, date = "2018-06-22 23:12:32", value = 100.45, author = list(
      name = "Jane Smith")),
    list(name = "C", is_ok = FALSE))

  complete_data <- select_fields(data[[1]], fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(complete_data, list(
    name = "A",
    date = "2018-06-21 08:56:23",
    value = 23,
    author_name = "Bob Wilson",
    author_email = "bob.wilson@acme.com"))

  missing_nested_data <- select_fields(data[[2]], fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(missing_nested_data, list(
    name = "B",
    date = "2018-06-22 23:12:32",
    value = 100.45,
    author_name = "Jane Smith",
    author_email = NA))

  missing_data <- select_fields(data[[3]], fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(missing_data, list(
    name = "C",
    date = NA,
    value = NA,
    author_name = NA,
    author_email = NA))

  renamed_data <- select_fields(data[[1]], fields = list(
    "name", "date", "value", author = c("author", "name"), email = c("author", "email")))
  expect_identical(renamed_data, list(
    name = "A",
    date = "2018-06-21 08:56:23",
    value = 23,
    author = "Bob Wilson",
    email = "bob.wilson@acme.com"))

})

test_that("bind_rows returns the specified columns with correct names and types", {
  data <- list(
    list(name = "A", is_ok = TRUE, date = "2018-06-21 08:56:23", value = 23, author = list(
      name = "Bob Wilson", email = "bob.wilson@acme.com")),
    list(name = "B", is_ok = FALSE, date = "2018-06-22 23:12:32", value = 100.45, author = list(
      name = "Jane Smith")),
    list(name = "C", is_ok = FALSE))

  basic_data <- bind_fields(data, fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(basic_data, tibble(
    name = c("A", "B", "C"),
    date = c("2018-06-21 08:56:23", "2018-06-22 23:12:32", NA),
    value = c(23, 100.45, NA),
    author_name = c("Bob Wilson", "Jane Smith", NA),
    author_email = c("bob.wilson@acme.com", NA, NA)))

  renamed_data <- bind_fields(data, fields = list(
    "name", "date", "value", author = c("author", "name"), email = c("author", "email")))
  expect_identical(renamed_data, tibble(
    name = c("A", "B", "C"),
    date = c("2018-06-21 08:56:23", "2018-06-22 23:12:32", NA),
    value = c(23, 100.45, NA),
    author = c("Bob Wilson", "Jane Smith", NA),
    email = c("bob.wilson@acme.com", NA, NA)))

  converted_data <- bind_fields(data, fields = list(
    "name",
    date = c("date", as = "datetime"),
    value = c("value", as = "character"),
    author = c("author", "name"),
    email = c("author", "email")))
  expect_identical(converted_data, tibble(
    name = c("A", "B", "C"),
    date = as.POSIXct(c("2018-06-21 08:56:23", "2018-06-22 23:12:32", NA)),
    value = as.character(c(23, 100.45, NA)),
    author = c("Bob Wilson", "Jane Smith", NA),
    email = c("bob.wilson@acme.com", NA, NA)))

  empty_data <- bind_fields(list(), fields = list(
    "name",
    date = c("date", as = "datetime"),
    value = c("value", as = "character"),
    author = c("author", "name"),
    email = c("author", "email")))
  expect_identical(empty_data, tibble(
    name = logical(),
    date = as.POSIXct(logical()),
    value = character(),
    author = logical(),
    email = logical()))
})


# TEST: remove_missing ------------------------------------------------------------------------

test_that("remove_missing removes all empty elements from a list", {
  x <- list(1, a = NULL, b = list(), c = NA, d = 42)
  expect_identical(remove_missing(x), list(1, d = 42))
})
