context("utilities")

# TEST: map -----------------------------------------------------------------------------------

test_that("map applies a function over a list", {
  one_letter <- gh_map("D", function(l) which(LETTERS == l))

  expect_is(one_letter, "list")
  expect_identical(one_letter, list(D = 4L))

  some_letters <- gh_map(c("D", "G", "W"), function(l) which(LETTERS == l))

  expect_is(some_letters, "list")
  expect_identical(some_letters, list(D = 4L, G = 7L, W = 23L))

  indexed_letters <- gh_map(c("D", "G", "W"), idx = 0, function(l, idx) paste0(idx, which(LETTERS == l)))

  expect_is(indexed_letters, "list")
  expect_identical(indexed_letters, list(D = "04", G = "07", W = "023"))

  named_list <- gh_map(list(bob = "manager", mary = "analyst"), function(p) p == "manager")

  expect_is(named_list, "list")
  expect_identical(named_list, list(bob = TRUE, mary = FALSE))

  unnamed_list <- gh_map(list(bob = "manager", mary = "analyst"), function(p) p == "manager", use_names = FALSE)

  expect_is(unnamed_list, "list")
  expect_identical(unnamed_list, list(TRUE, FALSE))
})

# TEST: pmap ----------------------------------------------------------------------------------

test_that("pmap applies a function over a list of lists", {
  one_letter <- gh_pmap(list(a = "A", b = 1), function(a, b) paste0(a, b))

  expect_is(one_letter, "list")
  expect_identical(one_letter, list(A = "A1"))

  some_letters <- gh_pmap(list(a = c("D", "G", "W"), b = 1:3), function(a, b) paste0(a, b))

  expect_is(some_letters, "list")
  expect_identical(some_letters, list(D = "D1", G = "G2", W = "W3"))

  indexed_letters <- gh_pmap(
    list(a = c("D", "G", "W"), b = 1:3),
    idx = 0,
    function(a, b, idx) paste0(idx, a, b))

  expect_is(indexed_letters, "list")
  expect_identical(indexed_letters, list(D = "0D1", G = "0G2", W = "0W3"))

  named_list <- gh_pmap(
    list(role = c(bob = "manager", mary = "analyst"), level = c(bob = 1, mary = 2)),
    function(role, level) paste(role, level))

  expect_is(named_list, "list")
  expect_identical(named_list, list(bob = "manager 1", mary = "analyst 2"))

  unnamed_list <- gh_pmap(
    list(role = c(bob = "manager", mary = "analyst"), level = c(bob = 1, mary = 2)),
    function(role, level) paste(role, level),
    use_names = FALSE)

  expect_is(unnamed_list, "list")
  expect_identical(unnamed_list, list("manager 1", "analyst 2"))
})

# TEST: set_names -----------------------------------------------------------------------------

test_that("set_names adds or replaces the names of an object", {
  expect_identical(
    set_names(c(1, 2, 3), "A", "B", "C"),
    c(A = 1, B = 2, C = 3))

  expect_identical(
    set_names(c(1, 2, 3), c("A", "B", "C")),
    c(A = 1, B = 2, C = 3))

  expect_identical(
    set_names(c(1, 2, 3), list("A", "B", "C")),
    c(A = 1, B = 2, C = 3))
})

# TEST: select_fields -------------------------------------------------------------------------

test_that("select_fields returns the specified fields with correct names", {
  data <- list(
    list(name = "A", is_ok = TRUE, date = "2018-06-21T08:56:23Z", value = 23, author = list(
      name = "Bob Wilson", email = "bob.wilson@acme.com")),
    list(name = "B", is_ok = FALSE, date = "2018-06-22T23:12:32Z", value = 100.45, author = list(
      name = "Jane Smith")),
    list(name = "C", is_ok = FALSE))

  complete_data <- select_fields(data[[1]], fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(complete_data, list(
    name = "A",
    date = "2018-06-21T08:56:23Z",
    value = 23,
    author_name = "Bob Wilson",
    author_email = "bob.wilson@acme.com"))

  missing_nested_data <- select_fields(data[[2]], fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(missing_nested_data, list(
    name = "B",
    date = "2018-06-22T23:12:32Z",
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
    date = "2018-06-21T08:56:23Z",
    value = 23,
    author = "Bob Wilson",
    email = "bob.wilson@acme.com"))

})

# TEST: bind_fields ---------------------------------------------------------------------------

test_that("bind_fields returns the specified columns with correct names and types", {
  data <- list(
    list(name = "A", is_ok = TRUE, date = "2018-06-21T08:56:23Z", value = 23, author = list(
      name = "Bob Wilson", email = "bob.wilson@acme.com")),
    list(name = "B", is_ok = FALSE, date = "2018-06-22T23:12:32Z", value = 100.45, author = list(
      name = "Jane Smith")),
    list(name = "C", is_ok = FALSE))

  basic_data <- bind_fields(data, fields = list(
    "name", "date", "value", c("author", "name"), c("author", "email")))
  expect_identical(basic_data, tibble(
    name = c("A", "B", "C"),
    date = c("2018-06-21T08:56:23Z", "2018-06-22T23:12:32Z", NA),
    value = c(23, 100.45, NA),
    author_name = c("Bob Wilson", "Jane Smith", NA),
    author_email = c("bob.wilson@acme.com", NA, NA)))

  renamed_data <- bind_fields(data, fields = list(
    "name", "date", "value", author = c("author", "name"), email = c("author", "email")))
  expect_identical(renamed_data, tibble(
    name = c("A", "B", "C"),
    date = c("2018-06-21T08:56:23Z", "2018-06-22T23:12:32Z", NA),
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
