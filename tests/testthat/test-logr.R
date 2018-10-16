context("messages")

# TEST: parse_vector --------------------------------------------------------------------------

test_that("parse_vector returns quoted strings", {
  expect_output(parse_vector(LETTERS[1:3]), "\"A\",\n\"B\",\n\"C\"")
  expect_output(parse_vector(LETTERS[1:3], sep = ", "), "\"A\", \"B\", \"C\"")
  expect_output(
    parse_vector(c(A = "aaa", B = "bbb", C = "ccc")),
    "A = \"aaa\",\nB = \"bbb\",\nC = \"ccc\"")
})

# TEST: info ----------------------------------------------------------------------------------

test_that("info displays a message, and records it in a log file", {
  log_path <- tempdir()

  expect_message(
    info("This is INFO", level = 1, debug_types = "INFO", debug_level = 1, log_path = log_path),
    "This is INFO")

  log_file <- log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
  log_file_path <- file.path(log_path, log_file)
  expect_true(file.exists(log_file_path))

  log <- readLines(log_file_path)
  expect_match(log[length(log)], "INFO: This is INFO")

  expect_silent(
    info("This is INFO", level = 2, debug_types = "INFO", debug_level = 1, log_path = log_path))

  expect_silent(
    info("This is INFO", level = 1, debug_types = "ERROR", debug_level = 1, log_path = log_path))

  expect_message(
    info("This is INFO", level = 2, debug_types = "INFO", debug_level = 3, log_path = log_path),
    "This is INFO")

  log_lines <- length(readLines(log_file_path))

  expect_message(
    info("This is INFO", level = 1, debug_types = "INFO", debug_level = 1, log_path = ""),
    "This is INFO")

  expect_identical(length(readLines(log_file_path)), log_lines)
})

# TEST: warn ----------------------------------------------------------------------------------

test_that("warn displays a warning, and records it in a log file", {
  log_path <- tempdir()

  expect_warning(
    warn("This is a WARNING", level = 1, debug_types = "WARNING", debug_level = 1, log_path = log_path),
    "This is a WARNING")

  log_file <- log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
  log_file_path <- file.path(log_path, log_file)
  expect_true(file.exists(log_file_path))

  log <- readLines(log_file_path)
  expect_match(log[length(log)], "WARNING: This is a WARNING")

  expect_silent(
    warn("This is a WARNING", level = 2, debug_types = "WARNING", debug_level = 1, log_path = log_path))

  expect_silent(
    warn("This is a WARNING", level = 1, debug_types = "ERROR", debug_level = 1, log_path = log_path))

  expect_warning(
    warn("This is a WARNING", level = 2, debug_types = "WARNING", debug_level = 3, log_path = log_path),
    "This is a WARNING")

  log_lines <- length(readLines(log_file_path))

  expect_warning(
    warn("This is a WARNING", level = 1, debug_types = "WARNING", debug_level = 1, log_path = ""),
    "This is a WARNING")

  expect_identical(length(readLines(log_file_path)), log_lines)
})

# TEST: error ---------------------------------------------------------------------------------

test_that("error displays an error, and records it in a log file", {
  log_path <- tempdir()

  expect_error(
    error("This is an ERROR", level = 1, debug_types = "ERROR", debug_level = 1, log_path = log_path),
    "This is an ERROR")

  log_file <- log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
  log_file_path <- file.path(log_path, log_file)
  expect_true(file.exists(log_file_path))

  log <- readLines(log_file_path)
  expect_match(log[length(log)], "ERROR: This is an ERROR")

  expect_silent(
    error("This is an ERROR", level = 2, debug_types = "ERROR", debug_level = 1, log_path = log_path))

  expect_silent(
    error("This is an ERROR", level = 1, debug_types = "INFO", debug_level = 1, log_path = log_path))

  expect_error(
    error("This is an ERROR", level = 2, debug_types = "ERROR", debug_level = 3, log_path = log_path),
    "This is an ERROR")

  log_lines <- length(readLines(log_file_path))

  expect_error(
    error("This is an ERROR", level = 1, debug_types = "ERROR", debug_level = 1, log_path = ""),
    "This is an ERROR")

  expect_identical(length(readLines(log_file_path)), log_lines)
})

# TEST: info_if -------------------------------------------------------------------------------

test_that("info_if returns a message if the condition is true", {
  test_info_if <- function(x, y) info_if(x > y)

  expect_message(test_info_if(2, 1), "In test_info_if\\(\\): x > y is true")
  expect_silent(test_info_if(1, 2))

  test_info_if_msg <- function(x, y) info_if(x > y, "This is rubbish")
  expect_message(test_info_if_msg(2, 1), "In test_info_if_msg\\(\\): This is rubbish")
})

# TEST: warn_if -------------------------------------------------------------------------------

test_that("warn_if returns a warning if the condition is true", {
  test_warn_if <- function(x, y) warn_if(x > y)

  expect_warning(test_warn_if(2, 1), "In test_warn_if\\(\\): x > y is true")
  expect_silent(test_warn_if(1, 2))

  test_warn_if_msg <- function(x, y) warn_if(x > y, "This is rubbish")
  expect_warning(test_warn_if_msg(2, 1), "In test_warn_if_msg\\(\\): This is rubbish")
})

# TEST: error_if ------------------------------------------------------------------------------

test_that("error_if returns an error if the condition is true", {
  test_error_if <- function(x, y) error_if(x > y)

  expect_error(test_error_if(2, 1), "In test_error_if\\(\\): x > y is true")
  expect_silent(test_error_if(1, 2))

  test_error_if_msg <- function(x, y) error_if(x > y, "This is rubbish")
  expect_error(test_error_if_msg(2, 1), "In test_error_if_msg\\(\\): This is rubbish")
})

# TEST: assert --------------------------------------------------------------------------------

test_that("assert returns an error if the condition is false", {
  test_assert <- function(x, y) assert(x > y)

  expect_error(test_assert(1, 2), "In test_assert\\(\\): x > y is not true")
  expect_silent(test_assert(2, 1))

  test_assert_msg <- function(x, y) assert(x > y, "This is rubbish")
  expect_error(test_assert_msg(1, 2), "In test_assert_msg\\(\\): This is rubbish")
})
