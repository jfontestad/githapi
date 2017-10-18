context("github api")

#  FUNCTION: gh_url ----------------------------------------------------------------------------
test_that("gh_url returns a valid URL for the GitHub API", {
  expect_identical(
    gh_url("repos"),
    file.path(getOption("github.api"), "repos"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/refs", "heads/master"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/refs", "heads/master")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/refs/heads/master"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1)),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1"))

  expect_identical(
    gh_url("repos", "ChadGoymer/githapi", "git/trees", "234752384", recursive = 1, type = "bob"),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list(recursive = 1, type = "bob")),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384?recursive=1&type=bob"))

  expect_identical(
    gh_url(c("repos", "ChadGoymer/githapi", "git/trees", "234752384"), list()),
    file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/234752384"))
})

# FUNCTION: gh_get ---------------------------------------------------------------------------
test_that("gh_get returns a parsed list by default", {
  repo_info <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi") %>% gh_get()
  expect_is(repo_info, "list")
  expect_identical(repo_info$name, "githapi")
})

test_that("gh_get returns raw text when accept = raw", {
  readme <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi/readme") %>%
    gh_get(accept = "raw")
  expect_true(is.string(readme))
  expect_match(readme, "^githapi\n=======\n\nUser-friendly access to the GitHub API for R")
})

test_that("gh_get returns a parsed tibble when simplify = TRUE", {
  issues <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi/issues") %>%
    gh_get(simplify = TRUE)
  expect_is(issues, "tbl")
  expect_true("Test issue" %in% issues$title)
})

test_that("gh_get returns a parsed tibble of a sublist when simplify = TRUE and sub_list is specified", {
  tree <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi/git/trees/master") %>%
    gh_get(sub_list = "tree", simplify = TRUE)
  expect_is(tree, "tbl")
  expect_true("README.md" %in% tree$path)
})

# FUNCTION: gh_page ---------------------------------------------------------------------------
test_that("gh_page returns a list of specified length", {
  commits <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits") %>%
    gh_page(n_max = 20)
  expect_is(commits, "list")
  expect_identical(length(commits), 20L)
})

test_that("gh_page returns a tibble of specified number of rows when simplify = TRUE", {
  commits <- file.path(getOption("github.api"), "repos/ChadGoymer/githapi/commits") %>%
    gh_page(simplify = TRUE, n_max = 20)
  expect_is(commits, "tbl")
  expect_identical(nrow(commits), 20L)
})

# FUNCTION: select_safe -----------------------------------------------------------------------
test_that("select_safe returns a tibble with the selected columns", {
  expect_identical(
    select_safe(tibble(), new_name = name, login),
    tibble(new_name = NA_character_, login = NA_character_))

  valid_tbl <- tibble(name = c("bob", "jane"), login = c("smithb", "brownj"), job = c("actuary", "engineer"))
  expect_identical(
    select_safe(valid_tbl, new_name = name, login),
    tibble(new_name = c("bob", "jane"), login = c("smithb", "brownj")))

  missing_column <- tibble(name = c("bob", "jane"), job = c("actuary", "engineer"))
  expect_identical(
    select_safe(missing_column, new_name = name, login),
    tibble(new_name = c("bob", "jane"), login = as.character(NA, NA)))

  missing_column2 <- tibble(login = c("smithb", "brownj"), job = c("actuary", "engineer"))
  expect_identical(
    select_safe(missing_column2, new_name = name, login),
    tibble(new_name = as.character(NA, NA), login = c("smithb", "brownj")))

  missing_column3 <- tibble(name = c("bob", "jane"), job = c("actuary", "engineer"))
  expect_identical(
    select_safe(missing_column3, new_name = name, login, email),
    tibble(new_name = c("bob", "jane"), login = as.character(NA, NA), email = as.character(NA, NA)))
})
