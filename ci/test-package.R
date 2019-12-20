library(stringr)
library(purrr)
library(tibble)
library(dplyr)
library(readr)
library(msgr)

options(msgr.level = 10)
devtools::load_all()


devtools::check(args = "--no-tests")
devtools::test()
devtools::test_coverage(type = "tests", quiet = FALSE)
devtools::spell_check()


devtools::test_file("tests/testthat/test-github-api.R")
devtools::test_file("tests/testthat/test-utilities.R")
devtools::test_file("tests/testthat/test-projects.R")
devtools::test_file("tests/testthat/test-columns.R")
devtools::test_file("tests/testthat/test-cards.R")

devtools::test_coverage_file("tests/testthat/test-github-api.R")
devtools::test_coverage_file("tests/testthat/test-utilities.R")
devtools::test_coverage_file("tests/testthat/test-projects.R")
devtools::test_coverage_file("tests/testthat/test-columns.R")
devtools::test_coverage_file("tests/testthat/test-cards.R")
