context("git data api")

#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
test_that("gh_git_blob returns the contents of a file in the repository", {
  readme <- suppressWarnings(
    gh_git_blob("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "ChadGoymer/githapi"))

  expect_true(is_string(readme))
  expect_identical(
    readme,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_git_commit --------------------------------------------------------------------
test_that("gh_git_commit returns a list of information about a commit", {
  git_commit <- gh_git_commit("7ca61bb71f877f462c0b6132759d7c5e507c921f", "ChadGoymer/githapi")
  expect_is(git_commit, "list")
  expect_named(
    git_commit,
    c("sha", "node_id", "url", "html_url", "author", "committer",
      "tree", "message", "parents", "verification"))
  expect_identical(git_commit$sha, "7ca61bb71f877f462c0b6132759d7c5e507c921f")
  expect_identical(git_commit$author$name, "Chad Goymer")
  expect_identical(git_commit$message, "removed reference to github_url\n\nAlso added gh_readme and gh_commit_sha")
})

#  FUNCTION: gh_git_reference -----------------------------------------------------------------
test_that("gh_git_reference returns information about a git reference", {
  head_master <- gh_git_reference("heads/master", "ChadGoymer/githapi")
  expect_is(head_master, "list")
  expect_identical(head_master$ref, "refs/heads/master")

  tag_v0.1.0 <- gh_git_reference("tags/v0.1.0", "ChadGoymer/githapi")
  expect_is(tag_v0.1.0, "list")
  expect_identical(tag_v0.1.0$ref, "refs/tags/v0.1.0")
})

#  FUNCTION: is_tag ---------------------------------------------------------------------------
test_that("is_tag returns a boolean, with attributes describing the errors, if there are any", {
  expect_true(is_tag("v0.1.0", "ChadGoymer/githapi"))
  expect_false(is_tag(list(x = "alist"), "ChadGoymer/githapi"))
  expect_false(is_tag("0.0.9000", "ChadGoymer/githapi"))
})

#  FUNCTION: gh_git_references ----------------------------------------------------------------
test_that("gh_git_references returns a tibble of information about references", {
  references <- gh_git_references("ChadGoymer/githapi")
  expect_is(references, "tbl")

  expect_identical(
    sapply(references, function(field) class(field)[[1]]),
    c(name        = "character",
      type        = "character",
      object_type = "character",
      object_sha  = "character",
      ref         = "character",
      url         = "character"))

  expect_true(all(c("master", "v0.0.0") %in% references$name))
  expect_true("ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964" %in% references$object_sha)
})

#  FUNCTION: gh_git_tag -----------------------------------------------------------------------
test_that("gh_git_tags returns a list of information about a tag", {
  test_tag <- gh_git_tag("30426b4f967d8c253b1bb5a67c5838dc306aab50", "ChadGoymer/githapi")
  expect_is(test_tag, "list")
  expect_named(
    test_tag,
    c("node_id", "sha", "url", "tagger", "object", "tag", "message", "verification"))
  expect_identical(test_tag$sha, "30426b4f967d8c253b1bb5a67c5838dc306aab50")
  expect_identical(test_tag$tagger$name, "Chad Goymer")
  expect_identical(test_tag$object$sha, "ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964")
})

#  FUNCTION: gh_git_tree ----------------------------------------------------------------------
test_that("gh_git_tree returns a tibble of information about the files in a commit", {
  tree <- gh_git_tree("ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964", "ChadGoymer/githapi")
  expect_is(tree, "tbl")

  expect_identical(
    sapply(tree, function(field) class(field)[[1]]),
    c(path = "character",
      type = "character",
      sha  = "character",
      size = "integer",
      url  = "character"))

  expect_true(all(c("README.md", "R/githapi.R") %in% tree$path))
  expect_true(all(
    c("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "9b3517bffc7f42185b6f3465ac1f739547157943") %in%
      tree$sha))
})

#  FUNCTION: gh_save --------------------------------------------------------------------------
test_that("gh_save creates a local copy of a file in GitHub", {
  temp_path <- file.path(tempdir(), "test-gh_save")
  on.exit(unlink(temp_path, recursive = TRUE))

  gh_save("DESCRIPTION", "ChadGoymer/githapi", path = temp_path)
  expect_true(file.exists(file.path(temp_path, "DESCRIPTION")))
  expect_identical(readLines(file.path(temp_path, "DESCRIPTION"), n = 1), "Package: githapi")

  unlink(temp_path, recursive = TRUE)

  gh_save("DESCRIPTION", "ChadGoymer/githapi", path = temp_path, ref = "v0.3.0")
  expect_true(file.exists(file.path(temp_path, "DESCRIPTION")))
  desc_version <- readLines(file.path(temp_path, "DESCRIPTION")) %>% grep("Version", ., value = TRUE)
  expect_identical(desc_version, "Version: 0.3.0")

  unlink(temp_path, recursive = TRUE)

  gh_save(c("DESCRIPTION", "README.md"), "ChadGoymer/githapi", path = temp_path, ref = "v0.3.0")
  expect_true(all(file.exists(file.path(temp_path, "DESCRIPTION"), file.path(temp_path, "README.md"))))
})

#  FUNCTION: gh_source ------------------------------------------------------------------------
test_that("gh_source sources a file in GitHub", {
  gh_source("inst/test-data/test-source.R", "ChadGoymer/githapi", ref = "master")
  expect_true(exists("test_source"))
  expect_is(test_source, "function")
  expect_identical(test_source(), "Testing gh_source")
})
