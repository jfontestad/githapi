context("git data api")

#  FUNCTION: gh_git_blob ----------------------------------------------------------------------
test_that("gh_git_blob returns the contents of a file in the repository", {
  readme <- gh_git_blob("abb7f8ce52e6bdea33170ec8edbd6cfb1eca0722", "ChadGoymer/githapi")
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
  gh_source("inst/test-data/test-source.R", "ChadGoymer/githapi", ref = "develop")
  expect_true(exists("test_source"))
  expect_is(test_source, "function")
  expect_identical(test_source(), "Testing gh_source")
})

# TEST: view_tags, create_tags & delete_tags --------------------------------------------------
test_that("create_tags creates some tags, view_tags retreives them and delete_tags deletes them", {
  all_tags <- view_tags(repo = "ChadGoymer/githapi")

  expect_is(all_tags, "tbl")
  expect_identical(
    names(all_tags),
    c("name", "ref", "url", "object_sha", "object_type", "object_url"))
  expect_true("test-tag" %in% all_tags$name)
  expect_identical(
    filter(all_tags, name == "test-tag") %>% pull(object_sha),
    "30426b4f967d8c253b1bb5a67c5838dc306aab50")

  created_tags <- create_tags(
    tags = c("aaa", "bbb"),
    shas = c("dc7da99ef557f362d5f2c0f006a240f84927a8a9", "47c2562fe0800a997fa3ed595edaac926b6c7f8a"),
    repo = "ChadGoymer/githapi")

  expect_is(created_tags, "tbl")
  expect_identical(
    names(created_tags),
    c("name", "ref", "url", "object_sha", "object_type", "object_url"))
  expect_identical(created_tags$ref, c("refs/tags/aaa", "refs/tags/bbb"))
  expect_identical(
    created_tags$object_sha,
    c("dc7da99ef557f362d5f2c0f006a240f84927a8a9", "47c2562fe0800a997fa3ed595edaac926b6c7f8a"))

  viewed_tags <- view_tags("ChadGoymer/githapi", c("aaa", "bbb"))

  expect_is(viewed_tags, "tbl")
  expect_identical(
    names(viewed_tags),
    c("name", "ref", "url", "object_sha", "object_type", "object_url"))
  expect_identical(viewed_tags$name, c("aaa", "bbb"))
  expect_identical(
    viewed_tags$object_sha,
    c("dc7da99ef557f362d5f2c0f006a240f84927a8a9", "47c2562fe0800a997fa3ed595edaac926b6c7f8a"))

  updated_tags <- update_tags(
    tags = c("aaa", "bbb"),
    shas = c("fdeca491336b285a662fc9b92f13072ade881295", "9bb32a0c6ce901ceeab1f08870bb06781d16e756"),
    repo = "ChadGoymer/githapi")

  expect_is(updated_tags, "tbl")
  expect_identical(
    names(updated_tags),
    c("name", "ref", "url", "object_sha", "object_type", "object_url"))
  expect_identical(updated_tags$ref, c("refs/tags/aaa", "refs/tags/bbb"))
  expect_identical(
    updated_tags$object_sha,
    c("fdeca491336b285a662fc9b92f13072ade881295", "9bb32a0c6ce901ceeab1f08870bb06781d16e756"))

  delete_results <- delete_tags("ChadGoymer/githapi", c("aaa", "bbb"))

  expect_identical(delete_results, list(aaa = TRUE, bbb = TRUE))
  expect_error(view_tags("ChadGoymer/githapi", "aaa"), "Not Found")
  expect_error(view_tags("ChadGoymer/githapi", "bbb"), "Not Found")
})
