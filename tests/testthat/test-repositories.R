context("repositories api")

#  FUNCTION: gh_repository --------------------------------------------------------------------
test_that("gh_repository returns a list describing the repository", {
  repo <- gh_repository("ChadGoymer/githapi")
  expect_is(repo, "list")
  expect_identical(repo$name, "githapi")
  expect_identical(repo$owner$login, "ChadGoymer")
})

test_that("gh_repository returns an error is the specified repo does not exist", {
  expect_error(gh_repository("SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_repositories ------------------------------------------------------------------
test_that("gh_repositories returns a tibble describing all the repositories a user has", {
  repos <- gh_repositories("ChadGoymer")
  expect_is(repos, "tbl")
  expect_true("githapi" %in% repos$name)
  expect_identical(repos$name, sort(repos$name))

  repos <- gh_repositories("ChadGoymer", sort = "updated")
  expect_identical(repos$updated_at, sort(repos$updated_at, decreasing = TRUE))
})

test_that("gh_repositories returns a tibble describing all the repositories an org has", {
  repos <- gh_repositories("tidyverse")
  expect_is(repos, "tbl")
  expect_true(all(c("dplyr", "tidyr") %in% repos$name))
})

test_that("gh_repositories returns an error is the specified owner does not exist", {
  expect_error(gh_repositories("SomeNameThatDoesNotExist"))
})

#  FUNCTION: gh_tags ----------------------------------------------------------------------
test_that("gh_tags returns a tibble describing all the tags", {
  tags <- gh_tags("ChadGoymer/githapi")
  expect_is(tags, "tbl")
  expect_true("v0.0.0" %in% tags$name)
  expect_identical(names(tags), c("name", "commit_sha", "commit_url", "zipball_url", "tarball_url"))
})

test_that("gh_tags returns an error is the specified repo does not exist", {
  expect_error(gh_tags("SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_branch ------------------------------------------------------------------------
test_that("gh_branch returns a list describing the branch", {
  branch <- gh_branch("master", "ChadGoymer/githapi")
  expect_is(branch, "list")
  expect_identical(branch$name, "master")
})

test_that("gh_branch returns an error is the specified branch or repo does not exist", {
  expect_error(gh_branch("no_branch", "ChadGoymer/githapi"))
  expect_error(gh_branch("master", "SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_branches ------------------------------------------------------------------
test_that("gh_branches returns a tibble describing all the branches", {
  branches <- gh_branches("ChadGoymer/githapi")
  expect_is(branches, "tbl")
  expect_true("master" %in% branches$name)
  expect_identical(names(branches), c("name", "commit_sha", "commit_url"))
})

test_that("gh_branches returns an error is the specified repo does not exist", {
  expect_error(gh_branches("SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_commit ------------------------------------------------------------------------
test_that("gh_commit returns a list describing the commit", {
  commit_master <- gh_commit("master", "ChadGoymer/githapi")
  expect_is(commit_master, "list")
  expect_true(all(c("sha", "commit", "author", "committer", "files") %in% names(commit_master)))

  commit_d9fe50f <- gh_commit("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi")
  expect_identical(commit_d9fe50f$sha, "d9fe50f8e31d7430df2c5b02442dffb68c854f08")
  expect_identical(commit_d9fe50f$commit$message, "Initial commit")
  expect_identical(commit_d9fe50f$author$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$committer$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$files[[1]]$filename, "README.md")
})

test_that("gh_commit returns an error is the specified commit or repo does not exist", {
  expect_error(gh_commit("no_commit", "ChadGoymer/githapi"))
  expect_error(gh_commit("master", "SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_commit_sha --------------------------------------------------------------------
test_that("gh_commit_sha returns a string with the SHA-1", {
  commit_sha <- gh_commit_sha("v0.0.0", "ChadGoymer/githapi")
  expect_true(is.string(commit_sha))
  expect_identical(commit_sha, "ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964")
})

#  FUNCTION: gh_commits -------------------------------------------------------------------
test_that("gh_commits returns a tibble describing all the commits on a branch", {
  commits <- gh_commits("master", "ChadGoymer/githapi", n_max = 1000)
  expect_is(commits, "tbl")
  expect_true("d9fe50f8e31d7430df2c5b02442dffb68c854f08" %in% commits$sha)
  expect_identical(
    names(commits),
    c("sha", "date", "message", "url", "author_name", "author_email",
      "committer_name", "committer_email", "tree_sha", "tree_url"))
})

test_that("gh_commits returns an error is the specified repo does not exist", {
  expect_error(gh_commits("master", "SomeNameThatDoesNotExist/repo"))
})

#  FUNCTION: gh_compare_commits ---------------------------------------------------------------
test_that("gh_compare_commits returns information on the differences between two commits", {
  comparison <- gh_compare_commits(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi")

  expect_true(is.tibble(comparison))

  expect_identical(
    names(comparison),
    c("sha", "date", "message", "url", "author_name", "author_email",
      "committer_name", "committer_email"))

  expect_identical(
    comparison$sha,
    c("2a1e6b031d75bb97d94ab9ee26272a052da83339", "d8a62ccdc3df3e002dbac55390772424b136844a"))
})

#  FUNCTION: gh_compare_files -----------------------------------------------------------------
test_that("gh_compare_files returns a tibble of information of file differences between commits", {
  comparison <- gh_compare_files(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi")

  expect_is(comparison, "tbl")

  expect_identical(
    names(comparison),
    c("filename", "status", "additions", "deletions", "changes", "contents_url"))

  expect_identical(
    comparison$filename,
    c(".Rbuildignore",
      ".gitignore",
      "DESCRIPTION",
      "NAMESPACE",
      "R/githapi.R",
      "R/repositories.R",
      "R/utilities.R",
      "githapi.Rproj",
      "man/gh_branch.Rd",
      "man/gh_branches.Rd",
      "man/githapi.Rd",
      "tests/testthat.R",
      "tests/testthat/test-repositories.R"))
})

#  FUNCTION: gh_readme ------------------------------------------------------------------------
test_that("gh_readme returns the text in the README file", {
  readme_d9fe50f <- gh_readme("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi")
  expect_true(is.string(readme_d9fe50f))
  expect_identical(
    readme_d9fe50f,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_contents ----------------------------------------------------------------------
test_that("gh_contents returns the text in a specified file", {
  description_master <- gh_contents("DESCRIPTION", "master", "ChadGoymer/githapi")
  expect_true(is.string(description_master))

  readme_d9fe50f <- gh_contents("README.md", "d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi")
  expect_identical(
    readme_d9fe50f,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_download ----------------------------------------------------------------------
test_that("gh_download saves the contents of a commit to the specified location", {
  temp_path <- tempdir()
  on.exit(unlink(temp_path, recursive = TRUE))
  gh_download("master", "ChadGoymer/githapi", temp_path)

  expect_true(file.exists(file.path(temp_path, "DESCRIPTION")))
  expect_true(dir.exists(file.path(temp_path, "R")))
})

#  FUNCTION: gh_collaborator ------------------------------------------------------------------
test_that("gh_collaborator return TRUE if the user is a collaborator, FALSE otherwise", {
  is_collaborator <- gh_collaborator("ChadGoymer", "ChadGoymer/githapi")
  expect_true(is_collaborator)

  not_collaborator <- gh_collaborator("Batman", "ChadGoymer/githapi")
  expect_false(not_collaborator)
})

#  FUNCTION: gh_collaborators -----------------------------------------------------------------
test_that("gh_collaborators returns a tibble describing the collaborators", {
  collaborators <- gh_collaborators("ChadGoymer/githapi")
  expect_is(collaborators, "tbl")
  expect_identical(
    names(collaborators),
    c("id", "login", "type", "site_admin", "permissions_admin", "permissions_push",
      "permissions_pull", "url"))
  expect_true("ChadGoymer" %in% collaborators$login)
})

#  FUNCTION: gh_permissions -------------------------------------------------------------------
test_that("gh_permissions returns a list describing the user's permissions", {
  permissions <- gh_permissions("ChadGoymer", "ChadGoymer/githapi")
  expect_is(permissions, "list")
  expect_identical(names(permissions), c("permission", "user"))
  expect_identical(permissions$permission, "admin")
  expect_identical(permissions$user$login, "ChadGoymer")
})
