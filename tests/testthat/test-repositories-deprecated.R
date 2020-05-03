context("repositories")

#  FUNCTION: gh_repository --------------------------------------------------------------------
test_that("gh_repository returns a list describing the repository", {
  repo <- suppressWarnings(gh_repository("ChadGoymer/githapi"))
  expect_is(repo, "list")
  expect_identical(repo$name, "githapi")
  expect_identical(repo$owner$login, "ChadGoymer")
})

test_that("gh_repository returns an error is the specified repo does not exist", {
  expect_error(suppressWarnings(gh_repository("SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: is_repository --------------------------------------------------------------------
test_that("is_repository returns a boolean, with attributes describing the errors, if there are any", {
  expect_true(suppressWarnings(is_repository("ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_repository("githapi")))
  expect_false(suppressWarnings(is_repository("DoesNotExist/githapi")))
})

#  FUNCTION: gh_repositories ------------------------------------------------------------------
test_that("gh_repositories returns a tibble describing all the repositories a user has", {
  repos <- suppressWarnings(gh_repositories("ChadGoymer"))
  expect_is(repos, "tbl")
  expect_true("githapi" %in% repos$name)
  expect_identical(repos$name, sort(repos$name))

  expect_identical(
    sapply(repos, function(field) class(field)[[1]]),
    c(name           = "character",
      description    = "character",
      owner_login    = "character",
      owner_type     = "character",
      default_branch = "character",
      open_issues    = "integer",
      size           = "integer",
      url            = "character",
      html_url       = "character",
      created_at     = "POSIXct",
      updated_at     = "POSIXct"))

  repos <- suppressWarnings(gh_repositories("ChadGoymer", sort = "updated"))
  expect_identical(repos$updated_at, sort(repos$updated_at, decreasing = TRUE))
})

test_that("gh_repositories returns a tibble describing all the repositories an org has", {
  repos <- suppressWarnings(gh_repositories("tidyverse"))
  expect_is(repos, "tbl")
  expect_true(all(c("dplyr", "tidyr") %in% repos$name))
})

test_that("gh_repositories returns an error is the specified owner does not exist", {
  expect_error(suppressWarnings(gh_repositories("SomeNameThatDoesNotExist")))
})

#  FUNCTION: gh_tags ----------------------------------------------------------------------
test_that("gh_tags returns a tibble describing all the tags", {
  tags <- suppressWarnings(gh_tags("ChadGoymer/githapi"))

  expect_is(tags, "tbl")
  expect_true("v0.0.0" %in% tags$name)
  expect_identical(
    sapply(tags, function(field) class(field)[[1]]),
    c(name        = "character",
      commit_sha  = "character",
      commit_url  = "character",
      zipball_url = "character",
      tarball_url = "character"))
})

test_that("gh_tags returns an error is the specified repo does not exist", {
  expect_error(suppressWarnings(gh_tags("SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: gh_branch ------------------------------------------------------------------------
test_that("gh_branch returns a list describing the branch", {
  branch <- suppressWarnings(gh_branch("master", "ChadGoymer/githapi"))
  expect_is(branch, "list")
  expect_identical(branch$name, "master")
})

test_that("gh_branch returns an error is the specified branch or repo does not exist", {
  expect_error(suppressWarnings(gh_branch("no_branch", "ChadGoymer/githapi")))
  expect_error(suppressWarnings(gh_branch("master", "SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: is_branch ------------------------------------------------------------------------
test_that("is_branch returns a boolean, with attributes describing the errors, if there are any", {
  expect_true(suppressWarnings(is_branch("master", "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_branch(list(x = "alist"), "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_branch("no_branch", "ChadGoymer/githapi")))
})

#  FUNCTION: gh_branches ------------------------------------------------------------------
test_that("gh_branches returns a tibble describing all the branches", {
  branches <- suppressWarnings(gh_branches("ChadGoymer/githapi"))

  expect_is(branches, "tbl")
  expect_true("master" %in% branches$name)
  expect_identical(
    sapply(branches, function(field) class(field)[[1]]),
    c(name       = "character",
      commit_sha = "character",
      commit_url = "character"))
})

test_that("gh_branches returns an error is the specified repo does not exist", {
  expect_error(suppressWarnings(gh_branches("SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: gh_commit ------------------------------------------------------------------------
test_that("gh_commit returns a list describing the commit", {
  commit_master <- suppressWarnings(gh_commit("master", "ChadGoymer/githapi"))

  expect_is(commit_master, "list")
  expect_true(all(c("sha", "commit", "author", "committer", "files") %in% names(commit_master)))

  commit_d9fe50f <- suppressWarnings(
    gh_commit("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi"))

  expect_identical(commit_d9fe50f$sha, "d9fe50f8e31d7430df2c5b02442dffb68c854f08")
  expect_identical(commit_d9fe50f$commit$message, "Initial commit")
  expect_identical(commit_d9fe50f$author$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$committer$login, "ChadGoymer")
  expect_identical(commit_d9fe50f$files[[1]]$filename, "README.md")
})

test_that("gh_commit returns an error is the specified commit or repo does not exist", {
  expect_error(suppressWarnings(gh_commit("no_commit", "ChadGoymer/githapi")))
  expect_error(suppressWarnings(gh_commit("master", "SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: is_valid_sha ---------------------------------------------------------------------
test_that("is_valid_sha returns a boolean, with attributes describing the errors, if there are any", {
  expect_true(suppressWarnings(is_valid_sha("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_valid_sha("aaaaa", "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_valid_sha("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa", "ChadGoymer/githapi")))
})

#  FUNCTION: gh_commit_sha --------------------------------------------------------------------
test_that("gh_commit_sha returns a string with the SHA-1", {
  commit_sha <- suppressWarnings(gh_commit_sha("v0.0.0", "ChadGoymer/githapi"))
  expect_true(is_scalar_character(commit_sha))
  expect_identical(commit_sha, "ad7e70df7c81ab7c0edbb26725ae7cf4b2ce8964")
})

#  FUNCTION: gh_commits -------------------------------------------------------------------
test_that("gh_commits returns a tibble describing all the commits on a branch", {
  commits <- suppressWarnings(gh_commits("master", "ChadGoymer/githapi", n_max = 1000))
  expect_is(commits, "tbl")
  expect_true("d9fe50f8e31d7430df2c5b02442dffb68c854f08" %in% commits$sha)
  expect_identical(
    sapply(commits, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character"))
})

test_that("gh_commits returns an error is the specified repo does not exist", {
  expect_error(suppressWarnings(gh_commits("master", "SomeNameThatDoesNotExist/repo")))
})

#  FUNCTION: gh_compare_commits ---------------------------------------------------------------
test_that("gh_compare_commits returns information on the differences between two commits", {
  comparison <- suppressWarnings(gh_compare_commits(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi"))

  expect_is(comparison, "tbl")

  expect_identical(
    sapply(comparison, function(field) class(field)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_name     = "character",
      author_email    = "character",
      committer_name  = "character",
      committer_email = "character",
      date            = "POSIXct",
      url             = "character",
      tree_sha        = "character",
      tree_url        = "character"))

  expect_identical(
    comparison$sha,
    c("2a1e6b031d75bb97d94ab9ee26272a052da83339", "d8a62ccdc3df3e002dbac55390772424b136844a"))
})

#  FUNCTION: gh_compare_files -----------------------------------------------------------------
test_that("gh_compare_files returns a tibble of information of file differences between commits", {
  comparison <- suppressWarnings(gh_compare_files(
    "d9fe50f8e31d7430df2c5b02442dffb68c854f08",
    "d8a62ccdc3df3e002dbac55390772424b136844a",
    "ChadGoymer/githapi"))

  expect_is(comparison, "tbl")

  expect_identical(
    sapply(comparison, function(field) class(field)[[1]]),
    c(filename     = "character",
      status       = "character",
      additions    = "integer",
      deletions    = "integer",
      changes      = "integer",
      contents_url = "character"))

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
  readme_d9fe50f <- suppressWarnings(
    gh_readme("d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi"))

  expect_true(is_scalar_character(readme_d9fe50f))
  expect_identical(
    readme_d9fe50f,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_contents ----------------------------------------------------------------------
test_that("gh_contents returns the text in a specified file", {
  description_master <- suppressWarnings(
    gh_contents("DESCRIPTION", "master", "ChadGoymer/githapi"))

  expect_true(is_scalar_character(description_master))

  readme_d9fe50f <- suppressWarnings(
    gh_contents("README.md", "d9fe50f8e31d7430df2c5b02442dffb68c854f08", "ChadGoymer/githapi"))

  expect_identical(
    readme_d9fe50f,
    "# githapi\nUser-friendly access to the GitHub API for R, consistent with the tidyverse.\n")
})

#  FUNCTION: gh_download ----------------------------------------------------------------------
test_that("gh_download saves the contents of a commit to the specified location", {
  temp_path <- file.path(tempdir(), "gh_download")
  on.exit(unlink(temp_path, recursive = TRUE))
  suppressWarnings(gh_download("master", "ChadGoymer/githapi", temp_path))

  expect_true(file.exists(file.path(temp_path, "DESCRIPTION")))
  expect_true(dir.exists(file.path(temp_path, "R")))
})

#  FUNCTION: is_collaborator ------------------------------------------------------------------
test_that("is_collaborator return TRUE if the user is a collaborator, FALSE otherwise", {
  expect_true(suppressWarnings(is_collaborator("ChadGoymer", "ChadGoymer/githapi")))
  expect_false(suppressWarnings(is_collaborator("Batman", "ChadGoymer/githapi")))
})

#  FUNCTION: gh_collaborators -----------------------------------------------------------------
test_that("gh_collaborators returns a tibble describing the collaborators", {
  collaborators <- suppressWarnings(gh_collaborators("ChadGoymer/githapi"))
  expect_is(collaborators, "tbl")

  expect_identical(
    sapply(collaborators, function(field) class(field)[[1]]),
    c(id                = "integer",
      login             = "character",
      type              = "character",
      site_admin        = "logical",
      permissions_admin = "logical",
      permissions_push  = "logical",
      permissions_pull  = "logical",
      url               = "character"))

  expect_true("ChadGoymer" %in% collaborators$login)
})

#  FUNCTION: gh_permissions -------------------------------------------------------------------
test_that("gh_permissions returns a list describing the user's permissions", {
  permissions <- suppressWarnings(gh_permissions("ChadGoymer", "ChadGoymer/githapi"))

  expect_is(permissions, "list")
  expect_named(permissions, c("permission", "user"))
  expect_identical(permissions$permission, "admin")
  expect_identical(permissions$user$login, "ChadGoymer")
})

#  FUNCTION: gh_commit_comment ----------------------------------------------------------------
test_that("gh_commit_comment returns a list describing the commit comment", {
  comment <- suppressWarnings(gh_commit_comment(24028377, "ChadGoymer/githapi"))
  expect_is(comment, "list")
  expect_named(
    comment,
    c("url", "html_url", "id", "node_id", "user", "position", "line", "path", "commit_id",
      "created_at", "updated_at", "author_association", "body"))
  expect_identical(comment$body, "Wow, This is a cool commit!")
})

#  FUNCTION: gh_commit_comments ---------------------------------------------------------------
test_that("gh_commit_comments returns a tibble describing all the commit comments", {
  repo_comments <- suppressWarnings(gh_commit_comments("ChadGoymer/githapi"))
  expect_is(repo_comments, "tbl")
  expect_identical(
    sapply(repo_comments, function(field) class(field)[[1]]),
    c(id         = "integer",
      commit_id  = "character",
      body       = "character",
      user_login = "character",
      created_at = "POSIXct",
      updated_at = "POSIXct",
      position   = "character",
      line       = "character",
      path       = "character",
      url        = "character"))
  expect_true("Wow, This is a cool commit!" %in% repo_comments$body)

  commit_comments <- suppressWarnings(gh_commit_comments("ChadGoymer/githapi", "d378328243626794ca725946c4e0662622aeb933"))
  expect_is(commit_comments, "tbl")
  expect_named(
    commit_comments,
    c("id", "commit_id", "body", "user_login", "created_at",
      "updated_at", "position", "line", "path", "url"))
  expect_true("Wow, This is a cool commit!" %in% commit_comments$body)
})

#  FUNCTION: gh_contributers ------------------------------------------------------------------
test_that("gh_contributers returns a tibble describing the contributers", {
  contributers <- suppressWarnings(gh_contributers("ChadGoymer/githapi"))
  expect_is(contributers, "tbl")
  expect_identical(
    sapply(contributers, function(field) class(field)[[1]]),
    c(id            = "integer",
      login         = "character",
      contributions = "integer",
      type          = "character",
      site_admin    = "logical",
      url           = "character"))
  expect_true("ChadGoymer" %in% contributers$login)
})

#  FUNCTION: gh_languages ---------------------------------------------------------------------
test_that("gh_languages returns a tibble describing the languages", {
  languages <- suppressWarnings(gh_languages("ChadGoymer/githapi"))
  expect_is(languages, "list")
  expect_named(languages, "R")
})

#  FUNCTION: gh_releases ----------------------------------------------------------------------
test_that("gh_releases returns a tibble describing the releases", {
  releases <- suppressWarnings(gh_releases("ChadGoymer/githapi"))
  expect_is(releases, "tbl")
  expect_identical(
    sapply(releases, function(field) class(field)[[1]]),
    c(id               = "integer",
      tag_name         = "character",
      name             = "character",
      body             = "character",
      author_login     = "character",
      draft            = "logical",
      prerelease       = "logical",
      target_commitish = "character",
      created_at       = "POSIXct",
      published_at     = "POSIXct",
      assets           = "list",
      zipball_url      = "character",
      url              = "character"))
  expect_true("v0.1.0" %in% releases$tag_name)
})

#  FUNCTION: gh_release -----------------------------------------------------------------------
test_that("gh_release returns a list describing the release", {
  release_0.1.0 <- suppressWarnings(gh_release("v0.1.0", "ChadGoymer/githapi"))
  expect_is(release_0.1.0, "list")
  expect_named(
    release_0.1.0,
    c("url", "assets_url", "upload_url", "html_url", "id", "node_id", "tag_name",
      "target_commitish", "name", "draft", "author", "prerelease", "created_at", "published_at",
      "assets", "tarball_url", "zipball_url", "body"))
  expect_identical(release_0.1.0$tag_name, "v0.1.0")

  release_by_id <- suppressWarnings(gh_release(7210389, "ChadGoymer/githapi"))
  expect_identical(release_by_id, release_0.1.0)

  release_latest <- suppressWarnings(gh_release(repo = "ChadGoymer/githapi"))
  expect_is(release_latest, "list")
  expect_true(as.POSIXct(release_0.1.0$created_at) < as.POSIXct(release_latest$created_at))
})

#  FUNCTION: gh_asset -------------------------------------------------------------------------
test_that("gh_asset returns a list describing the release asset", {
  asset <- suppressWarnings(gh_asset(4759932, "ChadGoymer/githapi"))
  expect_is(asset, "list")
  expect_named(
    asset,
    c("url", "id", "node_id", "name", "label", "uploader", "content_type", "state", "size",
      "download_count", "created_at", "updated_at", "browser_download_url"))
  expect_identical(asset$name, "githapi-v0.3.0.zip")
})

#  FUNCTION: gh_assets ------------------------------------------------------------------------
test_that("gh_assets returns a tibble describing the assets for a release", {
    assets <- suppressWarnings(gh_assets(7657161, "ChadGoymer/githapi"))
    expect_is(assets, "tbl")
    expect_identical(
      sapply(assets, function(field) class(field)[[1]]),
      c(id             = "integer",
        name           = "character",
        label          = "character",
        content_type   = "character",
        state          = "character",
        size           = "integer",
        download_count = "integer",
        created_at     = "POSIXct",
        updated_at     = "POSIXct",
        uploader_login = "character",
        url            = "character"))
    expect_true("githapi-v0.3.0.zip" %in% assets$name)
})
