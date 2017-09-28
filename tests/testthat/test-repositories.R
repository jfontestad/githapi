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
  temp_path <- file.path(tempdir(), "gh_download")
  on.exit(unlink(temp_path, recursive = TRUE))
  gh_download("master", "ChadGoymer/githapi", temp_path)

  expect_true(file.exists(file.path(temp_path, "DESCRIPTION")))
  expect_true(dir.exists(file.path(temp_path, "R")))
})

#  FUNCTION: is_collaborator ------------------------------------------------------------------
test_that("gh_collaborator return TRUE if the user is a collaborator, FALSE otherwise", {
  expect_true(is_collaborator("ChadGoymer", "ChadGoymer/githapi"))
  expect_false(is_collaborator("Batman", "ChadGoymer/githapi"))
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

#  FUNCTION: gh_commit_comment ----------------------------------------------------------------
test_that("gh_commit_comment returns a list describing the commit comment", {
  comment <- gh_commit_comment(24028377, "ChadGoymer/githapi")
  expect_is(comment, "list")
  expect_identical(
    names(comment),
    c("url", "html_url", "id", "user", "position", "line", "path", "commit_id", "created_at",
      "updated_at", "author_association", "body"))
  expect_identical(comment$body, "Wow, This is a cool commit!")
})

#  FUNCTION: gh_commit_comments ---------------------------------------------------------------
test_that("gh_commit_comments returns a tibble describing all the commit comments", {
  repo_comments <- gh_commit_comments("ChadGoymer/githapi")
  expect_is(repo_comments, "tbl")
  expect_identical(
    names(repo_comments),
    c("id", "commit_id", "body", "user_login", "created_at",
      "updated_at", "position", "line", "path", "url"))
  expect_true("Wow, This is a cool commit!" %in% repo_comments$body)

  commit_comments <- gh_commit_comments("ChadGoymer/githapi", "d378328243626794ca725946c4e0662622aeb933")
  expect_is(commit_comments, "tbl")
  expect_identical(
    names(commit_comments),
    c("id", "commit_id", "body", "user_login", "created_at",
      "updated_at", "position", "line", "path", "url"))
  expect_true("Wow, This is a cool commit!" %in% commit_comments$body)
})

#  FUNCTION: gh_contributers ------------------------------------------------------------------
test_that("gh_contributers returns a tibble describing the contributers", {
  contributers <- gh_contributers("ChadGoymer/githapi")
  expect_is(contributers, "tbl")
  expect_identical(
    names(contributers),
    c("id", "login", "contributions", "type", "site_admin", "url"))
  expect_true("ChadGoymer" %in% contributers$login)
})

#  FUNCTION: gh_languages ---------------------------------------------------------------------
test_that("gh_languages returns a tibble describing the languages", {
  languages <- gh_languages("ChadGoymer/githapi")
  expect_is(languages, "list")
  expect_identical(names(languages), "R")
})

#  FUNCTION: gh_releases ----------------------------------------------------------------------
test_that("gh_releases returns a tibble describing the releases", {
  releases <- gh_releases("ChadGoymer/githapi")
  expect_is(releases, "tbl")
  expect_identical(
    names(releases),
    c("id", "tag_name", "name", "body", "author_login", "draft", "prerelease",
      "target_commitish", "created_at", "published_at", "assets", "zipball_url", "url"))
  expect_true("v0.1.0" %in% releases$tag_name)
})

#  FUNCTION: gh_release -----------------------------------------------------------------------
test_that("gh_release returns a list describing the release", {
  release_0.1.0 <- gh_release("v0.1.0", "ChadGoymer/githapi")
  expect_is(release_0.1.0, "list")
  expect_identical(
    names(release_0.1.0),
    c("url", "assets_url", "upload_url", "html_url", "id", "tag_name", "target_commitish",
      "name", "draft", "author", "prerelease", "created_at", "published_at", "assets",
      "tarball_url", "zipball_url", "body"))
  expect_identical(release_0.1.0$tag_name, "v0.1.0")

  release_by_id <- gh_release(7210389, "ChadGoymer/githapi")
  expect_identical(release_by_id, release_0.1.0)

  release_latest <- gh_release(repo = "ChadGoymer/githapi")
  expect_is(release_latest, "list")
  expect_true(parse_datetime(release_0.1.0$created_at) < parse_datetime(release_latest$created_at))
})

#  FUNCTION: gh_asset -------------------------------------------------------------------------
test_that("gh_asset returns a list describing the release asset", {
  asset <- gh_asset(4759932, "ChadGoymer/githapi")
  expect_is(asset, "list")
  expect_identical(
    names(asset),
    c("url", "id", "name", "label", "uploader", "content_type", "state", "size",
      "download_count", "created_at", "updated_at", "browser_download_url"))
  expect_identical(asset$name, "githapi-v0.3.0.zip")
})

#  FUNCTION: gh_assets ------------------------------------------------------------------------
test_that("gh_assets returns a tibble describing the assets for a release", {
    assets <- gh_assets(7657161, "ChadGoymer/githapi")
    expect_is(assets, "tbl")
    expect_identical(
      names(assets),
      c("id", "name", "label", "content_type", "state", "size", "download_count",
        "created_at", "updated_at", "uploader_login", "url"))
    expect_true("githapi-v0.3.0.zip" %in% assets$name)
})
