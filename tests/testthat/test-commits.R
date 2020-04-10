context("commits")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages({

  test_repo <- create_repository(
    name        = str_c("test-commits-", now),
    description = "This is a repository to test commits",
    auto_init   = TRUE)

}))

teardown(suppressMessages({

  delete_repository(str_c("ChadGoymer/test-commits-", now))

}))


# TEST: .upload_commit -------------------------------------------------------------------------

test_that(".upload_commit uploads files in a directory and creates a commit", {

  temp_path <- file.path(tempdir(), "test-upload-commit")
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  dir.create(temp_path)
  on.exit(unlink(temp_path))

  flat_path <- file.path(temp_path, "flat")
  dir.create(flat_path)

  walk(str_c("file", 1:2, ".txt"), function(f) {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(file.path(flat_path, f))
  })


  flat_commit <- .upload_commit(
    path    = flat_path,
    branch  = "master",
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now))

  expect_is(flat_commit, "list")
  expect_identical(attr(flat_commit, "status"), 200L)
  expect_identical(
    map_chr(flat_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(flat_commit$message, "Commit to test .upload_commit()")
  expect_identical(flat_commit$author_name, "Chad Goymer")
  expect_identical(flat_commit$author_email, "chad.goymer@gmail.com")
  expect_identical(flat_commit$committer_name, "Chad Goymer")
  expect_identical(flat_commit$committer_email, "chad.goymer@gmail.com")


  recursive_path <- file.path(temp_path, "recursive")
  recursive_file_paths <- file.path(recursive_path, c(
    "dir-1/file-1.txt",
    "dir-1/dir-1-1/file-1.txt",
    "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
    "dir-1/dir-1-2/file-3.txt",
    "dir-2/file-4.txt"))
  file.path(recursive_path, c("dir-1/dir-1-1/dir-1-1-1", "dir-1/dir-1-2", "dir-2")) %>%
    walk(dir.create, recursive = TRUE)

  walk(recursive_file_paths, function(f) {
    map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
      writeLines(f)
  })

  recursive_commit <- .upload_commit(
    path    = recursive_path,
    branch  = "master",
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now))

  expect_is(recursive_commit, "list")
  expect_identical(attr(recursive_commit, "status"), 200L)
  expect_identical(
    map_chr(recursive_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(recursive_commit$message, "Commit to test .upload_commit()")


  author_commit <- .upload_commit(
    path      = flat_path,
    branch    = "master",
    message   = "Commit to test .upload_commit()",
    repo      = str_c("ChadGoymer/test-commits-", now),
    author    = list(name = "Bob",  email = "bob@acme.com"),
    committer = list(name = "Jane", email = "jane@acme.com"))

  expect_is(author_commit, "list")
  expect_identical(attr(author_commit, "status"), 200L)
  expect_identical(
    map_chr(author_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(author_commit$message, "Commit to test .upload_commit()")
  expect_identical(author_commit$author_name, "Bob")
  expect_identical(author_commit$author_email, "bob@acme.com")
  expect_identical(author_commit$committer_name, "Jane")
  expect_identical(author_commit$committer_email, "jane@acme.com")


  orphan_commit <- .upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-1-", now),
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now))

  expect_is(orphan_commit, "list")
  expect_identical(attr(orphan_commit, "status"), 200L)
  expect_identical(
    map_chr(orphan_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(orphan_commit$message, "Commit to test .upload_commit()")
  expect_identical(orphan_commit$parents, character())


  valid_parent_commit <- .upload_commit(
    path    = flat_path,
    branch  = "master",
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now),
    parents = author_commit$sha)

  expect_is(valid_parent_commit, "list")
  expect_identical(attr(valid_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(valid_parent_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(valid_parent_commit$message, "Commit to test .upload_commit()")
  expect_identical(valid_parent_commit$parents, author_commit$sha)


  expect_error(
    .upload_commit(
      path    = flat_path,
      branch  = "master",
      message = "Commit to test .upload_commit()",
      repo    = str_c("ChadGoymer/test-commits-", now),
      parents = flat_commit$sha),
    "Update is not a fast forward")


  force_parent_commit <- .upload_commit(
    path    = flat_path,
    branch  = "master",
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now),
    parents = flat_commit$sha,
    force   = TRUE)

  expect_is(force_parent_commit, "list")
  expect_identical(attr(force_parent_commit, "status"), 200L)
  expect_identical(
    map_chr(force_parent_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(force_parent_commit$message, "Commit to test .upload_commit()")
  expect_identical(force_parent_commit$parents, flat_commit$sha)


  new_branch_commit <- .upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-2-", now),
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now),
    parents = flat_commit$sha)

  expect_is(new_branch_commit, "list")
  expect_identical(attr(new_branch_commit, "status"), 200L)
  expect_identical(
    map_chr(new_branch_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(new_branch_commit$message, "Commit to test .upload_commit()")
  expect_identical(new_branch_commit$parents, flat_commit$sha)


  merge_master_commit <- .upload_commit(
    path    = flat_path,
    branch  = "master",
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now),
    parents = c("master", new_branch_commit$sha))

  expect_is(merge_master_commit, "list")
  expect_identical(attr(merge_master_commit, "status"), 200L)
  expect_identical(
    map_chr(merge_master_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(merge_master_commit$message, "Commit to test .upload_commit()")
  expect_identical(merge_master_commit$parents, c(force_parent_commit$sha, new_branch_commit$sha))


  merge_branch_commit <- .upload_commit(
    path    = flat_path,
    branch  = str_c("test-commits-3-", now),
    message = "Commit to test .upload_commit()",
    repo    = str_c("ChadGoymer/test-commits-", now),
    parents = c("master", new_branch_commit$sha))

  expect_is(merge_branch_commit, "list")
  expect_identical(attr(merge_branch_commit, "status"), 200L)
  expect_identical(
    map_chr(merge_branch_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(merge_branch_commit$message, "Commit to test .upload_commit()")
  expect_identical(merge_branch_commit$parents, c(merge_master_commit$sha, new_branch_commit$sha))

})


# TEST: .download_commit -----------------------------------------------------------------------

test_that(".download_commit downloads a commit to the specified path", {

  temp_path <- file.path(tempdir(), "test-download-commit") %>%
    normalizePath(winslash = "/", mustWork = FALSE)
  if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
  on.exit(unlink(temp_path))

  path <- .download_commit(
    ref  = "master",
    repo = str_c("ChadGoymer/test-commits-", now),
    path = temp_path)

  expect_is(path, "character")
  expect_identical(attr(path, "status"), 200L)
  expect_identical(as.character(path), temp_path)

  files <- list.files(temp_path)

  expect_true(length(files) > 0)

})


# TEST: .view_commits --------------------------------------------------------------------------

test_that(".view_commits returns a tibble of commit properties", {

  master_commits <- .view_commits("master", str_c("ChadGoymer/test-commits-", now))

  expect_is(master_commits, "tbl")
  expect_identical(attr(master_commits, "status"), 200L)
  expect_identical(
    map_chr(master_commits, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "list",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(last(master_commits$message), "Initial commit")

  readme_commits <- .view_commits(
    ref  = "master",
    repo = str_c("ChadGoymer/test-commits-", now),
    path = "README.md")

  expect_is(readme_commits, "tbl")
  expect_identical(attr(readme_commits, "status"), 200L)
  expect_identical(
    map_chr(readme_commits, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "list",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(last(readme_commits$message), "Initial commit")

  author_commits <- .view_commits(
    ref    = "master",
    repo   = str_c("ChadGoymer/test-commits-", now),
    author = "ChadGoymer")

  expect_is(author_commits, "tbl")
  expect_identical(attr(author_commits, "status"), 200L)
  expect_identical(
    map_chr(author_commits, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "list",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(last(author_commits$message), "Initial commit")

  time_commits <- .view_commits(
    ref   = "master",
    repo  = str_c("ChadGoymer/test-commits-", now),
    since = format(Sys.time() - 60*60*24, "%Y-%m-%d %H:%M:%S"),
    until = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  expect_is(time_commits, "tbl")
  expect_identical(attr(time_commits, "status"), 200L)
  expect_identical(
    map_chr(time_commits, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "list",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(last(time_commits$message), "Initial commit")

})


# TEST: view_commit ---------------------------------------------------------------------------

test_that("view_commit returns a list of commit properties", {

  master_commit <- view_commit("master", str_c("ChadGoymer/test-commits-", now))

  expect_is(master_commit, "list")
  expect_identical(attr(master_commit, "status"), 200L)
  expect_identical(
    map_chr(master_commit, ~ class(.)[[1]]),
    c(sha             = "character",
      message         = "character",
      author_login    = "character",
      author_name     = "character",
      author_email    = "character",
      committer_login = "character",
      committer_name  = "character",
      committer_email = "character",
      tree_sha        = "character",
      parents         = "character",
      date            = "POSIXct",
      html_url        = "character"))

  expect_identical(master_commit$author_login, "ChadGoymer")

})


# TEST: browse_commits ------------------------------------------------------------------------

test_that("browse_commits opens the commit's history page in the browser", {

  skip_if(!interactive(), "browse_commits must be tested manually")

  commits <- browse_commits("master", str_c("ChadGoymer/test-commits-", now))

  expect_is(commits, "character")
  expect_identical(attr(commits, "status"), 200L)
  expect_identical(
    dirname(commits),
    str_c("https://github.com/ChadGoymer/test-commits-", now, "/commits"))

})


# TEST: browse_commit -------------------------------------------------------------------------

test_that("browse_commit opens the commit's history page in the browser", {

  skip_if(!interactive(), "browse_commit must be tested manually")

  commit <- browse_commit("master", str_c("ChadGoymer/test-commits-", now))

  expect_is(commit, "character")
  expect_identical(attr(commit, "status"), 200L)
  expect_identical(
    dirname(commit),
    str_c("https://github.com/ChadGoymer/test-commits-", now, "/commit"))

})


# TEST: view_sha ------------------------------------------------------------------------------

test_that("view_sha returns the commit SHA given the reference", {

  master_sha <- view_sha("master", repo = str_c("ChadGoymer/test-commits-", now))

  expect_is(master_sha, "character")
  expect_identical(attr(master_sha, "status"), 200L)
  expect_true(is_sha(master_sha))

  tag <- create_tag(
    name = "test-commits",
    ref  = "master",
    repo = str_c("ChadGoymer/test-commits-", now))

  tag_sha <- view_sha("test-commits", repo = str_c("ChadGoymer/test-commits-", now))

  expect_is(tag_sha, "character")
  expect_identical(attr(tag_sha, "status"), 200L)
  expect_true(is_sha(tag_sha))
  expect_identical(as.character(master_sha), as.character(tag_sha))

})
