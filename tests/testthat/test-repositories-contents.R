context("repositories contents")

# TEST: view_file, create_file, update_file & delete_file -------------------------------------

test_that("view_files, create_files, update_files and delete files on the default branch works", {
  all_files <- view_files("ChadGoymer/test-githapi")

  expect_is(all_files, "tbl")
  expect_identical(
    sapply(all_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character"))
  expect_true("README.md" %in% all_files$name)
  expect_identical(filter(all_files, name == "README.md") %>% pull(type), "file")

  created_files <- create_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = c("aaaa.txt", "bbbb.txt"),
    contents = c("Created to test:\n\n  `create_files()`", "Created to test:\n\n  `create_files()`"),
    messages = "Testing create_files()")

  # TODO: Add a test to check contents

  expect_is(created_files, "tbl")
  expect_identical(
    sapply(created_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(created_files$name, c("aaaa.txt", "bbbb.txt"))
  expect_identical(
    created_files$commit_message,
    c("Testing create_files() - added aaaa.txt", "Testing create_files() - added bbbb.txt"))

  viewed_files <- view_files("ChadGoymer/test-githapi", c("aaaa.txt", "bbbb.txt"))

  expect_is(viewed_files, "tbl")
  expect_identical(
    sapply(viewed_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character"))
  expect_identical(viewed_files$name, c("aaaa.txt", "bbbb.txt"))
  expect_identical(viewed_files$type, c("file", "file"))

  updated_files <- update_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = c("aaaa.txt", "bbbb.txt"),
    contents = c("Updated to test:\n\n  `update_files()`", "Updated to test:\n\n  `update_files()`"),
    messages = "Testing update_files()")

  expect_is(updated_files, "tbl")
  expect_identical(
    sapply(updated_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(updated_files$name, c("aaaa.txt", "bbbb.txt"))
  expect_identical(
    updated_files$commit_message,
    c("Testing update_files() - updated aaaa.txt", "Testing update_files() - updated bbbb.txt"))

  deleted_files <- delete_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = c("aaaa.txt", "bbbb.txt"),
    messages = "Testing delete_files()")

  expect_is(deleted_files, "tbl")
  expect_identical(
    sapply(deleted_files, function(col) class(col)[[1]]),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(
    deleted_files$commit_message,
    c("Testing delete_files() - deleted aaaa.txt", "Testing delete_files() - deleted bbbb.txt"))
  expect_error(view_files("ChadGoymer/test-githapi", "aaaa.txt"), "Not Found")
  expect_error(view_files("ChadGoymer/test-githapi", "bbbb.txt"), "Not Found")
})

test_that("view_files, create_files, update_files and delete files in the specified directory works", {
  created_files <- create_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "test-dir/aaaa.txt",
    contents = "Created to test:\n\n  `create_files()`",
    messages = "Testing create_files()")

  expect_is(created_files, "tbl")
  expect_identical(
    sapply(created_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(created_files$commit_message, "Testing create_files() - added test-dir/aaaa.txt")

  viewed_files <- view_files("ChadGoymer/test-githapi", "test-dir/aaaa.txt")

  expect_is(viewed_files, "tbl")
  expect_identical(
    sapply(viewed_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character"))
  expect_identical(viewed_files$path, "test-dir/aaaa.txt")

  updated_files <- update_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "test-dir/aaaa.txt",
    contents = "Updated to test:\n\n  `update_files()`",
    messages = "Testing update_files()")

  expect_is(updated_files, "tbl")
  expect_identical(
    sapply(updated_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(updated_files$commit_message, "Testing update_files() - updated test-dir/aaaa.txt")

  deleted_files <- delete_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "test-dir/aaaa.txt",
    messages = "Testing delete_files()")

  expect_is(deleted_files, "tbl")
  expect_identical(
    sapply(deleted_files, function(col) class(col)[[1]]),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(deleted_files$commit_message, "Testing delete_files() - deleted test-dir/aaaa.txt")
  expect_error(view_files("ChadGoymer/test-githapi", "aaaa.txt"), "Not Found")
})

test_that("view_files, create_files, update_files and delete files on the specified branch works", {
  created_files <- create_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "aaaa.txt",
    contents = "Created to test:\n\n  `create_files()`",
    messages = "Testing create_files() on test-branch",
    branches = "test-branch")

  # TODO: check against commits for `test-branch`

  expect_is(created_files, "tbl")
  expect_identical(
    sapply(created_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(created_files$name, "aaaa.txt")

  viewed_files <- view_files("ChadGoymer/test-githapi", "aaaa.txt", ref = "test-branch")

  expect_is(viewed_files, "tbl")
  expect_identical(
    sapply(viewed_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character"))
  expect_identical(viewed_files$name, "aaaa.txt")

  updated_files <- update_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "aaaa.txt",
    contents = "Updated to test:\n\n  `update_files()`",
    messages = "Testing update_files()",
    branches = "test-branch")

  expect_is(updated_files, "tbl")
  expect_identical(
    sapply(updated_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(updated_files$commit_message, "Testing update_files() - updated aaaa.txt")

  deleted_files <- delete_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "aaaa.txt",
    messages = "Testing delete_files()",
    branches = "test-branch")

  expect_is(deleted_files, "tbl")
  expect_identical(
    sapply(deleted_files, function(col) class(col)[[1]]),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(deleted_files$commit_message, "Testing delete_files() - deleted aaaa.txt")
  expect_error(view_files("ChadGoymer/test-githapi", "aaaa.txt", ref = "test-branch"), "Not Found")
})

test_that("author and committer can be set when creating, updating and deleting files", {
  created_files <- create_files(
    repo      = "ChadGoymer/test-githapi",
    paths     = "aaaa.txt",
    contents  = "Created to test:\n\n  `create_files()`",
    messages  = "Testing create_files()",
    author    = list(name = "Bob Smith", email = "bob.smith@acme.com"),
    committer = list(name = "Jane Jones", email = "jane.jones@acme.com"))

  expect_is(created_files, "tbl")
  expect_identical(
    sapply(created_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(created_files$commit_author, "Bob Smith")
  expect_identical(created_files$commit_committer, "Jane Jones")

  updated_files <- update_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "aaaa.txt",
    contents = "Updated to test:\n\n  `update_files()`",
    messages = "Testing update_files()",
    author    = list(name = "Jane Jones", email = "jane.jones@acme.com"),
    committer = list(name = "Bob Smith", email = "bob.smith@acme.com"))

  expect_is(updated_files, "tbl")
  expect_identical(
    sapply(updated_files, function(col) class(col)[[1]]),
    c(name              = "character",
      path              = "character",
      sha               = "character",
      size              = "integer",
      type              = "character",
      url               = "character",
      html_url          = "character",
      git_url           = "character",
      download_url      = "character",
      commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(updated_files$commit_author, "Jane Jones")
  expect_identical(updated_files$commit_committer, "Bob Smith")

  deleted_files <- delete_files(
    repo     = "ChadGoymer/test-githapi",
    paths    = "aaaa.txt",
    messages = "Testing delete_files()",
    author    = list(name = "Jane Jones", email = "jane.jones@acme.com"),
    committer = list(name = "Bob Smith", email = "bob.smith@acme.com"))

  expect_is(deleted_files, "tbl")
  expect_identical(
    sapply(deleted_files, function(col) class(col)[[1]]),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "character",
      commit_parent_url = "character"))
  expect_identical(deleted_files$commit_author, "Jane Jones")
  expect_identical(deleted_files$commit_committer, "Bob Smith")
})
