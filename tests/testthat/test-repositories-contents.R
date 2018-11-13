context("repositories contents")

# TEST: view_readme ---------------------------------------------------------------------------

test_that("view_readme returns the contents of the readme file", {
  readme <- view_readme("ChadGoymer/test-githapi")

  expect_true(is_string(readme))
  expect_match(readme, "^# test-githapi")

  readme_88f7769 <- view_readme("ChadGoymer/test-githapi", ref = "88f77699b1d592e1d5338f04a88ea1218975cba8")

  expect_true(is_string(readme_88f7769))
  expect_identical(readme_88f7769, "# test-githapi\nThis repo is used to test the githapi R package\n")
})

# TEST: view_contents -------------------------------------------------------------------------

test_that("view_contents returns a list of the file contents", {
  files <- view_contents("ChadGoymer/test-githapi", c("README.md", "test-file.txt"))

  expect_is(files, "character")
  expect_match(files["README.md"], "^# test-githapi")
  expect_match(files["test-file.txt"], "This is a test file.\n")

  files_dd72be1 <- view_contents(
    repo  = "ChadGoymer/test-githapi",
    paths = c("README.md", "test-file.txt"),
    ref   = "dd72be153e9edae67a659f1cb441f8dfe4486f1f")

  expect_is(files_dd72be1, "character")
  expect_identical(
    files_dd72be1["README.md"],
    c(README.md = "# test-githapi\nThis repo is used to test the githapi R package\n"))
  expect_identical(
    files_dd72be1["test-file.txt"],
    c(`test-file.txt` = "This is a test file.\n"))
})

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
  expect_identical(
    view_contents("ChadGoymer/test-githapi", c("aaaa.txt", "bbbb.txt")),
    c(aaaa.txt = "Created to test:\n\n  `create_files()`", bbbb.txt = "Created to test:\n\n  `create_files()`"))

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
  expect_identical(
    view_contents("ChadGoymer/test-githapi", c("aaaa.txt", "bbbb.txt")),
    c(aaaa.txt = "Updated to test:\n\n  `update_files()`", bbbb.txt = "Updated to test:\n\n  `update_files()`"))

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

# TEST: download_commit -----------------------------------------------------------------------

test_that("download_commit saves all the files in a commit to a directory", {
  temp_path <- file.path(tempdir(), "test-repo", fsep = "\\")
  on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

  path <- download_commit("ChadGoymer/test-githapi", path = temp_path)

  expect_identical(path, temp_path)
  commit_files <- list.files(temp_path)
  expect_true(length(commit_files) >= 1L)
  expect_true("README.md" %in% commit_files)

  temp_path_dd72be1 <- file.path(tempdir(), "test-repo-dd72be1", fsep = "\\")
  on.exit(unlink(temp_path_dd72be1, recursive = TRUE), add = TRUE)

  path_dd72be1 <- download_commit(
    repo = "ChadGoymer/test-githapi",
    ref  = "dd72be153e9edae67a659f1cb441f8dfe4486f1f",
    path = temp_path_dd72be1)

  expect_identical(path_dd72be1, temp_path_dd72be1)
  expect_identical(list.files(temp_path_dd72be1), c("README.md", "test-file.txt"))
})
