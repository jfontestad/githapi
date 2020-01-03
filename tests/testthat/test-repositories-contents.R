context("repositories contents")

repo_contents_branch <- str_c("test-repo-contents-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
new_files_branch <- str_c("test-create-files-", format(Sys.time(), "%Y-%m-%d-%H-%M-%S"))
master_sha <- view_shas(refs = "unedited-contents", repo = "ChadGoymer/test-githapi")[[1]]

setup(suppressMessages({

  create_branches(
    branches = repo_contents_branch,
    shas     = master_sha,
    repo     = "ChadGoymer/test-githapi")

}))

teardown(suppressMessages(tryCatch({

  delete_branches(
    branches = c(repo_contents_branch, new_files_branch),
    repo     = "ChadGoymer/test-githapi")

  update_branches(
    branches = "master",
    shas     = master_sha,
    repo     = "ChadGoymer/test-githapi")

})))


# TEST: view_files, create_files, update_files & delete_files ---------------------------------

# test_that("view_files, create_files, update_files and delete files on the default branch works", {
#   all_files <- view_files("ChadGoymer/test-githapi")
#
#   expect_is(all_files, "tbl")
#   expect_identical(
#     gh_map(all_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character"))
#
#   expect_true("README.md" %in% all_files$name)
#   expect_identical(filter(all_files, name == "README.md") %>% pull(type), "file")
#
#   created_files <- create_files(
#     paths    = c("aaaa.txt", "bbbb.txt"),
#     contents = c("Created to test:\n\n  `create_files()`", "Created to test:\n\n  `create_files()`"),
#     messages = "Testing create_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(created_files, "tbl")
#   expect_identical(
#     gh_map(created_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character",
#       commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(created_files$name, c("aaaa.txt", "bbbb.txt"))
#   expect_identical(
#     created_files$commit_message,
#     c("Testing create_files()", "Testing create_files()"))
#
#   expect_identical(
#     read_files(c("aaaa.txt", "bbbb.txt"), "ChadGoymer/test-githapi"),
#     c(aaaa.txt = "Created to test:\n\n  `create_files()`", bbbb.txt = "Created to test:\n\n  `create_files()`"))
#
#   viewed_files <- view_files(c("aaaa.txt", "bbbb.txt"), "ChadGoymer/test-githapi")
#
#   expect_is(viewed_files, "tbl")
#   expect_identical(
#     gh_map(viewed_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character"))
#
#   expect_identical(viewed_files$name, c("aaaa.txt", "bbbb.txt"))
#   expect_identical(viewed_files$type, c("file", "file"))
#
#   updated_files <- update_files(
#     paths    = c("aaaa.txt", "bbbb.txt"),
#     contents = c("Updated to test:\n\n  `update_files()`", "Updated to test:\n\n  `update_files()`"),
#     messages = "Testing update_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(updated_files, "tbl")
#   expect_identical(
#     gh_map(updated_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character",
#       commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(updated_files$name, c("aaaa.txt", "bbbb.txt"))
#   expect_identical(
#     updated_files$commit_message,
#     c("Testing update_files() - updated aaaa.txt", "Testing update_files() - updated bbbb.txt"))
#
#   expect_identical(
#     read_files(c("aaaa.txt", "bbbb.txt"), "ChadGoymer/test-githapi"),
#     c(aaaa.txt = "Updated to test:\n\n  `update_files()`", bbbb.txt = "Updated to test:\n\n  `update_files()`"))
#
#   deleted_files <- delete_files(
#     paths    = c("aaaa.txt", "bbbb.txt"),
#     messages = "Testing delete_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(deleted_files, "tbl")
#   expect_identical(
#     gh_map(deleted_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(
#     deleted_files$commit_message,
#     c("Testing delete_files() - deleted aaaa.txt", "Testing delete_files() - deleted bbbb.txt"))
#
#   expect_error(suppressWarnings(view_files("aaaa.txt", "ChadGoymer/test-githapi")), "Not Found")
#   expect_error(suppressWarnings(view_files("bbbb.txt", "ChadGoymer/test-githapi")), "Not Found")
# })
#
# test_that("view_files, create_files, update_files and delete files in the specified directory works", {
#   created_files <- create_files(
#     paths    = "test-dir/aaaa.txt",
#     contents = "Created to test:\n\n  `create_files()`",
#     messages = "Testing create_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(created_files, "tbl")
#   expect_identical(
#     gh_map(created_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character",
#       commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(created_files$commit_message, "Testing create_files()")
#
#   viewed_files <- view_files("test-dir/aaaa.txt", "ChadGoymer/test-githapi")
#
#   expect_is(viewed_files, "tbl")
#   expect_identical(
#     gh_map(viewed_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character"))
#
#   expect_identical(viewed_files$path, "test-dir/aaaa.txt")
#
#   updated_files <- update_files(
#     paths    = "test-dir/aaaa.txt",
#     contents = "Updated to test:\n\n  `update_files()`",
#     messages = "Testing update_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(updated_files, "tbl")
#   expect_identical(
#     gh_map(updated_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(name              = "character",
#       path              = "character",
#       sha               = "character",
#       size              = "integer",
#       type              = "character",
#       url               = "character",
#       html_url          = "character",
#       git_url           = "character",
#       download_url      = "character",
#       commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(updated_files$commit_message, "Testing update_files() - updated test-dir/aaaa.txt")
#
#   deleted_files <- delete_files(
#     paths    = "test-dir/aaaa.txt",
#     messages = "Testing delete_files()",
#     repo     = "ChadGoymer/test-githapi")
#
#   expect_is(deleted_files, "tbl")
#   expect_identical(
#     gh_map(deleted_files, function(col) class(col)[[1]], simplify = TRUE),
#     c(commit_message    = "character",
#       commit_sha        = "character",
#       commit_url        = "character",
#       commit_author     = "character",
#       commit_committer  = "character",
#       commit_tree_sha   = "character",
#       commit_tree_url   = "character",
#       commit_parent_sha = "list",
#       commit_parent_url = "list"))
#
#   expect_identical(deleted_files$commit_message, "Testing delete_files() - deleted test-dir/aaaa.txt")
#
#   expect_error(suppressWarnings(view_files("aaaa.txt", "ChadGoymer/test-githapi")), "Not Found")
# })

test_that("view_files, create_files, update_files and delete files on the specified branch works", {
  created_files <- create_files(
    paths    = "aaaa.txt",
    contents = "Created to test:\n\n  `create_files()`",
    messages = "Testing create_files() on test branch",
    branches = repo_contents_branch,
    repo     = "ChadGoymer/test-githapi")

  expect_is(created_files, "tbl")
  expect_identical(
    gh_map(created_files, function(col) class(col)[[1]], simplify = TRUE),
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
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(created_files$name, "aaaa.txt")

  viewed_files <- view_files("aaaa.txt", "ChadGoymer/test-githapi", ref = repo_contents_branch)

  expect_is(viewed_files, "tbl")
  expect_identical(
    gh_map(viewed_files, function(col) class(col)[[1]], simplify = TRUE),
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
    paths    = "aaaa.txt",
    contents = "Updated to test:\n\n  `update_files()`",
    messages = "Testing update_files()",
    branches = repo_contents_branch,
    repo     = "ChadGoymer/test-githapi")

  expect_is(updated_files, "tbl")
  expect_identical(
    gh_map(updated_files, function(col) class(col)[[1]], simplify = TRUE),
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
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(updated_files$commit_message, "Testing update_files() - updated aaaa.txt")

  deleted_files <- delete_files(
    paths    = "aaaa.txt",
    messages = "Testing delete_files()",
    branches = repo_contents_branch,
    repo     = "ChadGoymer/test-githapi")

  expect_is(deleted_files, "tbl")
  expect_identical(
    gh_map(deleted_files, function(col) class(col)[[1]], simplify = TRUE),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(deleted_files$commit_message, "Testing delete_files() - deleted aaaa.txt")

  expect_error(suppressWarnings(view_files("aaaa.txt", "ChadGoymer/test-githapi", ref = repo_contents_branch)), "Not Found")
})

test_that("when creating files on a branch that does not exist, it is created", {
  created_files <- create_files(
    paths    = "aaaa.txt",
    contents = "Created to test:\n\n  `create_files()`",
    messages = "Testing create_files() on test branch",
    branches = new_files_branch,
    parent   = "master",
    repo     = "ChadGoymer/test-githapi")

  expect_is(created_files, "tbl")
  expect_identical(
    gh_map(created_files, function(col) class(col)[[1]], simplify = TRUE),
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
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(created_files$name, "aaaa.txt")

  viewed_files <- view_files("aaaa.txt", "ChadGoymer/test-githapi", ref = new_files_branch)

  expect_is(viewed_files, "tbl")
  expect_identical(
    gh_map(viewed_files, function(col) class(col)[[1]], simplify = TRUE),
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
})

test_that("author and committer can be set when creating, updating and deleting files", {
  created_files <- create_files(
    paths     = "aaaa.txt",
    contents  = "Created to test:\n\n  `create_files()`",
    messages  = "Testing create_files()",
    branches  = repo_contents_branch,
    author    = list(name = "Bob Smith", email = "bob.smith@acme.com"),
    committer = list(name = "Jane Jones", email = "jane.jones@acme.com"),
    repo      = "ChadGoymer/test-githapi")

  expect_is(created_files, "tbl")
  expect_identical(
    gh_map(created_files, function(col) class(col)[[1]], simplify = TRUE),
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
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(created_files$commit_author, "Bob Smith")
  expect_identical(created_files$commit_committer, "Jane Jones")

  updated_files <- update_files(
    paths     = "aaaa.txt",
    contents  = "Updated to test:\n\n  `update_files()`",
    messages  = "Testing update_files()",
    branches  = repo_contents_branch,
    author    = list(name = "Jane Jones", email = "jane.jones@acme.com"),
    committer = list(name = "Bob Smith", email = "bob.smith@acme.com"),
    repo      = "ChadGoymer/test-githapi")

  expect_is(updated_files, "tbl")
  expect_identical(
    gh_map(updated_files, function(col) class(col)[[1]], simplify = TRUE),
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
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(updated_files$commit_author, "Jane Jones")
  expect_identical(updated_files$commit_committer, "Bob Smith")

  deleted_files <- delete_files(
    paths     = "aaaa.txt",
    messages  = "Testing delete_files()",
    branches  = repo_contents_branch,
    author    = list(name = "Jane Jones", email = "jane.jones@acme.com"),
    committer = list(name = "Bob Smith", email = "bob.smith@acme.com"),
    repo      = "ChadGoymer/test-githapi")

  expect_is(deleted_files, "tbl")
  expect_identical(
    gh_map(deleted_files, function(col) class(col)[[1]], simplify = TRUE),
    c(commit_message    = "character",
      commit_sha        = "character",
      commit_url        = "character",
      commit_author     = "character",
      commit_committer  = "character",
      commit_tree_sha   = "character",
      commit_tree_url   = "character",
      commit_parent_sha = "list",
      commit_parent_url = "list"))

  expect_identical(deleted_files$commit_author, "Jane Jones")
  expect_identical(deleted_files$commit_committer, "Bob Smith")
})

# TEST: files_exist ---------------------------------------------------------------------------

test_that("files_exist returns TRUE or FALSE depending on whether the file exists in the repo", {

  expect_true(files_exist("README.md", "ChadGoymer/test-githapi"))
  expect_false(files_exist("no-such-file", "ChadGoymer/test-githapi"))

  expect_identical(
    files_exist(c("README.md", "no-such-file"), "ChadGoymer/test-githapi"),
    c(`README.md` = TRUE, `no-such-file` = FALSE))

})

# TEST: download_commit -----------------------------------------------------------------------

test_that("download_commit saves all the files in a commit to a directory", {
  temp_path <- file.path(tempdir(), "test-repo", fsep = "\\")
  on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

  path <- download_commit(temp_path, "ChadGoymer/test-githapi")

  expect_identical(path, temp_path)
  commit_files <- list.files(temp_path)
  expect_true(length(commit_files) >= 1L)
  expect_true("README.md" %in% commit_files)

  temp_path_dd72be1 <- file.path(tempdir(), "test-repo-dd72be1", fsep = "\\")
  on.exit(unlink(temp_path_dd72be1, recursive = TRUE), add = TRUE)

  path_dd72be1 <- download_commit(
    ref  = "dd72be153e9edae67a659f1cb441f8dfe4486f1f",
    path = temp_path_dd72be1,
    repo = "ChadGoymer/test-githapi")

  expect_identical(path_dd72be1, temp_path_dd72be1)
  expect_identical(list.files(temp_path_dd72be1), c("README.md", "test-file.txt"))
})
