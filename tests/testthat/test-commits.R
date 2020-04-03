context("commits")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")

setup(suppressMessages(try(silent = TRUE, {

  test_repo <- create_repository(
    name        = str_c("test-commits-", now),
    description = "This is a repository to test commits",
    auto_init   = TRUE)

})))

teardown(suppressMessages(try(silent = TRUE, {

  delete_repository(str_c("ChadGoymer/test-commits-", now))

})))


# TEST: upload_commit -------------------------------------------------------------------------

# TODO: Uncomment in version 1.0
# test_that("upload_commit uploads files in a directory and creates a commit", {
#
#   temp_path <- file.path(tempdir(), "test-upload-commit")
#   if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
#   dir.create(temp_path)
#   on.exit(unlink(temp_path))
#
#   flat_path <- file.path(temp_path, "flat")
#   dir.create(flat_path)
#
#   walk(str_c("file", 1:2, ".txt"), function(f) {
#     map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
#       writeLines(file.path(flat_path, f))
#   })
#
#
#   flat_commit <- upload_commit(
#     path    = flat_path,
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now))
#
#   expect_is(flat_commit, "list")
#   expect_identical(attr(flat_commit, "status"), 200L)
#   expect_identical(
#     map_chr(flat_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(flat_commit$message, "Commit to test upload_commit()")
#   expect_identical(flat_commit$author_name, "Chad Goymer")
#   expect_identical(flat_commit$author_email, "chad.goymer@gmail.com")
#   expect_identical(flat_commit$committer_name, "Chad Goymer")
#   expect_identical(flat_commit$committer_email, "chad.goymer@gmail.com")
#
#
#   recursive_path <- file.path(temp_path, "recursive")
#   recursive_file_paths <- file.path(recursive_path, c(
#     "dir-1/file-1.txt",
#     "dir-1/dir-1-1/file-1.txt",
#     "dir-1/dir-1-1/dir-1-1-1/file-2.txt",
#     "dir-1/dir-1-2/file-3.txt",
#     "dir-2/file-4.txt"))
#   file.path(recursive_path, c("dir-1/dir-1-1/dir-1-1-1", "dir-1/dir-1-2", "dir-2")) %>%
#     walk(dir.create, recursive = TRUE)
#
#   walk(recursive_file_paths, function(f) {
#     map_chr(1:10, ~sample(LETTERS, 10, replace = TRUE) %>% str_c(collapse = "")) %>%
#       writeLines(f)
#   })
#
#   recursive_commit <- upload_commit(
#     path    = recursive_path,
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now))
#
#   expect_is(recursive_commit, "list")
#   expect_identical(attr(recursive_commit, "status"), 200L)
#   expect_identical(
#     map_chr(recursive_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(recursive_commit$message, "Commit to test upload_commit()")
#
#
#   author_commit <- upload_commit(
#     path      = flat_path,
#     branch    = "master",
#     message   = "Commit to test upload_commit()",
#     repo      = str_c("ChadGoymer/test-commits-", now),
#     author    = list(name = "Bob",   email = "bob@acme.com"),
#     committer = list(name = "Jane",  email = "jane@acme.com"))
#
#   expect_is(author_commit, "list")
#   expect_identical(attr(author_commit, "status"), 200L)
#   expect_identical(
#     map_chr(author_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(author_commit$message, "Commit to test upload_commit()")
#   expect_identical(author_commit$author_name, "Bob")
#   expect_identical(author_commit$author_email, "bob@acme.com")
#   expect_identical(author_commit$committer_name, "Jane")
#   expect_identical(author_commit$committer_email, "jane@acme.com")
#
#
#   orphan_commit <- upload_commit(
#     path    = flat_path,
#     branch  = str_c("test-commits-1-", now),
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now))
#
#   expect_is(orphan_commit, "list")
#   expect_identical(attr(orphan_commit, "status"), 200L)
#   expect_identical(
#     map_chr(orphan_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(orphan_commit$message, "Commit to test upload_commit()")
#   expect_identical(orphan_commit$parents, character())
#
#
#   valid_parent_commit <- upload_commit(
#     path    = flat_path,
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now),
#     parents = author_commit$sha)
#
#   expect_is(valid_parent_commit, "list")
#   expect_identical(attr(valid_parent_commit, "status"), 200L)
#   expect_identical(
#     map_chr(valid_parent_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(valid_parent_commit$message, "Commit to test upload_commit()")
#   expect_identical(valid_parent_commit$parents, author_commit$sha)
#
#
#   expect_error(
#     upload_commit(
#       path    = flat_path,
#       branch  = "master",
#       message = "Commit to test upload_commit()",
#       repo    = str_c("ChadGoymer/test-commits-", now),
#       parents = flat_commit$sha),
#     "Update is not a fast forward")
#
#
#   force_parent_commit <- upload_commit(
#     path    = flat_path,
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now),
#     parents = flat_commit$sha,
#     force   = TRUE)
#
#   expect_is(force_parent_commit, "list")
#   expect_identical(attr(force_parent_commit, "status"), 200L)
#   expect_identical(
#     map_chr(force_parent_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(force_parent_commit$message, "Commit to test upload_commit()")
#   expect_identical(force_parent_commit$parents, flat_commit$sha)
#
#
#   new_branch_commit <- upload_commit(
#     path    = flat_path,
#     branch  = str_c("test-commits-2-", now),
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now),
#     parents = flat_commit$sha)
#
#   expect_is(new_branch_commit, "list")
#   expect_identical(attr(new_branch_commit, "status"), 200L)
#   expect_identical(
#     map_chr(new_branch_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(new_branch_commit$message, "Commit to test upload_commit()")
#   expect_identical(new_branch_commit$parents, flat_commit$sha)
#
#
#   merge_master_commit <- upload_commit(
#     path    = flat_path,
#     branch  = "master",
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now),
#     parents = c("master", new_branch_commit$sha))
#
#   expect_is(merge_master_commit, "list")
#   expect_identical(attr(merge_master_commit, "status"), 200L)
#   expect_identical(
#     map_chr(merge_master_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(merge_master_commit$message, "Commit to test upload_commit()")
#   expect_identical(merge_master_commit$parents, c(force_parent_commit$sha, new_branch_commit$sha))
#
#
#   merge_branch_commit <- upload_commit(
#     path    = flat_path,
#     branch  = str_c("test-commits-3-", now),
#     message = "Commit to test upload_commit()",
#     repo    = str_c("ChadGoymer/test-commits-", now),
#     parents = c("master", new_branch_commit$sha))
#
#   expect_is(merge_branch_commit, "list")
#   expect_identical(attr(merge_branch_commit, "status"), 200L)
#   expect_identical(
#     map_chr(merge_branch_commit, ~ class(.)[[1]]),
#     c(sha             = "character",
#       message         = "character",
#       author_login    = "character",
#       author_name     = "character",
#       author_email    = "character",
#       committer_login = "character",
#       committer_name  = "character",
#       committer_email = "character",
#       tree_sha        = "character",
#       parents         = "character",
#       date            = "POSIXct",
#       html_url        = "character"))
#
#   expect_identical(merge_branch_commit$message, "Commit to test upload_commit()")
#   expect_identical(merge_branch_commit$parents, c(merge_master_commit$sha, new_branch_commit$sha))
#
# })


# TEST: download_commit -----------------------------------------------------------------------

# TODO: Uncomment in version 1.0
# test_that("download_commit downloads a commit to the specified path", {
#
#   temp_path <- file.path(tempdir(), "test-download-commit") %>%
#     normalizePath(winslash = "/", mustWork = FALSE)
#   if (dir.exists(temp_path)) unlink(temp_path, recursive = TRUE)
#   on.exit(unlink(temp_path))
#
#   path <- download_commit(
#     ref  = "master",
#     repo = str_c("ChadGoymer/test-commits-", now),
#     path = temp_path)
#
#   expect_is(path, "character")
#   expect_identical(attr(path, "status"), 200L)
#   expect_identical(as.character(path), temp_path)
#
#   files <- list.files(temp_path)
#
#   expect_true(length(files) > 0)
#
# })
