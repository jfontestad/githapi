context("git blobs")

# TEST: view_blobs ----------------------------------------------------------------------------

test_that("view_blobs returns information about files in the repository", {
  blobs <- view_blobs(
    repo = "ChadGoymer/test-githapi",
    shas = c("72b5faa9dc9e4bba87108bf302a5b453e985feec", "6de7b8c69d65923eb48b10a560f3d72939df256a"))

  expect_is(blobs, "tbl")
  expect_identical(
    map_vec(blobs, function(col) class(col)[[1]]),
    c(sha      = "character",
      content  = "character",
      encoding = "character",
      url      = "character",
      size     = "integer"))

  expect_identical(
    blobs$content,
    c("IyB0ZXN0LWdpdGhhcGkKVGhpcyByZXBvIGlzIHVzZWQgdG8gdGVzdCB0aGUg\nZ2l0aGFwaSBSIHBhY2thZ2UK\n",
      "VGhpcyBpcyBhIHRlc3QgZmlsZS4K\n"))
})

# TEST: create_blobs --------------------------------------------------------------------------

test_that("create_blobs creates files in the repository and returns the SHA", {
  random_contents <- c(
    paste(sample(LETTERS, 40, replace = TRUE), collapse = ""),
    paste(sample(LETTERS, 40, replace = TRUE), collapse = ""))

  blobs <- create_blobs(
    repo = "ChadGoymer/test-githapi",
    contents = random_contents)

  expect_is(blobs, "tbl")
  expect_identical(
    map_vec(blobs, function(col) class(col)[[1]]),
    c(sha      = "character",
      url      = "character"))
  expect_true(all(map_vec(blobs$sha, is_sha)))
})

# TEST: upload_blobs --------------------------------------------------------------------------

test_that("upload_blobs reads the specified files and uploads them to specified repository", {
  temp_file1 <- tempfile()
  temp_file2 <- tempfile()

  write("This is the first test file", temp_file1)
  write("This is the second test file", temp_file2)

  blobs <- upload_blobs(
    repo = "ChadGoymer/test-githapi",
    paths = c(temp_file1, temp_file2))

  expect_is(blobs, "tbl")
  expect_identical(
    map_vec(blobs, function(col) class(col)[[1]]),
    c(name = "character",
      sha  = "character",
      url  = "character"))
  expect_true(all(map_vec(blobs$sha, is_sha)))

  created_blobs <- view_blobs(
    repo = "ChadGoymer/test-githapi",
    shas = blobs$sha)

  expect_match(
    base64_dec(created_blobs$content[[1]]) %>% readBin("character"),
    "^This is the first test file")
  expect_match(
    base64_dec(created_blobs$content[[2]]) %>% readBin("character"),
    "^This is the second test file")
})
