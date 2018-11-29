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
