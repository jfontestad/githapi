context("repositories")


# SETUP ---------------------------------------------------------------------------------------

now <- format(Sys.time(), "%Y%m%d-%H%M%S")


# TEST: create_repository ---------------------------------------------------------------------

test_that("create_repository creates a repository and returns its properties", {

  user_repo <- create_repository(
    name        = paste0("user-repository-", now),
    description = "This is a user repository",
    homepage    = "https://user-repository.com",
    auto_init   = TRUE)

  expect_is(user_repo, "list")
  expect_identical(attr(user_repo, "status"), 201L)
  expect_identical(
    map_chr(user_repo, ~ class(.)[[1]]),
    c(id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      html_url           = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"))

  expect_identical(user_repo$full_name, paste0("ChadGoymer/user-repository-", now))
  expect_identical(user_repo$description, "This is a user repository")
  expect_identical(user_repo$homepage, "https://user-repository.com")


  org_repo <- create_repository(
    name        = paste0("org-repository-", now),
    org         = "HairyCoos",
    description = "This is an organization respository",
    homepage    = "https://org-repository.com")

  expect_is(org_repo, "list")
  expect_identical(attr(org_repo, "status"), 201L)
  expect_identical(
    map_chr(org_repo, ~ class(.)[[1]]),
    c(id                 = "integer",
      name               = "character",
      full_name          = "character",
      description        = "character",
      owner              = "character",
      html_url           = "character",
      homepage           = "character",
      language           = "character",
      size               = "numeric",
      default_branch     = "character",
      permission         = "character",
      private            = "logical",
      has_issues         = "logical",
      has_projects       = "logical",
      has_wiki           = "logical",
      has_pages          = "logical",
      has_downloads      = "logical",
      allow_squash_merge = "logical",
      allow_merge_commit = "logical",
      allow_rebase_merge = "logical",
      fork               = "logical",
      archived           = "logical",
      disabled           = "logical",
      pushed_at          = "POSIXct",
      created_at         = "POSIXct",
      updated_at         = "POSIXct"))

  expect_identical(org_repo$full_name, paste0("HairyCoos/org-repository-", now))
  expect_identical(org_repo$description, "This is an organization respository")
  expect_identical(org_repo$homepage, "https://org-repository.com")

})
