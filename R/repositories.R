#  FUNCTION: create_repository -------------------------------------------------------------
#
#' Create a repository for a user or organization
#'
#' This function creates a new repository for the specified user or organization in GitHub.
#' It can also be used to specify whether the project is private or has issues, projects or
#' a wiki and can define the allowed behaviour when merging pull requests.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/#create>
#'
#' @param name (string) The name of the repository.
#' @param org (string, optional) The name of the organization.
#' @param description (string, optional) A short description of the repository.
#' @param homepage (string, optional) A URL with more information about the repository.
#' @param private (boolean, optional) Whether the repository is private or public.
#'   Default: `FALSE`.
#' @param has_issues (boolean, optional) Whether to enable issues for the repository.
#'   Default: `TRUE`.
#' @param has_projects (boolean, optional) Whether to enable projects for the repository.
#'   Default: `TRUE`.
#' @param has_wiki (boolean, optional) Whether to enable the wiki for the repository.
#'   Default: `TRUE`.
#' @param auto_init (boolean, optional) Whether to create an initial commit with empty README.
#'   Default: `FALSE`.
#' @param allow_squash_merge (boolean, optional) Whether to allow squash-merging pull requests.
#'   Default: `TRUE`.
#' @param allow_merge_commit (boolean, optional) Whether to allow merging pull requests with a
#'   merge commit. Default: `TRUE`.
#' @param allow_rebase_merge (boolean, optional) Whether to allow rebase-merging pull requests.
#'   Default: `TRUE`.
#' @param delete_branch_on_merge (boolean, optional) Whether to allow automatically deleting
#'   branches when pull requests are merged. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_repository()` returns a list of the repository's properties.
#'
#' **Repository Properties:**
#'
#' - **id**: The ID of the repository.
#' - **name**: The name of the repository.
#' - **full_name**: The full name of the repository, in the format: `owner/repo`.
#' - **description**: The description of the repository.
#' - **owner**: The owner of the repository.
#' - **html_url**: The address of the repository's web page in GitHub.
#' - **homepage**: The homepage for the repository.
#' - **language**: The dominant programming language in the repository.
#' - **size**: The overall size of the repository in bytes.
#' - **default_branch**: The name of the default branch.
#' - **permission**: The permission the authenticated user has.
#' - **private**: Whether the repository is private.
#' - **has_issues**: Whether the repository has issues.
#' - **has_projects**: Whether the repository has projects.
#' - **has_wiki**: Whether the repository has a wiki.
#' - **has_pages**: Whether the repository has GitHub Pages.
#' - **has_downloads**: Whether the repository has downloads.
#' - **allow_squash_merge**: Whether the repository allows squash-merging pull requests.
#' - **allow_merge_commit**: Whether the repository allows merging pull requests with a merge
#'   commit.
#' - **allow_rebase_merge**: Whether the repository allows rebase-merging pull requests.
#' - **fork**: Whether the repository is a fork of another.
#' - **archived**: Whether the repository has been archived.
#' - **disabled**: Whether the repository has been disabled.
#' - **pushed_at**: When the repository was last pushed to.
#' - **created_at**: When the repository was created.
#' - **updated_at**: When the repository was last updated.
#'
#' @examples
#' \dontrun{
#'   create_repository(
#'     name        = "user-repository",
#'     description = "This is a user respository",
#'     homepage    = "https://user-repository.com")
#'
#'   create_repository(
#'     name        = "org-repository",
#'     org         = "HairyCoos",
#'     description = "This is a organization respository",
#'     homepage    = "https://org-repository.com")
#' }
#'
#' @export
#'
create_repository <- function(
  name,
  org,
  description,
  homepage,
  private                = FALSE,
  has_issues             = TRUE,
  has_projects           = TRUE,
  has_wiki               = TRUE,
  auto_init              = FALSE,
  allow_squash_merge     = TRUE,
  allow_merge_commit     = TRUE,
  allow_rebase_merge     = TRUE,
  delete_branch_on_merge = FALSE,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
  assert(is_scalar_logical(private), "'private' must be a boolean:\n  ", private)
  assert(is_scalar_logical(has_issues), "'has_issues' must be a boolean:\n  ", has_issues)
  assert(is_scalar_logical(has_projects), "'has_projects' must be a boolean:\n  ", has_projects)
  assert(is_scalar_logical(has_wiki), "'has_wiki' must be a boolean:\n  ", has_wiki)
  assert(is_scalar_logical(auto_init), "'auto_init' must be a boolean:\n  ", auto_init)
  assert(is_scalar_logical(allow_squash_merge), "'allow_squash_merge' must be a boolean:\n  ", allow_squash_merge)
  assert(is_scalar_logical(allow_merge_commit), "'allow_merge_commit' must be a boolean:\n  ", allow_merge_commit)
  assert(is_scalar_logical(allow_rebase_merge), "'allow_rebase_merge' must be a boolean:\n  ", allow_rebase_merge)
  assert(is_scalar_logical(delete_branch_on_merge), "'delete_branch_on_merge' must be a boolean:\n  ", delete_branch_on_merge)

  payload <- list(
    name                   = name,
    private                = private,
    has_issues             = has_issues,
    has_projects           = has_projects,
    has_wiki               = has_wiki,
    auto_init              = auto_init,
    allow_squash_merge     = allow_squash_merge,
    allow_merge_commit     = allow_merge_commit,
    allow_rebase_merge     = allow_rebase_merge,
    delete_branch_on_merge = delete_branch_on_merge)

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  if (!missing(homepage)) {
    assert(is_scalar_character(homepage), "'description' must be a string:\n  ", homepage)
    payload$homepage <- homepage
  }

  if (!missing(org)) {
    assert(is_scalar_character(org), "'org' must be a string:\n  ", org)
    info("Creating repository '", name, "' for organization '", org, "'")
    url <- gh_url("orgs", org, "repos")
  }
  else {
    info("Creating repository '", name, "' for authenticated user")
    url <- gh_url("user/repos")
  }

  repo_lst <- gh_request("POST", url = url, payload = payload, ...)

  info("Transforming results", level = 4)
  repo_gh <- select_properties(repo_lst, properties$repository) %>%
    append(
      list(permission = values$repository$permission[max(which(as.logical(repo_lst$permissions[values$repository$permission])))]),
      after = which(names(.) == "default_branch"))

  info("Done", level = 7)
  structure(
    repo_gh,
    class   = class(repo_lst),
    url     = attr(repo_lst, "url"),
    request = attr(repo_lst, "request"),
    status  = attr(repo_lst, "status"),
    header  = attr(repo_lst, "header"))
}


#  FUNCTION: update_repository ----------------------------------------------------------------
#
#' Update a user or organization repository
#'
#' This function updates a repository for the specified user or organization in GitHub. It can
#' be used to change whether the project is private or has issues, projects or a wiki and can
#' redefine the allowed behaviour when merging pull requests.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/#edit>
#'
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the repository.
#' @param description (string, optional) A short description of the repository.
#' @param homepage (string, optional) A URL with more information about the repository.
#' @param private (boolean, optional) Whether the repository is private or public.
#' @param has_issues (boolean, optional) Whether to enable issues for the repository.
#' @param has_projects (boolean, optional) Whether to enable projects for the repository.
#' @param has_wiki (boolean, optional) Whether to enable the wiki for the repository.
#' @param default_branch (string, optional) The name of the default branch.
#' @param allow_squash_merge (boolean, optional) Whether to allow squash-merging pull requests.
#' @param allow_merge_commit (boolean, optional) Whether to allow merging pull requests with a
#'   merge commit.
#' @param allow_rebase_merge (boolean, optional) Whether to allow rebase-merging pull requests.
#' @param delete_branch_on_merge (boolean, optional) Whether to allow automatically deleting
#'   branches when pull requests are merged.
#' @param archived (boolean, optional) Whether to archive the repository.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_repository()` returns a list of the repository properties.
#'
#' **Repository Properties:**
#'
#' - **id**: The ID of the repository.
#' - **name**: The name of the repository.
#' - **full_name**: The full name of the repository, in the format: `owner/repo`.
#' - **description**: The description of the repository.
#' - **owner**: The owner of the repository.
#' - **html_url**: The address of the repository's web page in GitHub.
#' - **homepage**: The homepage for the repository.
#' - **language**: The dominant programming language in the repository.
#' - **size**: The overall size of the repository in bytes.
#' - **default_branch**: The name of the default branch.
#' - **permission**: The permission the authenticated user has.
#' - **private**: Whether the repository is private.
#' - **has_issues**: Whether the repository has issues.
#' - **has_projects**: Whether the repository has projects.
#' - **has_wiki**: Whether the repository has a wiki.
#' - **has_pages**: Whether the repository has GitHub Pages.
#' - **has_downloads**: Whether the repository has downloads.
#' - **allow_squash_merge**: Whether the repository allows squash-merging pull requests.
#' - **allow_merge_commit**: Whether the repository allows merging pull requests with a merge
#'   commit.
#' - **allow_rebase_merge**: Whether the repository allows rebase-merging pull requests.
#' - **fork**: Whether the repository is a fork of another.
#' - **archived**: Whether the repository has been archived.
#' - **disabled**: Whether the repository has been disabled.
#' - **pushed_at**: When the repository was last pushed to.
#' - **created_at**: When the repository was created.
#' - **updated_at**: When the repository was last updated.
#'
#' @examples
#' \dontrun{
#'   # Update a repository
#'   update_repository(
#'     repo           = "ChadGoymer/user-repository",
#'     name           = "updated-user-repository",
#'     description    = "This is an updated user respository",
#'     homepage       = "https://updated-user-repository.com",
#'     has_issues     = FALSE,
#'     has_projects   = FALSE,
#'     has_wiki       = FALSE,
#'     default_branch = "master")
#'
#'   # Update an organization's repository
#'   update_repository(
#'     repo                   = "HairyCoos/org-repository",
#'     name                   = "updated-org-repository",
#'     description            = "This is an updated organization respository",
#'     homepage               = "https://updated-org-repository.com",
#'     private                = FALSE,
#'     allow_squash_merge     = FALSE,
#'     allow_merge_commit     = FALSE,
#'     allow_rebase_merge     = TRUE,
#'     delete_branch_on_merge = TRUE)
#'
#'   # Archive a repository
#'   update_repository("HairyCoos/org-repository", archived = TRUE)
#' }
#'
#' @export
#'
update_repository <- function(
  repo,
  name,
  description,
  homepage,
  private,
  has_issues,
  has_projects,
  has_wiki,
  default_branch,
  allow_squash_merge,
  allow_merge_commit,
  allow_rebase_merge,
  delete_branch_on_merge,
  archived,
  ...)
{
  payload <- list()

  if (!missing(name)) {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload$name <- name
  }

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  if (!missing(homepage)) {
    assert(is_scalar_character(homepage), "'description' must be a string:\n  ", homepage)
    payload$homepage <- homepage
  }

  if (!missing(private)) {
    assert(is_scalar_logical(private), "'private' must be a boolean:\n  ", private)
    payload$private <- private
  }

  if (!missing(has_issues)) {
    assert(is_scalar_logical(has_issues), "'has_issues' must be a boolean:\n  ", has_issues)
    payload$has_issues <- has_issues
  }

  if (!missing(has_projects)) {
    assert(is_scalar_logical(has_projects), "'has_projects' must be a boolean:\n  ", has_projects)
    payload$has_projects <- has_projects
  }

  if (!missing(has_wiki)) {
    assert(is_scalar_logical(has_wiki), "'has_wiki' must be a boolean:\n  ", has_wiki)
    payload$has_wiki <- has_wiki
  }

  if (!missing(default_branch)) {
    assert(is_scalar_character(default_branch), "'default_branch' must be a string:\n  ", default_branch)
    payload$default_branch <- default_branch
  }

  if (!missing(allow_squash_merge)) {
    assert(is_scalar_logical(allow_squash_merge), "'allow_squash_merge' must be a boolean:\n  ", allow_squash_merge)
    payload$allow_squash_merge <- allow_squash_merge
  }

  if (!missing(allow_merge_commit)) {
    assert(is_scalar_logical(allow_merge_commit), "'allow_merge_commit' must be a boolean:\n  ", allow_merge_commit)
    payload$allow_merge_commit <- allow_merge_commit
  }

  if (!missing(allow_rebase_merge)) {
    assert(is_scalar_logical(allow_rebase_merge), "'allow_rebase_merge' must be a boolean:\n  ", allow_rebase_merge)
    payload$allow_rebase_merge <- allow_rebase_merge
  }

  if (!missing(delete_branch_on_merge)) {
    assert(is_scalar_logical(delete_branch_on_merge), "'delete_branch_on_merge' must be a boolean:\n  ", delete_branch_on_merge)
    payload$delete_branch_on_merge <- delete_branch_on_merge
  }

  if (!missing(archived)) {
    assert(is_scalar_logical(archived), "'archived' must be a boolean:\n  ", archived)
    payload$archived <- archived
  }

  info("Updating repository '", repo, "'")
  repo_lst <- gh_url("repos", repo) %>% gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  repo_gh <- select_properties(repo_lst, properties$repository) %>%
    append(
      list(permission = values$repository$permission[max(which(as.logical(repo_lst$permissions[values$repository$permission])))]),
      after = which(names(.) == "default_branch"))

  info("Done", level = 7)
  structure(
    repo_gh,
    class   = class(repo_lst),
    url     = attr(repo_lst, "url"),
    request = attr(repo_lst, "request"),
    status  = attr(repo_lst, "status"),
    header  = attr(repo_lst, "header"))
}
