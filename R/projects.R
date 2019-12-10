#  FUNCTION: create_project -------------------------------------------------------------------
#
#' Create a GitHub project
#'
#' This function creates a new project in GitHub. The project will be empty so you will need
#' to add columns and cards separately.
#'
#' You can create a project associated with either a repository or organisation, by
#' supplying them as an input, as long as you have appropriate permissions. If no repository
#' or organisation is specified the project is created for the authenticated user.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/#create-a-repository-project>
#' - <https://developer.github.com/v3/projects/#create-an-organization-project>
#' - <https://developer.github.com/v3/projects/#create-a-user-project>
#'
#' @param name (string) The name of the project.
#' @param body (string) The description of the project.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_project()` returns a list of the project properties.
#'
#' **Project Properties:**
#'
#' - **id**: The ID of the project.
#' - **number**: The number of the project for the repository, user or organisation.
#' - **name**: The name given to the project.
#' - **body**: The description given to the project.
#' - **state**: Whether the project is "open" or "closed".
#' - **creator**: The user who created the project.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL to view the project.
#'
#' @examples
#' \dontrun{
#'   create_project(
#'     name = "Repo project",
#'     body = "This is a repository's project",
#'     repo = "ChadGoymer/test-githapi")
#'
#'   create_project(
#'     name = "User project",
#'     body = "This is a user's project")
#' }
#'
#' @export
#'
create_project <- function(
  name,
  body,
  repo,
  org,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
  assert(is_scalar_character(body), "'body' must be a string:\n  ", body)

  payload <- list(name = name, body = body)

  if (!missing(repo))
  {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
    url <- gh_url("repos", repo, "projects")
  }
  else if (!missing(org))
  {
    assert(is_scalar_character(org), "'org' must be a string:\n  ", org)
    url <- gh_url("orgs", org, "projects")
  }
  else
  {
    url <- gh_url("user/projects")
  }

  info("Posting project '", name, "'")
  project_lst <- gh_request(
    url     = url,
    type    = "POST",
    payload = payload,
    accept  = "application/vnd.github.inertia-preview+json",
    ...)

  info("Transforming results", level = 4)
  project_tbl <- select_properties(project_lst, properties$project)

  project_gh <- structure(
    project_tbl,
    class   = c("github", class(project_tbl)),
    url     = attr(project_lst, "url"),
    request = attr(project_lst, "request"),
    status  = attr(project_lst, "status"),
    header  = attr(project_lst, "header"))

  info("Done", level = 7)
  project_gh
}
