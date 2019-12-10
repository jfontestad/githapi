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


#  FUNCTION: update_project -------------------------------------------------------------------
#
#' Update a GitHub project
#'
#' This function updates a project in GitHub. It can be used to change the name and body, but
#' can also be used to close the project or change permissions.
#'
#' You can update a project associated with either a repository, user or organisation, by
#' supplying them as an input, as long as you have appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/#update-a-project>
#'
#' @param project (integer or string) Either the project number or name.
#' @param name (string, optional) The new name for the project.
#' @param body (string, optional) The new description of the project.
#' @param state (string, optional) The new state of the project, either `"open"` or `"closed"`.
#' @param permission (string, optional) The new permissions for the project, either `"read"`,
#'   `"write"`, `"admin"` or `"none"`.
#' @param private (boolean, optional) Whether the project should be private.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_project()` returns a list of the project properties.
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
#'   update_project(
#'     project = "Repo project",
#'     name    = "Updated repo project",
#'     body    = "This is an updated repository's project",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   update_project(
#'     name  = "User project",
#'     state = "closed",
#'     user  = "ChadGoymer")
#' }
#'
#' @export
#'
update_project <- function(
  project,
  name,
  body,
  state,
  permission,
  private,
  repo,
  user,
  org,
  ...)
{
  payload <- list()

  if (!missing(name))
  {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload <- c(payload, name = name)
  }

  if (!missing(body))
  {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload <- c(payload, body = body)
  }

  if (!missing(state))
  {
    assert(
      is_scalar_character(state) && state %in% str_subset(values$project$state, "all", negate = TRUE),
      "'state' must be one of '", str_c(values$project$state, collapse = "', '"), "':\n  ", state)
    payload <- c(payload, state = state)
  }

  if (!missing(permission))
  {
    assert(
      is_scalar_character(permission) && permission %in% values$project$permission,
      "'permission' must be one of '", str_c(values$project$permission, collapse = "', '"), "':\n  ", permission)
    payload <- c(payload, organization_permission = permission)
  }

  if (!missing(private))
  {
    assert(is_scalar_logical(private), "'private' must be a boolean:\n  ", private)
    payload <- c(payload, private = private)
  }

  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org)

  info("Updating project '", project$name, "'")
  project_lst <- gh_url("projects", project$id) %>%
    gh_request(
      type    = "PATCH",
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
