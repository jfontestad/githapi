#  FUNCTION: create_column -------------------------------------------------------------------
#
#' Create a column in a GitHub project
#'
#' This function creates a new column in a project in GitHub. The column will be empty so you
#' will need to add cards separately.
#'
#' You can create a column in a project associated with either a repository, user or
#' organisation, by supplying them as an input, as long as you have appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/columns/#create-a-project-column>
#'
#' @param name (string) The name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_column()` returns a list of the column properties.
#'
#' **column Properties:**
#'
#' - **id**: The ID of the column.
#' - **name**: The name given to the column.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'   # Create a column in a repository project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # Create a column in a user's project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     user    = "ChadGoymer")
#'
#'   # Create a column in an organisation's project
#'   create_column(
#'     name    = "Test column",
#'     project = "Test project",
#'     org     = "HairyCoos")
#' }
#'
#' @export
#'
create_column <- function(
  name,
  project,
  repo,
  user,
  org,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)

  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...)

  info("Creating column '", name, "' in project '", project$name, "'")
  column_lst <- gh_url("projects", project$id, "columns") %>%
    gh_request(
      type    = "POST",
      payload = list(name = name),
      accept  = "application/vnd.github.inertia-preview+json",
      ...)

  info("Transforming results", level = 4)
  column_gh <- select_properties(column_lst, properties$column) %>%
    structure(
      class   = class(column_lst),
      url     = attr(column_lst, "url"),
      request = attr(column_lst, "request"),
      status  = attr(column_lst, "status"),
      header  = attr(column_lst, "header"))

  info("Done", level = 7)
  column_gh
}
