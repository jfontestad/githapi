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


#  FUNCTION: update_column -------------------------------------------------------------------
#
#' Update a column in a GitHub project
#'
#' `update_column()` can be used to change the column name in a project in GitHub.
#' `move_column()` can be used to reorder the columns.
#'
#' You can update a column associated with either a repository, user or organisation, by
#' supplying them as an input, as long as you have appropriate permissions.
#'
#' You can move a column by either specifying the position, either `"first"` or `"last"`, or
#' by specifying another column to place it after.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/columns/#update-a-project-column>
#' - <https://developer.github.com/v3/projects/columns/#move-a-project-column>
#'
#' @param column (integer or string) Either the column number or name.
#' @param name (string, optional) The new name for the column.
#' @param position (string, optional) Either `"first"` or `"last"`.
#' @param after (integer or string) An ID or name of another column to place this one after.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_column()` returns a list of the column properties.
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
#'   # Update the name of a column in a repository project
#'   update_column(
#'     column  = "Test column",
#'     name    = "Updated test column",
#'     project = "Test project",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # Move a column to the first position in a user's project
#'   move_column(
#'     name     = "Test column",
#'     position = "first",
#'     user     = "ChadGoymer")
#'
#'   # Move a column after another on in an organisation's project
#'   move_column(
#'     name  = "Test column",
#'     after = "Test column 2",
#'     org   = "HairyCoos")
#' }
#'
#' @export
#'
update_column <- function(
  column,
  name,
  project,
  repo,
  user,
  org,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org)

  info("Updating column '", column$name, "' in project '", project, "'")
  column_lst <- gh_url("projects/columns", column$id) %>%
    gh_request(
      type    = "PATCH",
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


#  FUNCTION: move_column ----------------------------------------------------------------------
#
#' @rdname update_column
#' @export
#'
move_column <- function(
  column,
  position,
  after,
  project,
  repo,
  user,
  org,
  ...)
{
  if (!missing(position))
  {
    assert(
      is_scalar_character(position) && position %in% values$column$position,
      "'position' must be one of '", str_c(values$column$position, collapse = "', '"), "':\n  ", position)

    payload <- list(position = position)
  }
  else if (!missing(after))
  {
    after_column <- view_column(
      column  = after,
      project = project,
      repo    = repo,
      user    = user,
      org     = org)

    payload <- list(position = str_c("after:", after_column$id))
  }
  else
  {
    error("Either 'position' or 'after' must be supplied")
  }

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org)

  info("Moving column '", column$name, "' in project '", project, "'")
  response <- gh_url("projects/columns", column$id, "moves") %>%
    gh_request(
      type    = "POST",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...)

  info("Transforming results", level = 4)
  column_gh <- structure(
    column,
    class   = class(column),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header"))

  info("Done", level = 7)
  column_gh
}


#  FUNCTION: view_columns --------------------------------------------------------------------
#
#' View columns within a GitHub project
#'
#' `view_columns()` summarises columns in a table with the properties as columns and a row
#' for each column in the project. `view_column()` returns a list of all properties for a
#' single column.
#'
#' You can summarise all the columns of a project associated with either a repository, user
#' or organisation, by supplying them as an input.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/columns/#list-project-columns>
#' - <https://developer.github.com/v3/projects/columns/#get-a-project-column>
#'
#' @param column (integer or string) The number or name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_columns()` returns a tibble of column properties. `view_column()`
#'   returns a list of properties for a single column.
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
#'   # View columns in a repository project
#'   view_columns(
#'     project = "Test columns",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # View columns in a user's project
#'   view_columns(
#'     project = "Test columns",
#'     user    = "ChadGoymer")
#'
#'   # View columns in an organisation's project
#'   view_column(
#'     project = "Test columns",
#'     org     = "HairyCoos")
#'
#'   # View a column in a repository project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # View a column in a user's project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     user    = "ChadGoymer")
#'
#'   # View a column in an organisation's project
#'   view_column(
#'     column  = "Test column",
#'     project = "Test columns",
#'     org     = "HairyCoos")
#' }
#'
#' @export
#'
view_columns <- function(
  project,
  repo,
  user,
  org,
  n_max = 1000,
  ...)
{
  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...)

  info("Viewing columns in project '", project$name, "'")
  columns_lst <- gh_url("projects", project$id, "columns") %>%
    gh_page(
      accept = "application/vnd.github.inertia-preview+json",
      n_max  = n_max,
      ...)

  info("Transforming results", level = 4)
  columns_gh <- bind_properties(columns_lst, properties$column) %>%
    structure(
      class   = c("github", class(.)),
      url     = attr(columns_lst, "url"),
      request = attr(columns_lst, "request"),
      status  = attr(columns_lst, "status"),
      header  = attr(columns_lst, "header"))

  info("Done", level = 7)
  columns_gh
}


#  FUNCTION: view_column ---------------------------------------------------------------------
#
#' @rdname view_columns
#' @export
#'
view_column <- function(
  column,
  project,
  repo,
  user,
  org,
  ...)
{
  if (is_scalar_integerish(column))
  {
    property <- "id"
  }
  else if (is_scalar_character(column))
  {
    property <- "name"
  }
  else
  {
    error("'column' must be either an integer or a string:\n  ", column)
  }

  project <- view_project(
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...)

  info("Viewing column '", column, "' in project '", project$name, "'")
  column_lst <- gh_url("projects", project$id, "columns") %>%
    gh_find(
      property = property,
      value    = column,
      accept   = "application/vnd.github.inertia-preview+json",
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
