#  FUNCTION: update_collaborator --------------------------------------------------------------
#
#' Update collaborators for a repository or project
#'
#' This function can be used to invite a user to collaborate on either a repository or project,
#' or update their permission within the repository or project.
#'
#' Note: you can only invite or update a user if the authenticate user is an "admin" of the
#' repository or project.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/collaborators/#add-user-as-a-collaborator>
#' - <https://developer.github.com/v3/projects/collaborators/#add-user-as-a-collaborator>
#'
#' @param user (string) The login of the user.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param project (integer or string, optional) Either the project number or name.
#' @param org (string, optional) The name of the organization. Only required for projects.
#' @param permission (string, optional) The permission to give the user. For a repository
#'   this is either `"pull"`, `"push"` or `"admin"`, for a project it is either `"read"`,
#'   `"write"` or `"admin"`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_collaborator()` returns `TRUE` if successful.
#'
#' @examples
#' \dontrun{
#'   # Invite a user to collaborate on a repository
#'   update_collaborator("ChadGoymer2", repo = "ChadGoymer/test-githapi")
#'
#'   # Invite a user to collaborate on a project
#'   update_collaborator(
#'     user    = "ChadGoymer2",
#'     project = "TestProject",
#'     org     = "HairyCoos")
#'
#'   # Update a user's permissions on a repository
#'   update_collaborator(
#'     user       = "ChadGoymer2",
#'     repo       = "ChadGoymer/test-githapi",
#'     permission = "admin")
#'
#'   # Update a user's permissions on a project
#'   update_collaborator(
#'     user       = "ChadGoymer2",
#'     project    = "TestProject",
#'     org        = "HairyCoos",
#'     permission = "read")
#' }
#'
#' @export
#'
update_collaborator <- function(
  user,
  repo,
  project,
  org,
  permission,
  ...)
{
  assert(is_scalar_character(user), "'user' must be a string:\n  ", user)

  payload <- NULL

  if (!missing(repo))
  {
    if (!missing(permission))
    {
      assert(
        is_scalar_character(permission) && permission %in% values$collaborators$repo_permission,
        "'permission' for repositories must be either '", paste(values$collaborators$repo_permission, collapse = "', '"), "':\n  ", permission)
      payload$permission <- permission
    }

    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
    info("Updating collaborator '", user, "' for repository '", repo, "'")
    response <- gh_url("repos", repo, "collaborators", user) %>%
      gh_request("PUT", payload = payload, ...)
  }
  else if (!missing(project))
  {
    if (!missing(permission))
    {
      assert(
        is_scalar_character(permission) && permission %in% values$collaborators$project_permission,
        "'permission' for projects must be either '", paste(values$collaborators$project_permission, collapse = "', '"), "':\n  ", permission)
      payload$permission <- permission
    }

    project <- view_project(project = project, org = org)

    info("Updating collaborator '", user, "' for project '", project$name, "'")
    response <- gh_url("projects", project$id, "collaborators", user) %>%
      gh_request(
        type    = "PUT",
        payload = payload,
        accept  = "application/vnd.github.inertia-preview+json",
        ...)
  }
  else
  {
    error("A 'repo' or 'project' must be specified when creating a collaborator")
  }

  info("Done", level = 7)
  structure(
    TRUE,
    class   = c("github", "logical"),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header"))
}


#  FUNCTION: view_collaborators ---------------------------------------------------------------
#
#' View collaborators in an organization, repository or project
#'
#' `view_collaborators()` summarises the collaborators in a repository, project or
#' organization in a table with the properties as columns and a row for each collaborator.
#' `view_collaborator()` returns a list of all collaborator's properties including their
#' permission.
#'
#' Note: you can only view collaborators in an organization if the authenticate user is also
#' a member.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
#' - <https://developer.github.com/v3/projects/collaborators/#list-collaborators>
#' - <https://developer.github.com/v3/orgs/outside_collaborators/#list-outside-collaborators>
#' - <https://developer.github.com/v3/repos/collaborators/#review-a-users-permission-level>
#' - <https://developer.github.com/v3/projects/collaborators/#review-a-users-permission-level>
#'
#' @param user (string) The login of the user.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param project (integer or string, optional) Either the project number or name.
#' @param org (string, optional) The name of the organization. Only required for projects.
#' @param affiliation (string, optional) Filter by the affiliation of the user. This is either
#'   `"outside"`, `"direct"` or `"all"`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_collaborators()` returns a tibble of collaborator properties.
#'   `view_collaborator()` returns a list of properties for a single collaborator.
#'
#' **Collaborator Properties:**
#'
#' - **id**: The ID of the collaborator.
#' - **login**: The login name of the collaborator.
#' - **site_admin**: Whether the collaborator is an administrator.
#' - **html_url**: The GitHub page for the collaborator.
#' - **permission**: The permission the collaborator has (only available in `view_collaborator()`).
#'
#' @examples
#' \dontrun{
#'   # View collaborators on a repository
#'   view_collaborators(repo = "ChadGoymer/test-githapi")
#'
#'   # View collaborators on a project
#'   view_collaborators(project = test_project$name, org = "HairyCoos")
#'
#'   # View collaborators in an organization
#'   view_collaborators(org = "HairyCoos")
#'
#'   # View collaborator on a repository
#'   view_collaborator("ChadGoymer", repo = "ChadGoymer/test-githapi")
#'
#'   # View collaborator on a project
#'   view_collaborator(
#'     user    = "ChadGoymer",
#'     project = "Test project",
#'     org     = "HairyCoos")
#' }
#'
#' @export
#'
view_collaborators <- function(
  repo,
  project,
  org,
  affiliation,
  n_max = 1000,
  ...)
{
  payload <- NULL
  if (!missing(affiliation))
  {
    assert(
      is_scalar_character(affiliation) && affiliation %in% values$collaborators$affiliation,
      "'affiliation' for repositories must be either '", paste(values$collaborators$repo_affiliation, collapse = "', '"), "':\n  ", affiliation)
    payload$affiliation <- affiliation
  }

  if (!missing(repo))
  {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
    info("Viewing collaborators for repository '", repo, "'")
    collaborators_lst <- gh_url("repos", repo, "collaborators") %>%
      gh_request("GET", payload = payload, ...)
  }
  else if (!missing(project))
  {
    project <- view_project(project = project, org = org)

    info("Viewing collaborators for project '", project$name, "'")
    collaborators_lst <- gh_url("projects", project$id, "collaborators") %>%
      gh_request(
        type    = "GET",
        payload = payload,
        accept  = "application/vnd.github.inertia-preview+json",
        ...)
  }
  else if (!missing(org))
  {
    assert(is_scalar_character(org), "'org' must be a string:\n  ", org)
    info("Viewing collaborators for organization '", org, "'")
    collaborators_lst <- gh_url("orgs", org, "outside_collaborators") %>%
      gh_request("GET", ...)
  }
  else
  {
    error("A 'repo', 'project' or 'org' must be specified when viewing collaborators")
  }

  info("Transforming results", level = 4)
  collaborators_gh <- bind_properties(collaborators_lst, properties$users)

  info("Done", level = 7)
  collaborators_gh
}


#  FUNCTION: view_collaborator -------------------------------------------------------------
#
#' @rdname view_collaborators
#' @export
#'
view_collaborator <- function(
  user,
  repo,
  project,
  org,
  ...)
{
  assert(is_scalar_character(user), "'user' must be a string:\n  ", user)

  if (!missing(repo))
  {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
    info("Viewing collaborator '", user, "' for repository '", repo, "'")
    collaborators_lst <- gh_url("repos", repo, "collaborators", user, "permission") %>%
      gh_request("GET", ...)
  }
  else if (!missing(project))
  {
    project <- view_project(project = project, org = org)

    info("Viewing collaborator '", user, "' for project '", project$name, "'")
    collaborators_lst <- gh_url("projects", project$id, "collaborators", user, "permission") %>%
      gh_request("GET", accept = "application/vnd.github.inertia-preview+json", ...)
  }
  else
  {
    error("A 'repo' or 'project' must be specified when viewing a collaborator")
  }

  info("Transforming results", level = 4)
  collaborators_gh <- select_properties(collaborators_lst$user, properties$users) %>%
    append(list(permission = collaborators_lst$permission)) %>%
    structure(
      class   = class(collaborators_lst),
      url     = attr(collaborators_lst, "url"),
      request = attr(collaborators_lst, "request"),
      status  = attr(collaborators_lst, "status"),
      header  = attr(collaborators_lst, "header"))

  info("Done", level = 7)
  collaborators_gh
}
