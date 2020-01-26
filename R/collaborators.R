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
