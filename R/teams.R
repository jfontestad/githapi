#  FUNCTION: create_team -------------------------------------------------------------------
#
#' Create a team in a GitHub organization
#'
#' This function creates a new team in the specified organization in GitHub. It can also
#' be used to specify the maintainers, the repositories to add the team to and a parent
#' team.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/teams/#create-team>
#'
#' @param name (string) The name of the team.
#' @param organization (string) The login of the organization.
#' @param description (string, optional) The description of the team.
#' @param maintainers (character, optional) The logins of organization members to add as
#'   maintainers of the team. If you do not specify any maintainers, then you will
#'   automatically become a team maintainer when you create a new team.
#' @param repo_names (character, optional) The full name (e.g.
#'   "organization-name/repository-name") of repositories to add the team to.
#' @param privacy (string, optional) The level of privacy this team should have. The
#' options are:
#'   For a non-nested team:
#'   - `"secret"`: only visible to organization owners and members of this team.
#'   - `"closed"`: visible to all members of this organization.
#'
#'   Default: `"secret"`.
#'
#'   For a parent or child team:
#'   - `"closed"`: visible to all members of this organization.
#'
#'   Default for child team: `"closed"`
#' @param parent_team (integer or string, optional) The ID or name of a team to set as the
#'   parent team.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_team()` returns a list of the team's properties.
#'
#' **Team Properties:**
#'
#' - **id**: The ID of the team.
#' - **name**: The name of the team.
#' - **organization**: The organization the team is associated with.
#' - **slug**: The team slug name.
#' - **description**: The description of the team.
#' - **privacy**: The privacy setting of the team - either "closed" or "secret".
#' - **permission**: The default repository permissions of the team.
#' - **parent**: The parent team.
#' - **members_count**: The number of members.
#' - **repos_count**: The number of repositories the team has access to.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#' - **html_url**: The URL of the team page in GitHub
#'
#' @examples
#' \dontrun{
#'   # Create a team in an organization
#'   create_team("TestTeam", "HairyCoos")
#'
#'   # Create a team and specify maintainers
#'   create_team("TestTeam2", "HairyCoos", maintainers = c("ChadGoymer", "chris-walker1"))
#'
#'   # Create a team and set the privacy
#'   create_team("TestTeam3", "HairyCoos", privacy = "closed")
#'
#'   # Create a team and specify a parent team
#'   create_team("TestTeam4", "HairyCoos", parent_team = "TestTeam3")
#' }
#'
#' @export
#'
create_team <- function(
  name,
  organization,
  description,
  maintainers,
  repo_names,
  privacy,
  parent_team,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
  assert(is_scalar_character(organization), "'organization' must be a string:\n  ", organization)

  payload <- list(name = name)

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  if (!missing(maintainers)) {
    assert(is_character(maintainers), "'maintainers' must be a character vector:\n  ", maintainers)
    payload$maintainers <- maintainers
  }

  if (!missing(repo_names)) {
    assert(is_character(repo_names), "'repo_names' must be a character vector:\n  ", repo_names)
    payload$repo_names <- as.list(repo_names)
  }

  if (!missing(privacy)) {
    assert(
      is_scalar_character(privacy) && privacy %in% values$team$privacy,
      "'privacy' must be one of '", str_c(values$team$privacy, collapse = "', '"), "':\n  ", privacy)
    payload$privacy <- privacy
  }

  if (!missing(parent_team)) {
    if (is_scalar_character(parent_team))
    {
      parent_team <- gh_url("orgs", organization, "teams") %>%
        gh_find(property = "name", value = parent_team, ...) %>%
        pluck("id")
    }
    assert(is_scalar_integerish(parent_team), "'parent_team' must be an integer or string:\n  ", parent_team)
    payload$parent_team_id <- parent_team
  }

  team_lst <- gh_url("orgs", organization, "teams") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  team_gh <- select_properties(team_lst, properties$team)

  info("Done", level = 7)
  team_gh
}
