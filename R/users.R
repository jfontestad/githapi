#  FUNCTION: view_users --------------------------------------------------------------------
#
#' View users in GitHub
#'
#' `view_users()` summarises users in a table with the properties as columns and a row
#' for each user. `view_user()` returns a list of all properties for a single user.
#'
#' You can summarise all the users associated with either a repository, organisation or a
#' team within an organisation. If none of those are supplied the first `n_max` users of
#' GitHub are returned.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/collaborators/#list-collaborators>
#' - <https://developer.github.com/v3/orgs/members/#members-list>
#' - <https://developer.github.com/v3/teams/members/#list-team-members>
#' - <https://developer.github.com/v3/users/#get-all-users>
#' - <https://developer.github.com/v3/users/#get-a-single-user>
#'
#' @param user (integer) The ID of the user.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param org (string, optional) The name of the organization.
#' @param team (string, optional) The name of the team.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_users()` returns a tibble of user properties. `view_user()`
#'   returns a list of properties for a single user.
#'
#' **user Properties:**
#'
#' - **id**: The ID of the user.
#' - **login**: The login name of the user.
#' - **type**: The type of account.
#' - **site_admin**: Whether the user is an administrator.
#' - **html_url**: The GitHub page for the user.
#'
#' @examples
#' \dontrun{
#'   # View users collaborating on a repository
#'   view_users(repo = "ChadGoymer/githapi")
#'
#'   # View users in an organisation
#'   view_users(org = "HairyCoos")
#'
#'   # View users in a team within an organisation
#'   view_users(org = "HairyCoos", team = "HeadCoos")
#' }
#'
#' @export
#'
view_users <- function(
  repo,
  org,
  team,
  n_max = 1000,
  ...)
{
  if (!missing(repo))
  {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

    info("Viewing users in repo '", repo, "'")
    url <- gh_url("repos", repo, "collaborators")
  }
  else if (!missing(org))
  {
    assert(is_scalar_character(org), "'org' must be a string:\n  ", org)

    info("Viewing users in organisation '", org, "'")
    url <- gh_url("orgs", org, "members")
  }
  else if (!missing(team))
  {
    assert(is_scalar_character(team), "'team' must be a string:\n  ", team)

    team <- gh_url("orgs", org, "teams") %>% gh_find(property = "name", value = team, ...)

    info("Viewing users in team '", team$name, "'")
    url <- gh_url("teams", team$id, "members")
  }
  else
  {
    url <- gh_url("users")
  }

  users_lst <- gh_page(url = url, n_max  = n_max, ...)

  info("Transforming results", level = 4)
  users_gh <- bind_properties(users_lst, properties$user)

  info("Done", level = 7)
  users_gh
}


#  FUNCTION: view_user ---------------------------------------------------------------------
#
#' @rdname view_users
#' @export
#'
view_user <- function(
  user,
  ...)
{
  assert(is_scalar_character(user), "'user' must be a string:\n  ", user)

  info("Viewing user '", user, "'")
  user_lst <- gh_url("users", user) %>% gh_request("GET", ...)

  info("Transforming results", level = 4)
  user_gh <- select_properties(user_lst, properties$user)

  info("Done", level = 7)
  user_gh
}
