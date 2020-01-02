#  FUNCTION: view_organizations --------------------------------------------------------------------
#
#' View organizations in GitHub
#'
#' `view_organizations()` summarises organizations in a table with the properties as columns and a
#' row for each organization. `view_organization()` returns a list of all properties for a single
#' organization. `browse_organization()` opens the web page for the organization in the default
#' browser.
#'
#' You can summarise all the organizations a user is a member of by specifying the user login,
#' or the authenticated user if it is set to `NULL`. If a user is not supplied the first `n_max`
#' organizations of GitHub are returned.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/orgs/#list-user-organizations>
#' - <https://developer.github.com/v3/orgs/#list-your-organizations>
#' - <https://developer.github.com/v3/orgs/#list-all-organizations>
#'
#' @param organization (string) The login of the organization.
#' @param user (string, optional) The login of the user. If `NULL` the authenticated user is used.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `view_organizations()` returns a tibble of organization properties.
#'   `view_organization()` returns a list of properties for a single organization.
#'   `browse_organization()` opens the default browser on the organization's page and returns the
#'   URL invisibly.
#'
#' **Organization Properties:**
#'
#' - **id**: The ID of the organization.
#' - **login**: The login name of the organization.
#' - **description**: The description of the organization.
#'
#' The following are only returned using `view_organization()`:
#'
#' - **name**: The name of the organization.
#' - **company**: The name of the associated company.
#' - **blog**: The address for a blog.
#' - **location**: The geographical location.
#' - **email**: The email address for the organization.
#' - **is_verified**: Whether the organization has been verified.
#' - **has_organization_projects**: Whether the organization can have projects.
#' - **has_repository_projects**: Whether the organization's repositories can have projects.
#' - **public_repos**: The number of public repositories.
#' - **public_gists**: The number of public gists.
#' - **html_url**: The address for the organization's GitHub web page.
#' - **created_at**: When the organization was created.
#' - **total_private_repos**: The number of private repositories.
#' - **owned_private_repos**:  The number of owned private repositories.
#' - **private_gists**:  The number of private gists.
#' - **disk_usage**:  The total disk usage for the organization.
#' - **collaborators**: The number of collaborators.
#' - **billing_email**: The email address for billing.
#' - **plan_name**: The name of the GitHub plan.
#' - **plan_space**: The total space allocated for the plan.
#' - **plan_private_repos**: The number of private repositories for the plan.
#' - **default_repository_permission**: The default access for new repositories.
#' - **two_factor_requirement_enabled**: Whether members require two-factor authentication.
#' - **members_can_create_repositories**: Whether members can create repositories.
#' - **members_can_create_public_repositories**: Whether members can create public repositories.
#' - **members_can_create_private_repositories**: Whether members can create private repositories.
#' - **members_can_create_internal_repositories**:  Whether members can create internal repositories.
#'
#' @examples
#' \dontrun{
#'   # View organizations a user is a member of
#'   view_organizations(user = "ChadGoymer")
#'
#'   # View organizations the authenticated user is a member of
#'   view_organizations(user = NULL)
#'
#'   # View all organizations
#'   view_organizations()
#'
#'   # View a single organization in more detail.
#'   view_organization("HairyCoos")
#'
#'   # Browse a organization's GitHub page
#'   browse_organization("HairyCoos")
#' }
#'
#' @export
#'
view_organizations <- function(
  user,
  n_max = 1000,
  ...)
{
  if (!missing(user))
  {
    if (is_null(user))
    {
      info("Viewing organizations for authenticated user")
      url <- gh_url("/user/orgs")
    }
    else
    {
      assert(is_scalar_character(user), "'user' must be a string:\n  ", user)

      info("Viewing organizations for user '", user, "'")
      url <- gh_url("/users", user, "orgs")
    }
  }
  else
  {
    info("Viewing all organizations")
    url <- gh_url("organizations")
  }

  organizations_lst <- gh_page(url = url, n_max  = n_max, ...)

  info("Transforming results", level = 4)
  organizations_gh <- bind_properties(organizations_lst, properties$organizations)

  info("Done", level = 7)
  organizations_gh
}


#  FUNCTION: view_organization -------------------------------------------------------------
#
#' @rdname view_organizations
#' @export
#'
view_organization <- function(
  organization,
  ...)
{
  assert(is_scalar_character(organization), "'organization' must be a string:\n  ", organization)

  info("Viewing organization '", organization, "'")
  organization_lst <- gh_url("orgs", organization) %>% gh_request("GET", ...)

  info("Transforming results", level = 4)
  organization_gh <- select_properties(organization_lst, properties$organization)

  info("Done", level = 7)
  organization_gh
}


#  FUNCTION: browse_organization --------------------------------------------------------------
#
#' @rdname view_organizations
#' @export
#'
browse_organization <- function(
  organization,
  ...)
{
  organization <- view_organization(organization, ...)

  info("Browsing organization '", organization$login, "'")
  httr::BROWSE(organization$html_url)

  info("Done", level = 7)
  structure(
    organization$html_url,
    class   = c("github", "character"),
    url     = attr(organization, "url"),
    request = attr(organization, "request"),
    status  = attr(organization, "status"),
    header  = attr(organization, "header"))
}
