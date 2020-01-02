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


#  FUNCTION: update_organization --------------------------------------------------------------
#
#' Update an organization's properties in GitHub
#'
#' This function updates an organization's properties in GitHub. You can only update an
#' organization if you have the appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/orgs/#edit-an-organization>
#'
#' @param organization (string) The login of the organization.
#' @param name (string, optional) The shorthand name of the company.
#' @param description (string, optional) The description of the company.
#' @param email (string, optional) The publicly visible email address.
#' @param location (string, optional) The location.
#' @param company (string, optional) The company name.
#' @param billing_email (string, optional) Billing email address. This address is not
#'   publicized.
#' @param has_organization_projects (boolean, optional) Toggles whether an organization can use
#'   organization projects.
#' @param has_repository_projects (boolean, optional) Toggles whether repositories that belong
#'   to the organization can use repository projects.
#' @param default_repository_permission (string, optional) Default permission level members have
#' for organization repositories:
#'   - `"read"`: can pull, but not push to or administer this repository.
#'   - `"write"`: can pull and push, but not administer this repository.
#'   - `"admin"`: can pull, push, and administer this repository.
#'   - `"none"`: no permissions granted by default.
#'   Default: `"read"`.
#' @param members_can_create_repositories (boolean, optional) Toggles the ability of non-admin
#'   organization members to create repositories. Can be one of:
#'   - `TRUE`: all organization members can create repositories.
#'   - `FALSE`: only organization owners can create repositories.
#'   Default: `TRUE`.
#'   Note: A parameter can override this parameter. See
#'   members_allowed_repository_creation_type in this table for details.
#' @param members_can_create_internal_repositories (boolean, optional) Toggles whether
#'   organization members can create internal repositories, which are visible to all enterprise
#'   members. You can only allow members to create internal repositories if your organization is
#'   associated with an enterprise account using GitHub Enterprise Cloud. Can be one of:
#'   - `TRUE`: all organization members can create internal repositories.
#'   - `FALSE`: only organization owners can create internal repositories.
#'   Default: `TRUE`. For more information, see "Restricting repository creation in your
#'   organization" in the GitHub Help documentation.
#' @param members_can_create_private_repositories (boolean, optional) Toggles whether organization
#'   members can create private repositories, which are visible to organization members with
#'   permission. Can be one of:
#'   - `TRUE`: all organization members can create private repositories.
#'   - `FALSE`: only organization owners can create private repositories.
#'   Default: `TRUE`. For more information, see "Restricting repository creation in your
#'   organization" in the GitHub Help documentation.
#' @param members_can_create_public_repositories (boolean, optional) Toggles whether organization
#'   members can create public repositories, which are visible to anyone. Can be one of:
#'   - `TRUE`: all organization members can create public repositories.
#'   - `FALSE`: only organization owners can create public repositories.
#'   Default: `TRUE`. For more information, see "Restricting repository creation in your
#'   organization" in the GitHub Help documentation.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_organization()` returns a list of the new organization properties.
#'
#' **Organization Properties:**
#'
#' - **id**: The ID of the organization.
#' - **login**: The login name of the organization.
#' - **name**: The name of the organization.
#' - **description**: The description of the organization.
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
#' - **default_repository_settings**: The default access for new repositories.
#' - **members_can_create_repositories**: Whether members can create repositories.
#' - **two_factor_requirement_enabled**: Whether members require two-factor authentication.
#' - **members_allowed_repository_creation_type**: The types of repositories members can create.
#' - **members_can_create_public_repositories**: Whether members can create public repositories.
#' - **members_can_create_private_repositories**: Whether members can create private repositories.
#' - **members_can_create_internal_repositories**:  Whether members can create internal repositories.
#'
#' @examples
#' \dontrun{
#'   # Update your name
#'   update_organization(name = "Bob Smith")
#'
#'   # Update your company
#'   update_organization(company = "Acme")
#'
#'   # Update your hireable status
#'   update_organization(hireable = TRUE)
#' }
#'
#' @export
#'
update_organization <- function(
  organization,
  name                                     = NULL,
  description                              = NULL,
  email                                    = NULL,
  location                                 = NULL,
  company                                  = NULL,
  billing_email                            = NULL,
  has_organization_projects                = NULL,
  has_repository_projects                  = NULL,
  default_repository_permission            = NULL,
  members_can_create_repositories          = NULL,
  members_can_create_internal_repositories = NULL,
  members_can_create_private_repositories  = NULL,
  members_can_create_public_repositories   = NULL,
  ...)
{
  assert(is_null(name) || is_scalar_character(name), "'name' must be a string:\n  ", name)
  assert(is_null(description) || is_scalar_character(description), "'description' must be a string:\n  ", description)
  assert(is_null(email) || is_scalar_character(email), "'email' must be a string:\n  ", email)
  assert(is_null(location) || is_scalar_character(location), "'location' must be a string:\n  ", location)
  assert(is_null(company) || is_scalar_character(company), "'company' must be a string:\n  ", company)
  assert(is_null(billing_email) || is_scalar_character(billing_email), "'billing_email' must be a string:\n  ", billing_email)
  assert(
    is_null(has_organization_projects) || is_scalar_logical(has_organization_projects),
    "'has_organization_projects' must be a boolean:\n  ", has_organization_projects)
  assert(
    is_null(has_repository_projects) || is_scalar_logical(has_repository_projects),
    "'has_repository_projects' must be a boolean:\n  ", has_repository_projects)
  assert(
    is_null(default_repository_permission) || is_scalar_character(default_repository_permission),
    "'default_repository_permission' must be a string:\n  ", default_repository_permission)
  assert(
    is_null(default_repository_permission) || default_repository_permission %in% values$organization$default_repository_permission,
    "'default_repository_permission' must be one of '", str_c(values$organization$default_repository_permission, collapse = "', '"), "':\n  ", default_repository_permission)
  assert(
    is_null(members_can_create_repositories) || is_scalar_logical(members_can_create_repositories),
    "'members_can_create_repositories' must be a string:\n  ", members_can_create_repositories)
  assert(
    is_null(members_can_create_internal_repositories) || is_scalar_logical(members_can_create_internal_repositories),
    "'members_can_create_internal_repositories' must be a boolean:\n  ", members_can_create_internal_repositories)
  assert(
    is_null(members_can_create_private_repositories) || is_scalar_logical(members_can_create_private_repositories),
    "'members_can_create_private_repositories' must be a boolean:\n  ", members_can_create_private_repositories)
  assert(
    is_null(members_can_create_public_repositories) || is_scalar_logical(members_can_create_public_repositories),
    "'members_can_create_public_repositories' must be a boolean:\n  ", members_can_create_public_repositories)

  payload <- list(
    name                                     = name,
    description                              = description,
    email                                    = email,
    location                                 = location,
    company                                  = company,
    billing_email                            = billing_email,
    has_organization_projects                = has_organization_projects,
    has_repository_projects                  = has_repository_projects,
    default_repository_permission            = default_repository_permission,
    members_can_create_repositories          = members_can_create_repositories,
    members_can_create_internal_repositories = members_can_create_internal_repositories,
    members_can_create_private_repositories  = members_can_create_private_repositories,
    members_can_create_public_repositories   = members_can_create_public_repositories) %>%
    compact()

  info("Updating organization")
  organization_lst <- gh_url("orgs", organization) %>% gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  organization_gh <- select_properties(organization_lst, properties$organization)

  info("Done", level = 7)
  organization_gh
}
