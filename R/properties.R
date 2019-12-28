# LIST: properties ----------------------------------------------------------------------------
#
# The properties to show for all GitHub entities
#
# This list is used in the `view_*()` functions to extract the required properties and
# convert to the desired column type. The settings can also be used to rename properties.
#
properties <- list(

  project = list(
    id                                       = c("id",                                       as = "integer"),
    number                                   = c("number",                                   as = "integer"),
    name                                     = c("name",                                     as = "character"),
    body                                     = c("body",                                     as = "character"),
    state                                    = c("state",                                    as = "character"),
    permission                               = c("organization_permission",                  as = "character"),
    private                                  = c("private",                                  as = "logical"),
    creator                                  = c("creator", "login",                         as = "character"),
    created_at                               = c("created_at",                               as = "datetime"),
    updated_at                               = c("updated_at",                               as = "datetime"),
    html_url                                 = c("html_url",                                 as = "character")),

  column = list(
    id                                       = c("id",                                       as = "integer"),
    name                                     = c("name",                                     as = "character"),
    created_at                               = c("created_at",                               as = "datetime"),
    updated_at                               = c("updated_at",                               as = "datetime")),

  card = list(
    id                                       = c("id",                                       as = "integer"),
    note                                     = c("note",                                     as = "character"),
    archived                                 = c("archived",                                 as = "logical"),
    creator                                  = c("creator", "login",                         as = "character"),
    created_at                               = c("created_at",                               as = "datetime"),
    updated_at                               = c("updated_at",                               as = "datetime"),
    content_url                              = c("content_url",                              as = "character")),

  users = list(
    id                                       = c("id",                                       as = "integer"),
    login                                    = c("login",                                    as = "character"),
    site_admin                               = c("site_admin",                               as = "logical"),
    html_url                                 = c("html_url",                                 as = "character")),

  user = list(
    id                                       = c("id",                                       as = "integer"),
    login                                    = c("login",                                    as = "character"),
    name                                     = c("name",                                     as = "character"),
    email                                    = c("email",                                    as = "character"),
    blog                                     = c("blog",                                     as = "character"),
    company                                  = c("company",                                  as = "character"),
    location                                 = c("location",                                 as = "character"),
    hireable                                 = c("hireable",                                 as = "logical"),
    bio                                      = c("bio",                                      as = "character"),
    site_admin                               = c("site_admin",                               as = "logical"),
    html_url                                 = c("html_url",                                 as = "character")),

  organizations = list(
    id                                       = c("id",                                       as = "integer"),
    login                                    = c("login",                                    as = "character"),
    description                              = c("description",                              as = "character")),

  organization = list(
    id                                       = c("id",                                       as = "integer"),
    login                                    = c("login",                                    as = "character"),
    description                              = c("description",                              as = "character"),
    name                                     = c("name",                                     as = "character"),
    company                                  = c("company",                                  as = "character"),
    blog                                     = c("blog",                                     as = "character"),
    location                                 = c("location",                                 as = "character"),
    email                                    = c("email",                                    as = "character"),
    is_verified                              = c("is_verified",                              as = "logical"),
    has_organization_projects                = c("has_organization_projects",                as = "logical"),
    has_repository_projects                  = c("has_repository_projects",                  as = "logical"),
    public_repos                             = c("public_repos",                             as = "integer"),
    public_gists                             = c("public_gists",                             as = "integer"),
    html_url                                 = c("html_url",                                 as = "character"),
    created_at                               = c("created_at",                               as = "datetime"),
    total_private_repos                      = c("total_private_repos",                      as = "integer"),
    owned_private_repos                      = c("owned_private_repos",                      as = "integer"),
    private_gists                            = c("private_gists",                            as = "integer"),
    disk_usage                               = c("disk_usage",                               as = "numeric"),
    collaborators                            = c("collaborators",                            as = "integer"),
    billing_email                            = c("billing_email",                            as = "character"),
    plan_name                                = c("plan", "name",                             as = "character"),
    plan_space                               = c("plan", "space",                            as = "integer"),
    plan_private_repos                       = c("plan", "private_repos",                    as = "integer"),
    default_repository_settings              = c("default_repository_settings",              as = "character"),
    members_can_create_repositories          = c("members_can_create_repositories",          as = "logical"),
    two_factor_requirement_enabled           = c("two_factor_requirement_enabled",           as = "logical"),
    members_allowed_repository_creation_type = c("members_allowed_repository_creation_type", as = "character"),
    members_can_create_public_repositories   = c("members_can_create_public_repositories",   as = "logical"),
    members_can_create_private_repositories  = c("members_can_create_private_repositories",  as = "logical"),
    members_can_create_internal_repositories = c("members_can_create_internal_repositories", as = "logical"))

)


# LIST: values --------------------------------------------------------------------------------
#
# List of allowed values
#
# This list contains the allowed values the specified properties, as defined by enumerations
# in the GitHub documentation. They are used to validate parameters.
#
values <- list(

  project = list(
    state          = c("open", "closed", "all"),
    permission     = c("read", "write", "admin", "none")),

  column = list(
    position       = c("first", "last")),

  card = list(
    content_type   = c("Issue", "PullRequest"),
    archived_state = c("archived", "not_archived", "all"),
    position       = c("top", "bottom")),

  user = list(
    role           = c("admin", "member", "all"))

)
