# LIST: properties ----------------------------------------------------------------------------
#
# The properties to show for all GitHub entities
#
# This list is used in the `view_*()` functions to extract the required properties and
# convert to the desired column type. The settings can also be used to rename properties.
#
properties <- list(

  project = list(
    id          = c("id",                      as = "integer"),
    number      = c("number",                  as = "integer"),
    name        = c("name",                    as = "character"),
    body        = c("body",                    as = "character"),
    state       = c("state",                   as = "character"),
    permission  = c("organization_permission", as = "character"),
    private     = c("private",                 as = "logical"),
    creator     = c("creator", "login",        as = "character"),
    created_at  = c("created_at",              as = "datetime"),
    updated_at  = c("updated_at",              as = "datetime"),
    html_url    = c("html_url",                as = "character")),

  column = list(
    id          = c("id",                      as = "integer"),
    name        = c("name",                    as = "character"),
    created_at  = c("created_at",              as = "datetime"),
    updated_at  = c("updated_at",              as = "datetime")),

  card = list(
    id          = c("id",                      as = "integer"),
    note        = c("note",                    as = "character"),
    archived    = c("archived",                as = "logical"),
    creator     = c("creator", "login",        as = "character"),
    created_at  = c("created_at",              as = "datetime"),
    updated_at  = c("updated_at",              as = "datetime"),
    content_url = c("content_url",             as = "character"))

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
    position       = c("top", "bottom"))

)
