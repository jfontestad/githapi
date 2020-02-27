#  FUNCTION: is_sha ---------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid SHA
#'
#' A valid SHA is 40 characters long and only contains the characters 0-9 & a-f.
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid SHA, FALSE otherwise
#'
#' @export
#'
is_sha <- function(x)
{
  is_scalar_character(x) &&
    identical(str_length(x), 40L) &&
    all(str_split(x, "")[[1]] %in% c(0:9, letters[1:6]))
}


#  FUNCTION: is_repo --------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid repository name
#'
#' A valid repository name is comprised of two strings separated by a "/".
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid repository name, FALSE otherwise
#'
#' @export
#'
is_repo <- function(x)
{
  is_scalar_character(x) &&
    identical(length(str_split(x, "/")[[1]]), 2L)
}


# FUNCTION: is_hex ----------------------------------------------------------------------------
#
# Checks whether the supplied object is a hexidecimal color code
#
# @param x Object to check
#
# @return TRUE if x is a valid hexidecimal color code, FALSE otherwise
#
is_hex <- function(x)
{
  is_scalar_character(x) && nchar(x) == 7 && startsWith(x, "#")
}


# FUNCTION: as_hex ----------------------------------------------------------------------------
#
# Convert a vector of color names into hexidecimal codes
#
# @param x (character) The vector to convert
#
# @return A character vector of hexidecimal codes
#
as_hex <- function(color_name)
{
  color_matrix <- grDevices::col2rgb(color_name)
  grDevices::rgb(
    red   = color_matrix[1,] / 255,
    green = color_matrix[2,] / 255,
    blue  = color_matrix[3,] / 255)
}


# FUNCTION: random_color ----------------------------------------------------------------------
#
# Select a color at random
#
# @return A color name sampled from [grDevices::colors()]
#
random_color <- function()
{
  sample(grDevices::colors(), 1)
}


# FUNCTION: as.datetime -----------------------------------------------------------------------
#
# convert a vector into a date time (POSIXct) vector
#
# @param x (any) The vector to convert
#
# @return A `POSIXct` vector
#
as.datetime <- function(x)
{
  as.POSIXct(x, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC") %>%
    format(tz = "") %>%
    as.POSIXct()
}


# FUNCTION: property_names --------------------------------------------------------------------
#
# Construct property names
#
# If names have been specified then they are used, otherwise concatenate the property vector
#
# @param properties (list) A list of properties
#
# @return A character vector of names
#
property_names <- function(properties)
{
  names <- map_chr(properties, str_c, collapse = "_")

  if (!is_null(names(properties))) {
    for (property in seq_along(properties)) {
      if (!identical(names(properties)[[property]], "")) {
        names[[property]] <- names(properties)[[property]]
      }
    }
  }

  unname(names)
}


# FUNCTION: select_properties -----------------------------------------------------------------
#
# Select properties from an entity
#
# @param entity (list) An entity with properties
# @param properties (list) A single list of properties to extract
#
# @return A list of properties
#
select_properties <- function(entity, properties)
{
  assert(is_null(entity) || is_list(entity), "'entity' must be a list:\n  ", entity)
  assert(is_list(properties) && !identical(length(properties), 0L), "'properties' must be a non-empty list:\n  ", properties)

  conversions <- map_chr(properties, ~ .["as"])
  properties  <- properties %>%
    map(function(p) if (is_null(names(p))) p else p[names(p) != "as"]) %>%
    set_names(property_names(.))

  if (is_null(entity) || identical(length(entity), 0L))
  {
    selected_properties <- map(properties, ~ logical())
  }
  else
  {
    selected_properties <- map(properties, ~ pluck(.x = entity, !!!., .default = NA))
  }

  map2(selected_properties, conversions, function(prop, conv) {
    if (is_na(conv)) prop else exec(str_c("as.", conv), prop)
  }) %>%
    structure(
      class   = c("github", class(.)),
      url     = attr(entity, "url"),
      request = attr(entity, "request"),
      status  = attr(entity, "status"),
      header  = attr(entity, "header"))
}


# FUNCTION: bind_properties -------------------------------------------------------------------
#
# Bind properties from a collection of entities into a tibble
#
# @param collection (list) A collection of entities with common properties
# @param properties (list) A single list of properties to extract
#
# @return A tibble with properties as columns and a row for each entity
#
bind_properties <- function(collection, properties)
{
  assert(is_list(collection), "'collection' must be a list:\n  ", collection)
  assert(is_list(properties) && !identical(length(properties), 0L), "'properties' must be a non-empty list:\n  ", properties)

  conversions <- map_chr(properties, ~ .["as"])
  properties  <- properties %>%
    map(function(p) if (is_null(names(p))) p else p[names(p) != "as"]) %>%
    set_names(property_names(.))

  if (identical(length(collection), 0L))
  {
    selected_properties <- map_dfc(properties, ~ logical())
  }
  else
  {
    selected_properties <- map_dfr(collection, function(entity)
    {
      map(properties, ~ pluck(.x = entity, !!!., .default = NA))
    })
  }

  map2(selected_properties, conversions, function(prop, conv) {
    if (is_na(conv)) prop else exec(str_c("as.", conv), prop)
  }) %>%
    as_tibble() %>%
    structure(
      class   = c("github", class(.)),
      url     = attr(collection, "url"),
      request = attr(collection, "request"),
      status  = attr(collection, "status"),
      header  = attr(collection, "header"))
}
