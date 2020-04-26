#  FUNCTION: create_gist ----------------------------------------------------------------------
#
#' Create a gist in github
#'
#' This function creates a new gist in GitHub. You can specify one or more files by providing
#' a named list (see examples).
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/gists/#create-a-gist>
#'
#' @param files (list) The file contents with the list names as the filenames.
#' @param description (string, optional) A description for the gist.
#' @param public (boolean, optional) Whether the gist is public. Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_gist()` returns a list of the gist's properties.
#'
#' **Gist Properties:**
#'
#' - **id**: The id for the gist
#' - **description**: The description of the gist
#' - **files**: A tibble of file properties:
#'   - **filename**: The name of the file
#'   - **type**: The type of file
#'   - **content**: The file content
#'   - **size**: The size of the file in bytes
#'   - **truncated**: Whether the file content has been truncated
#' - **owner**: The login if the gist's owner
#' - **public**: Whether the gist is public
#' - **html_url**: The address of the gist's web page.
#' - **created_at**: The time and date the gist was created.
#' - **updated_at**: The time and date the gist was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a gist with a single file
#'   create_gist(files = list(hello_world.R = "print(\"Hello World!\")"))
#'
#'   # Create a gist with multiple files
#'   create_gist(
#'     files = list(
#'       hello_world.R    = "print(\"Hello World!\")",
#'       `hello_world-fn.R` = "helloworld <- function() print(\"Hello World!\")"),
#'     description = "A new gist")
#'
#'   # Create a public gist
#'   create_gist(
#'     files       = list(hello_world.R = "print(\"Hello World!\")"),
#'     description = "A new gist",
#'     public      = TRUE)
#'
#' }
#'
#' @export
#'
create_gist <- function(
  files,
  description,
  public = FALSE,
  ...)
{
  assert(is_list(files) & has_names(files), "'files' must be a named list:\n  ", files)
  assert(all(map_lgl(files, is_scalar_character)), "'files' must be a list of string:\n  ", files)
  assert(is_scalar_logical(public), "'public' must be a boolean:\n  ", public)

  payload <- list(public = public)
  payload$files <- map(files, ~ list(content = .))

  if (!missing(description))
  {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  info("Creating gist")
  gist_lst <- gh_url("gists") %>% gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  gist_gh <- select_properties(gist_lst, properties$gist) %>%
    modify_list(files = bind_properties(gist_lst$files, properties$gist_file), .before = "owner")

  info("Done", level = 7)
  structure(
    gist_gh,
    class   = class(gist_lst),
    url     = attr(gist_lst, "url"),
    request = attr(gist_lst, "request"),
    status  = attr(gist_lst, "status"),
    header  = attr(gist_lst, "header"))
}


#  FUNCTION: update_gist ----------------------------------------------------------------------
#
#' Update a gist in GitHub
#'
#' This function updates an existing gist in GitHub. You can specify add or update files by
#' providing a named list and rename a file (see examples).
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/gists/#update-a-gist>
#'
#' @param gist (string) The ID of the gist
#' @param files (list, optional) The file contents with the list names as the filenames.
#'   Added a `filename` element changes the filename (see examples).
#' @param description (string, optional) A description for the gist.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_gist()` returns a list of the gist's properties.
#'
#' **Gist Properties:**
#'
#' - **id**: The id for the gist
#' - **description**: The description of the gist
#' - **files**: A tibble of file properties:
#'   - **filename**: The name of the file
#'   - **type**: The type of file
#'   - **content**: The file content
#'   - **size**: The size of the file in bytes
#'   - **truncated**: Whether the file content has been truncated
#' - **owner**: The login if the gist's owner
#' - **public**: Whether the gist is public
#' - **html_url**: The address of the gist's web page.
#' - **created_at**: The time and date the gist was created.
#' - **updated_at**: The time and date the gist was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a gist's description
#'   update_gist(
#'     gist        = "806dca6b09a39e7b6326a0c8137583e6",
#'     description = "An updated description")
#'
#'   # Update the contents of a file
#'   create_gist(files = list(hello_world.R = "cat(\"Hello World!\")"))
#'
#'   # Update the contents of a file and the filename
#'   create_gist(
#'     files = list(hello_world.R = c("cat(\"Hello World!\")", filename = "new_filename.R")))
#'
#' }
#'
#' @export
#'
update_gist <- function(
  gist,
  files,
  description,
  ...)
{
  assert(is_scalar_character(gist), "'gist' must be a string:\n  ", gist)

  payload <- NULL

  if (!missing(files))
  {
    assert(is_list(files) & has_names(files), "'files' must be a named list:\n  ", files)
    assert(
      all(map_lgl(files, ~ is_character(.) && names(.) %in% c("", "content", "filename"))),
      "'files' must be a list of character vectors:\n  ", files)
    payload$files <- map(files, function(f) {
      as.list(set_names(f, ifelse(names(f) == "", "content", names(f))))
    })
  }

  if (!missing(description))
  {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  info("Updating gist '", gist, "'")
  gist_lst <- gh_url("gists", gist) %>% gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  gist_gh <- select_properties(gist_lst, properties$gist) %>%
    modify_list(files = bind_properties(gist_lst$files, properties$gist_file), .before = "owner")

  info("Done", level = 7)
  structure(
    gist_gh,
    class   = class(gist_lst),
    url     = attr(gist_lst, "url"),
    request = attr(gist_lst, "request"),
    status  = attr(gist_lst, "status"),
    header  = attr(gist_lst, "header"))
}


#  FUNCTION: view_gists -----------------------------------------------------------------------
#
#' View gists in GitHub
#'
#' `view_gists()` summarises a user's gists in a table with the properties as columns and a
#' row for each gist. `view_gist()` returns a list of all properties for a single gist.
#' `browse_gist()` opens the web page for the gist in the default browser.
#'
#' When viewing gists, if a user is not specified the gists for the authenticated user are
#' returned.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/gists/#list-gists-for-a-user>
#' - <https://developer.github.com/v3/gists/#list-gists-for-the-authenticated-user>
#' - <https://developer.github.com/v3/gists/#get-a-gist>
#'
#' @param gist (string) The id of the gist.
#' @param user (string, optional) The login of the user. If not specified the authenticated
#'   user is used.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `view_gists()` returns a tibble of gist properties. `view_gist()`
#'   returns a list of properties for a single gist. `browse_gist` opens the
#'   default browser on the gist page and returns the URL.
#'
#' **Gist Properties:**
#'
#' - **id**: The id for the gist
#' - **description**: The description of the gist
#' - **files**: For `view_gists()` just the filenames are returned, for `view_gist()` a
#'   tibble of file properties is returned:
#'   - **filename**: The name of the file
#'   - **type**: The type of file
#'   - **content**: The file content
#'   - **size**: The size of the file in bytes
#'   - **truncated**: Whether the file content has been truncated
#' - **owner**: The login if the gist's owner
#' - **public**: Whether the gist is public
#' - **html_url**: The address of the gist's web page.
#' - **created_at**: The time and date the gist was created.
#' - **updated_at**: The time and date the gist was last updated.
#'
#' @examples
#' \dontrun{
#'
#'   # View the authenticated user's gists
#'   view_gists()
#'
#'   # View a specific user's gists
#'   view_gists("ChadGoymer")
#'
#'   # View a specific gist
#'   view_gist("806dca6b09a39e7b6326a0c8137583e6")
#'
#'   # Browse a gist
#'   browse_gist("806dca6b09a39e7b6326a0c8137583e6")
#'
#' }
#'
#' @export
#'
view_gists <- function(
  user,
  since,
  n_max = 1000,
  ...)
{
  if (!missing(since)) {
    assert(is_scalar_character(since), "'since' must be a string:\n  ", since)
    since <- as.POSIXct(since, format = "%Y-%m-%d %H:%M:%S") %>%
      format("%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    assert(!is.na(since), "'since' must be specified in the format 'YYYY-MM-DD hh:mm:ss':\n  ", since)
  }
  else {
    since <- NULL
  }

  if (missing(user)) {
    info("Viewing gists for authenticated user")
    url <- gh_url("gists", since = since)
  }
  else {
    assert(is_scalar_character(user), "'user' must be a string:\n  ", user)
    if (user %in% c("public", "starred")) {
      info("Viewing ", user, " gists")
      url <- gh_url("gists", user, since = since)
    }
    else {
      info("Viewing gists for user '", user, "'")
      url <- gh_url("users", user, "gists", since = since)
    }
  }

  gists_lst <- gh_page(url, n_max = n_max, ...)

  info("Transforming results", level = 4)
  gists_gh <- bind_properties(gists_lst, properties$gist) %>%
    add_column(files = map(gists_lst, ~ names(.$files)), .before = "owner")

  info("Done", level = 7)
  gists_gh
}


#  FUNCTION: view_gist ------------------------------------------------------------------------
#
#' @rdname view_gists
#' @export
#'
view_gist <- function(
  gist,
  ...)
{
  assert(is_scalar_character(gist), "'gist' must be a string:\n  ", gist)

  info("Viewing gist '", gist, "'")
  gist_lst <- gh_url("gists", gist) %>% gh_request("GET", ...)

  info("Transforming results", level = 4)
  gist_gh <- select_properties(gist_lst, properties$gist) %>%
    modify_list(files = bind_properties(gist_lst$files, properties$gist_file), .before = "owner")

  info("Done", level = 7)
  structure(
    gist_gh,
    class   = class(gist_lst),
    url     = attr(gist_lst, "url"),
    request = attr(gist_lst, "request"),
    status  = attr(gist_lst, "status"),
    header  = attr(gist_lst, "header"))
}
