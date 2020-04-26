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
