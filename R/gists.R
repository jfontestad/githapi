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
