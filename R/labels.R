#  FUNCTION: create_label ------------------------------------------------------------------
#
#' Create an issue label in a repository
#'
#' This function creates a new label for issues and pull requests for the specified
#' repository in GitHub. It can also be used to set the colour and description.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/issues/labels/#create-a-label>
#'
#' @param name (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param color (string) Either the color name (see [grDevices::colors()]) or a hexidecimal
#'   color code (see [color-hex.com](http://www.color-hex.com/)).
#' @param description (string, optional) A description of the label.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_label()` returns a list of the label's properties.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
create_label <- function(
  name,
  repo,
  color,
  description,
  ...)
{
  assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_scalar_character(color), "'color' must be a string:\n  ", color)

  if (!is_hex(color)) {
    color <- as_hex(color)
  }
  color <- str_remove(color, "^#")

  payload <- list(name = name, color = color)

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  info("Creating label '", name, "' for repository '", repo, "'")
  label_lst <- gh_url("repos", repo, "labels") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  label_gh <- select_properties(label_lst, properties$label)

  info("Done", level = 7)
  label_gh
}


#  FUNCTION: update_label ---------------------------------------------------------------------
#
#' Update a label in a repository
#'
#' This function updates a label for the specified repository in GitHub. It can be used to
#' change name, color or description.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/issues/labels/#update-a-label>
#'
#' @param label (string) The name of the label.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the label.
#' @param color (string, optional) Either the color name (see [grDevices::colors()]) or a
#'   hexidecimal color code (see [color-hex.com](http://www.color-hex.com/)).
#' @param description (string, optional) A description of the label.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_label()` returns a list of the label properties.
#'
#' **Label Properties:**
#'
#' - **name**: The name of the label.
#' - **color**: The color of the label in hexidecimal code.
#' - **description**: The description of the label.
#'
#' @examples
#' \dontrun{
#' }
#'
#' @export
#'
update_label <- function(
  label,
  repo,
  name,
  color,
  description,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  payload <- list()

  if (!missing(name)) {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload$name <- name
  }

  if (!missing(color)) {
    assert(is_scalar_character(color), "'color' must be a string:\n  ", color)
    if (!is_hex(color)) {
      color <- as_hex(color)
    }
    payload$color <- str_remove(color, "^#")
  }

  if (!missing(description)) {
    assert(is_scalar_character(description), "'description' must be a string:\n  ", description)
    payload$description <- description
  }

  info("Updating label '", label, "' in repository '", repo, "'")
  label_lst <- gh_url("repos", repo, "labels", label) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  label_gh <- select_properties(label_lst, properties$label)

  info("Done", level = 7)
  label_gh
}
