#  FUNCTION: create_card -------------------------------------------------------------------
#
#' Create a card in a GitHub project
#'
#' This function creates a new card in the specified column in a project in GitHub. The
#' card can either contain an existing issue or pull request or you can create a note.
#'
#' You can create a card in a project associated with either a repository, user or
#' organisation, by supplying them as an input, as long as you have appropriate permissions.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/projects/cards/#create-a-project-card>
#'
#' @param content_id (integer, optional) The issue or pull request id associated with this
#'   card. Note: `content_type` is also required.
#' @param content_type (string, optional) Whether the `content_id` identifies an `"Issue"`
#'   or a `"PullRequest"`.
#' @param note (string, optional) The card's note content. Not used if `content_id` is
#'   specified.
#' @param column (integer or string) Either the ID or name of the column.
#' @param project (integer or string) Either the project number or name.
#' @param repo (string, optional) The repository specified in the format: `owner/repo`.
#' @param user (string, optional) The login of the user.
#' @param org (string, optional) The name of the organization.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_card()` returns a list of the card properties.
#'
#' **Card Properties:**
#'
#' - **id**: The ID of the card.
#' - **content_id**: The ID of the issue or pull request.
#' - **note**: The content of a note.
#' - **archived**: Whether the card has been archived.
#' - **creator**: The creator of the note.
#' - **created_at**: When it was created.
#' - **updated_at**: When it was last updated.
#'
#' @examples
#' \dontrun{
#'   # Create an issue card in a repository project
#'   create_card(
#'     content_id   = 1,
#'     content_type = "Issue",
#'     column       = "Test column",
#'     project      = "Test project",
#'     repo         = "ChadGoymer/test-githapi")
#'
#'   # Create a pull request card in a user's project
#'   create_card(
#'     content_id   = 2,
#'     content_type = "PullRequest",
#'     column       = "Test column",
#'     project      = "Test project",
#'     user         = "ChadGoymer")
#'
#'   # Create a note card in an organisation's project
#'   create_card(
#'     note    = "This is a note",
#'     column  = "Test column",
#'     project = "Test project",
#'     org     = "HairyCoos")
#' }
#'
#' @export
#'
create_card <- function(
  content_id,
  content_type,
  note,
  column,
  project,
  repo,
  user,
  org,
  ...)
{
  if (!missing(content_id))
  {
    assert(is_scalar_integerish(content_id), "'content_id' must be an integer:\n  ", content_id)
    assert(
      is_scalar_character(content_type) && content_type %in% values$card$content_type,
      "'content_type' must be one of '", str_c(values$card$content_type, collapse = "', '"), "':\n  ", content_type)

    # TODO: replace with view_issue() and view_pull_request()
    issue <- switch(
      content_type,
      Issue       = gh_issue(issue = content_id, repo = repo),
      PullRequest = gh_pull_request(pull_request = content_id, repo = repo))

    info("Creating card for ", str_to_lower(content_type), " '", content_id, "'")
    payload <- list(content_id = issue$id, content_type = content_type)
  }
  else if (!missing(note))
  {
    assert(is_scalar_character(note), "'note' must be an integer:\n  ", note)

    info("Creating card with specified note")
    payload    <- list(note = note)
    content_id <- NA
  }
  else
  {
    error("Either 'content_id' or 'note' must be supplied")
  }

  column <- view_column(
    column  = column,
    project = project,
    repo    = repo,
    user    = user,
    org     = org,
    ...)

  card_lst <- gh_url("projects/columns", column$id, "cards") %>%
    gh_request(
      type    = "POST",
      payload = payload,
      accept  = "application/vnd.github.inertia-preview+json",
      ...)

  info("Transforming results", level = 4)
  card_gh <- select_properties(card_lst, properties$card) %>%
    append(list(content_id   = as.integer(content_id)), after = 1) %>%
    discard(names(.) == "content_url") %>%
    structure(
      class   = class(card_lst),
      url     = attr(card_lst, "url"),
      request = attr(card_lst, "request"),
      status  = attr(card_lst, "status"),
      header  = attr(card_lst, "header"))

  info("Done", level = 7)
  card_gh
}
