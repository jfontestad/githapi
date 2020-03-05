#' Deprecated functions in package [githapi].
#'
#' The functions listed below are deprecated and will be removed in the near future. When
#' possible, alternative functions with similar functionality are also mentioned.
#'
#' @name githapi-deprecated
#' @keywords internal
#'
#' @section Git Data Functions:
#'
#' - [gh_git_blob()]: use [view_blobs()] instead.
#' - [gh_git_commit()]: use [view_commits()] instead.
#' - [gh_git_reference()]: use [view_tags()] or [view_branches()] instead.
#' - [gh_git_references()]: use [view_tags()] or [view_branches()] instead.
#' - [is_tag()]: use [tags_exist()] instead.
#' - [gh_git_tag()]: is being removed
#' - [gh_git_tree()]: replace with [view_trees()]
#' - [gh_save()]: replace with [download_files()]
#' - [gh_source()]: replace with [source_files()]
#'
#' @section Repositories Functions:
#'
#' - [gh_tags()]: replace with [view_tags()]
#' - [gh_branch()]: replace with [view_branches()]
#' - [gh_branches()]: replace with [view_branches()]
#' - [is_branch()]: replace with [branches_exist()]
#' - [gh_commit()]: replace with [view_commits()]
#' - [is_valid_sha()]: replace with [shas_exist()]
#' - [gh_commit_sha()]: replace with [view_shas()]
#' - [gh_commits()]: replace with [view_history()]
#' - [gh_compare_commits()]: replace with [compare_commits()]
#' - [gh_compare_files()]: replace with [compare_files()]
#' - [gh_readme()]: replace with [read_files()]
#' - [gh_contents()]: replace with [read_files()]
#' - [gh_download()]: replace with [download_commit()]
#' - [gh_release()]: replace with [view_releases()]
#' - [gh_releases()]: replace with [view_releases()]
#' - [gh_repository()]: replace with [view_repository()]
#' - [gh_repositories()]: replace with [view_repositories()]
#' - [is_repository()]: is being removed
#'
#' @section Issues, Pull Requests, labels and milestones:
#'
#' - [gh_label()]: replace with [view_label()]
#' - [gh_labels()]: replace with [view_labels()]
#' - [gh_milestone()]: replace with [view_milestone()]
#' - [gh_milestones()]: replace with [view_milestones()]
#' - [gh_issue()]: replace with [view_issue()]
#' - [gh_issues()]: replace with [view_issues()]
#' - [gh_user_issues()]: replace with [view_issues()]
#' - [gh_assignees()]: is being removed
#' - [gh_issue_comments()]: replace with [view_comments()]
#' - [gh_issue_comment()]: is being removed
#' - [gh_event()]: is being removed
#' - [gh_events()]: is being removed
#'
#' @section Projects Functions:
#'
#' - [gh_project()]: replace with [view_project()]
#' - [gh_projects()]: replace with [view_projects()]
#' - [gh_column()]: replace with [view_column()]
#' - [gh_columns()]: replace with [view_columns()]
#' - [gh_card()]: replace with [view_card()]
#' - [gh_cards()]: replace with [view_cards()]
#'
#' @section Organizations, teams and users:
#'
#' - [gh_organization()]: replace with [view_organization()]
#' - [gh_organizations()]: replace with [view_organizations()]
#' - [gh_membership()]: replace with [view_membership()]
#' - [gh_memberships()]: replace with [view_memberships()]
#' - [gh_members()]: replace with [view_users()]
#' - [gh_user()]: replace with [view_user()]
#' - [gh_users()]: replace with [view_users()]
#' - [gh_user_email()]: is being removed. Use [view_user()]
#' - [gh_team()]: replace with [view_team()]
#' - [gh_teams()]: replace with [view_teams()]
#' - [is_member()]: is being removed
#' - [is_manager()]: is being removed
#' - [is_collaborator()]: is being removed
#' - [gh_collaborators()]: replace with [view_collaborators()]
#' - [gh_permissions()]: replace with [view_collaborator()]
#'
#' @section Pull requests:
#'
#' - [gh_pull_request()]: replace with [view_pull_request()]
#' - [gh_pull_requests()]: replace with [view_pull_requests()]
#' - [gh_pull_commits()]: replace with [view_pull_request()]
#' - [gh_pull_files()]: replace with [view_pull_request()]
#' - [is_pull_merged()]: is being removed
#' - [gh_pull_review()]: is being removed
#' - [gh_pull_reviews()]: replace with [view_pull_request()]
#' - [gh_pull_comment()]: is being removed
#' - [gh_pull_comments()]: replace with [view_comments()]
#' - [gh_pull_review_requests()]: is being removed
#'
NULL

#  FUNCTION: gh_get ---------------------------------------------------------------------------
#
#' Send a http GET request to the specified GitHub url
#'
#' @param url (string) URL to the GitHub API.
#' @param accept (string) The format of the returned result. Either "json", "raw" or other
#'   GitHub accepted format. Default: "json".
#' @param parse (boolean) Whether to parse the response.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @return A list of the parsed response
#'
#' @export
#'
gh_get <- function(
  url,
  accept = "json",
  parse  = TRUE,
  token  = gh_token())
{
  assert(is_url(url))
  assert(is_scalar_character(accept))
  assert(is_scalar_logical(parse))
  assert(is_scalar_character(token))

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> GET: ", url, level = 2)
  response <- curl::curl_fetch_memory(url, handle = curl::handle_setheaders(
    curl::new_handle(),
    Authorization = paste("token", token),
    Accept        = accept))

  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(curl::parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (parse) {
    info("> Parsing response", level = 4)
    response_content <- rawToChar(response$content)
    if (grepl("json$", tolower(accept))) {
      response_content <- jsonlite::fromJSON(response_content, simplifyVector = FALSE)
      message <- response_content$message
    }
  }

  if (response$status_code >= 400) {
    error(
      "\nGitHub GET request failed\n",
      "\n[Status]: ", header_values$Status,
      "\n[URL]: ", url,
      "\n[Message]: ", message)
  }

  attributes(response_content) <- c(attributes(response_content), list(header = header_values))

  info("> Done", level = 4)
  response_content
}

#  FUNCTION: gh_download_binary ---------------------------------------------------------------
#
#' Download a binary file from GitHub
#'
#' @param url (string) URL to the GitHub API.
#' @param path (string) The location to download the file to.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#'
#' @return The path to the download location
#'
#' @export
#'
gh_download_binary <- function(
  url,
  path,
  token = getOption("github.token"))
{
  {
    (is_url(url)) ||
      error("'url' must be a valid URL:\n  '", paste(url, collapse = "'\n  '"), "'")
    (is_writeable(dirname(path))) ||
      error("'path' must be a file path within a writeable directory:\n  '", paste(path, collapse = "'\n  '"), "'")
    (is_sha(token)) ||
      error("'token' must be a 40 character string:\n  '", paste(token, collapse = "'\n  '"), "'")
  }

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  info("> DOWNLOAD: ", url, level = 2)
  response <- curl::curl_fetch_disk(url, path, handle = curl::handle_setheaders(
    curl::new_handle(),
    Authorization = paste("token", token),
    Accept = "application/vnd.github.raw"))

  info("> Parsing response", level = 4)
  response_header  <- strsplit(curl::parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (!is_null(response$status) && response$status >= 400) {
    error(
      "\nGitHub GET request failed\n",
      "\n[Status]:  ", header_values[[1]],
      "\n[URL]:     ", url,
      "\n[Path]:    ", path)
  }

  attributes(path) <- c(attributes(path), list(header = header_values))

  info("> Done", level = 4)
  path
}
