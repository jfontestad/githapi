#  PACKAGE: githapi ---------------------------------------------------------------------------
#
#' User-friendly access to the GitHub API for R, consistent with the tidyverse.
#'
#' GitHub is a wildly popular host for version controlled code, but also data. This package
#' provides a user-friendly way to access content within GitHub through v3 of its
#' [API](https://developer.github.com/v3/). The functions are also consistent with the
#' [tidyverse](http://www.tidyverse.org/) approach and return tibbles where possible.
#'
#' Detailed documentation can be found at: <http://www.goymer.me.uk/githapi>
#'
#' @section Authorisation:
#'
#' In order to access repositories in GitHub authentication is required, by setting a
#' [Personal Access Token](https://help.github.com/articles/creating-a-personal-access-token-for-the-command-line/).
#' githapi identifies the personal token in two possible ways:
#'
#' 1.  On package load - if set as an environment variable named either "GITHUB_TOKEN" or "GITHUB_PAT"
#' 2.  At any time - if set as an R option called "github.token"
#'
#' Setting an R option will override the environment variable.
#'
#' @section Usage:
#'
#' `githapi` works best if used as part of the [tidyverse](http://www.tidyverse.org/).
#'
#' ```r
#' library(tidyverse)
#' library(githapi)
#' ```
#'
#' @section Getting information about a repository:
#'
#' `githapi` provides many functions for extracting information from a repository. For example, To
#' get information about the branches use [view_branches()]:
#'
#' ```r
#' view_branches("ChadGoymer/test-githapi")
#' ```
#' ```r
#' #> # A tibble: 4 x 6
#' #>   name     ref        url                 object_sha      object_type object_url
#' #>   <chr>    <chr>      <chr>               <chr>           <chr>       <chr>
#' #> 1 master   refs/head~ https://api.github~ bd1be3ea52d69d~ commit      https://api.github.com/r~
#' #> 2 test-br~ refs/head~ https://api.github~ 244116707ad9e2~ commit      https://api.github.com/r~
#' #> 3 test-up~ refs/head~ https://api.github~ 61c19d6edb1b9e~ commit      https://api.github.com/r~
#' #> 4 unedite~ refs/head~ https://api.github~ 37d97bf9e93418~ commit      https://api.github.com/r~
#' ```
#'
#' Similarly, to get the history of commits for a particular branch use [view_history()]:
#'
#' ```r
#' view_history("master", "ChadGoymer/test-githapi")
#' ```
#' ```r
#' #> # A tibble: 1,000 x 12
#' #>    sha   message author_name author_email committer_name committer_email date
#' #>    <chr> <chr>   <chr>       <chr>        <chr>          <chr>           <dttm>
#' #>  1 bd1b~ Testin~ Jane Jones  jane.jones@~ Bob Smith      bob.smith@acme~ 2019-02-18 07:46:08
#' #>  2 d12b~ Testin~ Jane Jones  jane.jones@~ Bob Smith      bob.smith@acme~ 2019-02-18 07:46:07
#' #>  3 9d4c~ Testin~ Bob Smith   bob.smith@a~ Jane Jones     jane.jones@acm~ 2019-02-18 07:46:06
#' #>  4 3cb8~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:59
#' #>  5 06f3~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:58
#' #>  6 aaec~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:58
#' #>  7 523c~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:57
#' #>  8 3a95~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:56
#' #>  9 a032~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:53
#' #> 10 9b77~ Testin~ Chad Goymer chad.goymer~ Chad Goymer    chad.goymer@gm~ 2019-02-18 07:45:52
#' #> # ... with 990 more rows, and 5 more variables: url <chr>, tree_sha <chr>, tree_url <chr>,
#' #> #   parent_sha <list>, parent_url <list>
#' ```
#'
#' Note that, by default, when viewing many items, only the last 1000 are returned. If you wish to
#' view more you can set the `n_max` parameter. To get information about the files in a particular
#' commit use [view_files()]:
#'
#' ```r
#' view_files(ref = "master", repo = "ChadGoymer/test-githapi")
#' ```
#' ```r
#' #> # A tibble: 4 x 9
#' #>   name   path   sha       size type  url          html_url     git_url        download_url
#' #>   <chr>  <chr>  <chr>    <int> <chr> <chr>        <chr>        <chr>          <chr>
#' #> 1 READM~ READM~ 75b08f1~    65 file  https://api~ https://git~ https://api.g~ https://raw.gith~
#' #> 2 test-~ test-~ bbac77b~     0 dir   https://api~ https://git~ https://api.g~ NA
#' #> 3 test-~ test-~ f933c54~    22 file  https://api~ https://git~ https://api.g~ https://raw.gith~
#' #> 4 test-~ test-~ 68f9ad1~    63 file  https://api~ https://git~ https://api.g~ https://raw.gith~
#' ```
#'
#' Note that we have used a branch name here rather than a commit. That is because most functions
#' in `githapi` allow git references to identify commits. A git reference is either a branch, in
#' which case the head commit on the branch is used, a tag or a SHA-1 of the commit.
#'
#' Information about tags and releases can also be viewed by using [view_tags()] and
#' [view_releases()].
#'
#' ```r
#' view_tags("ChadGoymer/test-githapi")
#' ```
#' ```r
#' #> # A tibble: 1 x 6
#' #>   name  ref      url                  object_sha       object_type object_url
#' #>   <chr> <chr>    <chr>                <chr>            <chr>       <chr>
#' #> 1 0.0.0 refs/ta~ https://api.github.~ cbd94cf24a4c627~ commit      https://api.github.com/repo~
#' ```
#'
#' ```r
#' view_releases("ChadGoymer/test-githapi")
#' ```
#' ```r
#' #> # A tibble: 1 x 13
#' #>       id tag_name name  body  author_login draft prerelease target_commitish
#' #>    <int> <chr>    <chr> <chr> <chr>        <lgl> <lgl>      <chr>
#' #> 1 1.37e7 0.0.0    Init~ This~ ChadGoymer   FALSE FALSE      cbd94cf24a4c627~
#' #> # ... with 5 more variables: created_at <dttm>, published_at <dttm>, assets <list>,
#' #> #   zipball_url <chr>, url <chr>
#' ```
#'
#' @section Downloading files from a repository:
#'
#' In addition to getting information about the contents of a repository, `githapi` can also
#' download files and commits. To download individual files use [download_files()]:
#'
#' ```r
#' temp_path <- file.path(tempdir(), "githapi")
#'
#' download_files(
#'   paths    = c("README.md", "test-file.txt"),
#'   location = temp_path,
#'   repo     = "ChadGoymer/test-githapi")
#' ```
#'
#' To download an entire commit use [download_commit()]:
#'
#' ```r
#' download_commit(
#'   ref  = "master",
#'   path = temp_path,
#'   repo = "ChadGoymer/test-githapi")
#' ```
#'
#' You can also read text files from a commit directly using [read_files()]:
#'
#' ```r
#' files <- read_files(c("README.md", "test-file.txt"), "ChadGoymer/test-githapi")
#' ```
#'
#' and source R scripts directly from GitHub using [source_files()]:
#'
#' ```r
#' source_files("test-source.R", "ChadGoymer/test-githapi")
#' ```
#'
#' @section Updating a repository:
#'
#' `githapi` also provides a set of functions for updating repsitories, for adding branches, tags as
#' well as new commits.
#'
#' To create or update a file, or small number of text files, in a repository use [create_files()]
#' or [update_files()]:
#'
#' ```r
#' create_files(
#'   paths    = c("aaaa.txt", "bbbb.txt"),
#'   contents = c("Created to test:\n\n  `create_files()`", "Created to test:\n\n  `create_files()`"),
#'   messages = "Testing create_files()",
#'   repo     = "ChadGoymer/test-githapi")
#' ```
#'
#' ```r
#' update_files(
#'   paths    = c("aaaa.txt", "bbbb.txt"),
#'   contents = c("Updated to test:\n\n  `update_files()`", "Updated to test:\n\n  `update_files()`"),
#'   messages = "Testing update_files()",
#'   repo     = "ChadGoymer/test-githapi")
#' ```
#'
#' These functions create a new commit per file and specify the contents as strings. To upload many
#' files, and/or binary files, use [upload_commit()]:
#'
#' ```r
#' upload_commit(
#'   branch  = "master",
#'   message = "Commit made with upload_commit",
#'   path    = system.file("test-data/upload-tree", package = "githapi"),
#'   parents = "master",
#'   repo    = "ChadGoymer/test-githapi")
#' ```
#'
#' This function, by default, makes a new commit containing the contents of the specified directory.
#' If you wish to only specify the files to change then set the `replace` parameter to `FALSE`
#' (see documentation for details).
#'
#' Finally, tags and releases can be created using [create_tags()] and [create_releases()]
#' respectively:
#'
#' ```r
#' create_tags(
#'   tags = c("aaa", "bbb"),
#'   shas = c("cbd94cf24a4c62761b3ae59ca3c69f868591cf7d", "310c21d3f1601a46e014e68e94814b23406bf574"),
#'   repo = "ChadGoymer/test-githapi")
#' ```
#'
#' ```r
#' create_releases(
#'   tags    = c("aaa", "bbb"),
#'   commits = c("310c21d3f1601a46e014e68e94814b23406bf574", "32d3c5c4f6aba7ae9679480407e1b9f94ad04843"),
#'   names   = c("AAA", "BBB"),
#'   bodies  = c("Created for testing: aaa", "Created for testing: bbb"),
#'   repo    = "ChadGoymer/test-githapi")
#' ```
#'
#' @import curl jsonlite dplyr
#' @importFrom methods as is
#' @importFrom utils unzip
#'
#' @docType package
#' @name githapi
NULL

#  FUNCTION: is_sha ---------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid SHA
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid SHA, FALSE otherwise
#'
#' @export
#'
is_sha <- function(x) {
  is_string(x) && identical(nchar(x), 40L) && grepl("[0-9a-f]", x)
}

#  FUNCTION: is_repo --------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid repo name
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid repo, FALSE otherwise
#'
#' @export
#'
is_repo <- function(x) {
  is_string(x) && identical(length(strsplit(x, "/")[[1]]), 2L)
}

#  FUNCTION: gh_token -------------------------------------------------------------------------
#
#' Retrieve the GitHub personal access token
#'
#' The token is retreived by looking in the following locations, in order:
#'
#' 1. `github.token` option
#' 2. `GITHUB_TOKEN` environment variable
#' 3. `GITHUB_PAT` environment variable
#'
#' @return The token as a string
#'
gh_token <- function() {
  token <- getOption("github.token")
  if (is_null(token)) {
    token <- Sys.getenv("GITHUB_TOKEN")
  }
  if (identical(token, "")) {
    token <- Sys.getenv("GITHUB_PAT")
  }
  if (identical(token, "")) {
    error("Cannot find GitHub token. Please set the environment variable \"GITHUB_TOKEN\".")
  }
  token
}

#  FUNCTION: gh_url ----------------------------------------------------------------------------
#
#' Build the URL for the github API
#'
#' @param ... (strings, optional) unnamed strings are built up into a URL path and named
#'   parameters are added as queries.
#' @param api (string, optional) The URL of GitHub's API. Default: the value stored in the
#'   environment variable `GITHUB_API` or `https://api.github.com`.
#'
#' @return Valid URL (string)
#'
#' @export
#'
gh_url <- function(
  ...,
  api = getOption("github.api"))
{
  assert(is_url(api))

  url  <- api
  dots <- list(...) %>% remove_missing() %>% unlist()

  if (is_null(names(dots))) {
    path <- paste(dots, collapse = "/")
  } else {
    path <- paste(dots[names(dots) == ""], collapse = "/")
  }
  if (!identical(length(path), 0L))
    url <- file.path(url, path)

  query <- dots[names(dots) != "" & !sapply(dots, is_null)]
  if (!identical(length(query), 0L)) {
    url <- paste0(url, "?", paste(names(query), query, sep = "=", collapse = "&"))
  }

  url
}

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
  assert(is_string(accept))
  assert(is_boolean(parse))
  assert(is_string(token))

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> GET: ", url, level = 2)
  response <- curl_fetch_memory(url, handle = handle_setheaders(
    new_handle(),
    Authorization = paste("token", token),
    Accept        = accept))

  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (parse) {
    info("> Parsing response", level = 2)
    response_content <- rawToChar(response$content)
    if (grepl("json$", tolower(accept))) {
      response_content <- fromJSON(response_content, simplifyVector = FALSE)
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

  info("> Done", level = 2)
  response_content
}

#  FUNCTION: gh_page --------------------------------------------------------------------------
#
#' Get and parse the contents of multiple pages from a github http request
#'
#' @param url (string) URL for GitHub API endpoint.
#' @param accept (string) The format of the returned result. Either "json", "raw" or other
#'   GitHub accepted format. Default: "json".
#' @param n_max (integer, optional) Maximum number to return. Default: 1000.
#' @param token (string, optional) The personal access token for GitHub authorisation. Default:
#'   value stored in the environment variable `GITHUB_TOKEN` or `GITHUB_PAT`.
#' @param ... Parameters passed to [gh_get()].
#'
#' @return A list of the combined parsed responses
#'
#' @export
#'
gh_page <- function(
  url,
  accept = "json",
  n_max  = 1000L,
  token  = gh_token(),
  ...)
{
  assert(is_url(url))
  assert(is_string(accept))
  assert(is_integer(n_max), n_max > 0)
  assert(is_string(token))

  per_page <- min(n_max, 100)
  if (grepl("\\?", url)) {
    url <- paste0(url, "&per_page=", per_page)
  } else {
    url <- paste0(url, "?per_page=", per_page)
  }

  response <- list()

  while (length(response) < n_max) {
    page <- gh_request("GET", url = url, accept = accept, token = token, ...)
    response <- c(response, page)
    if (length(response) >= n_max) {
      info("> Returned ", length(response), level = 2)
      return(response[1:n_max])
    }
    if (is_null(attributes(page)[["header"]][["Link"]])) {
      info("> Returned ", length(response), level = 2)
      return(response)
    }
    links <- strsplit(attributes(page)[["header"]][["Link"]], ", ")[[1]]
    if (!any(grepl("next", links))) {
      info("> Returned ", length(response), level = 2)
      return(response)
    }
    url <- sub("<", "", strsplit(links[grepl("next", links)], ">")[[1]][[1]])
  }
}

#  FUNCTION: gh_request -----------------------------------------------------------------------
#
#' Send a http request to the specified GitHub url
#'
#' @param type (string) The type of the request. Either "GET", "POST", "DELETE" or "PATCH".
#' @param url (string) URL to the GitHub API.
#' @param payload (list) The information to send, which is converted to JSON.
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
gh_request <- function(
  type,
  url,
  payload = NULL,
  accept  = "json",
  parse   = TRUE,
  token   = gh_token())
{
  assert(is_string(type) && type %in% c("GET", "POST", "DELETE", "PATCH", "PUT"))
  assert(is_url(url))
  assert(is_string(accept))
  assert(is_boolean(parse))
  assert(is_string(token))

  if (identical(accept, "raw")) {
    accept <- "application/vnd.github.raw"
  } else if (identical(accept, "json")) {
    accept <- "application/vnd.github.v3+json"
  }

  info("> ", type, ": ", url, level = 2)
  h <- new_handle()
  if (!identical(type, "GET")) {
    h <- handle_setopt(h, customrequest = type)
  }
  if (!is_null(payload)) {
    assert(is_list(payload))
    h <- handle_setopt(h, COPYPOSTFIELDS = jsonlite::toJSON(payload, auto_unbox = TRUE))
  }
  h <- handle_setheaders(h, Authorization = paste("token", token), Accept = accept)

  response <- curl_fetch_memory(url, handle = h)
  response_content <- response$content
  message <- NULL

  response_header  <- strsplit(parse_headers(response$header), ": ")
  header_values <- lapply(response_header, function(h) ifelse(length(h) == 1, h[[1]], h[[2]])) %>%
    set_names(sapply(response_header, function(h) h[[1]]))

  if (parse) {
    info("> Parsing response", level = 2)
    response_content <- rawToChar(response$content)
    if (!identical(response_content, "") && grepl("json$", tolower(accept))) {
      response_content <- fromJSON(response_content, simplifyVector = FALSE)
      message <- response_content$message
    } else {
      message <- "None"
    }
  }

  if (response$status_code >= 400) {
    error(
      "\nGitHub ", type, " request failed\n",
      "\n[Status]:  ", header_values$Status,
      "\n[URL]:     ", url,
      "\n[Message]: ", message)
  }

  attributes(response_content) <- c(attributes(response_content), list(header = header_values))

  info("> Done", level = 2)
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
  token = gh_token())
{
  assert(is_url(url))
  assert(is_writeable(dirname(path)))
  assert(is_string(token))

  path <- normalizePath(path, winslash = "/", mustWork = FALSE)

  info("> DOWNLOAD: ", url, level = 2)
  response <- curl_fetch_disk(url, path, handle = handle_setheaders(
    new_handle(),
    Authorization = paste("token", token),
    Accept = "application/vnd.github.raw"))

  info("> Parsing response", level = 2)
  response_header  <- strsplit(parse_headers(response$header), ": ")
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

  info("> Done", level = 2)
  path
}
