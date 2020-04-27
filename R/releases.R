#  FUNCTION: create_release -------------------------------------------------------------------
#
#' Create a release in a repository
#'
#' This function creates a new release in the specified repository in GitHub. It must be
#' pointed at a commit by providing an existing tag or a Git reference, which can be either a
#' SHA or branch. If the tag does not exist it is created pointing at the commit; if it does
#' exist then the reference is ignored.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/releases/#create-a-release>
#'
#' @param tag (string) The name of the tag.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param name (string, optional) The name of the release.
#' @param body (string, optional) The description of the release.
#' @param ref (string, optional) Either a SHA or branch used to identify the commit. Ignored
#'   if the `tag` exists this `ref` is ignored. Default: "master".
#' @param draft (boolean, optional) Whether the release is a draft. Default: `FALSE`.
#' @param prerelease (boolean, optional) Whether the release is a pre-release.
#'   Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_release()` returns a list of the release's properties.
#'
#' **Release Properties:**
#'
#' - **id**: The id of the release.
#' - **tag**: The tags associated with the release.
#' - **name**: The name of the release.
#' - **body**: The description of the release.
#' - **commit**: The commit associated with the release.
#' - **draft**: Whether the release is draft.
#' - **prerelease**:  Whether it is a pre-release.
#' - **author_login**: The author's account login.
#' - **assets**: The name of the assets associated with the release.
#' - **html_url**: The address of the release's web page.
#' - **created_at**: The time and date the release was created.
#' - **published_at**: The time and date the release was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a release on the master branch
#'   create_release(
#'     tag  = "1.0.0",
#'     repo = "ChadGoymer/githapi",
#'     name = "Initial production release",
#'     body = "This is a release created by create_release()")
#'
#'   # Create a draft pre-release
#'   create_release(
#'     tag        = "1.0.9000",
#'     repo       = "ChadGoymer/githapi",
#'     name       = "Draft 1.1 release",
#'     body       = "This is a release created by create_release()",
#'     ref        = "dev-branch",
#'     draft      = TRUE,
#'     prerelease = TRUE)
#'
#' }
#'
#' @export
#'
create_release <- function(
  tag,
  repo,
  name,
  body,
  ref        = "master",
  draft      = FALSE,
  prerelease = FALSE,
  ...)
{
  assert(is_ref(tag), "'tag' must be a valid git reference - see help(is_ref):\n  ", tag)
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_ref(ref), "'ref' must be a valid git reference - see help(is_ref):\n  ", ref)
  assert(is_scalar_logical(draft), "'draft' must be a boolean:\n  ", draft)
  assert(is_scalar_logical(prerelease), "'prerelease' must be a boolean:\n  ", prerelease)

  payload <- list(
    tag_name         = tag,
    target_commitish = ref,
    draft            = draft,
    prerelease       = prerelease)

  if (!missing(name))
  {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload$name <- name
  }

  if (!missing(body))
  {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

  info("Creating release '", tag, "' in repository '", repo, "'")
  release_lst <- gh_url("repos", repo, "releases") %>%
    gh_request("POST", payload = payload, ...)

  info("Transforming results", level = 4)
  release_gh <- select_properties(release_lst, properties$release) %>%
    modify_list(assets = map_chr(release_lst$assets, "name"), .before = "html_url")

  info("Done", level = 7)
  structure(
    release_gh,
    class   = class(release_lst),
    url     = attr(release_lst, "url"),
    request = attr(release_lst, "request"),
    status  = attr(release_lst, "status"),
    header  = attr(release_lst, "header"))
}


#  FUNCTION: update_release -------------------------------------------------------------------
#
#' Update a release in a repository
#'
#' This function updates a release in the specified repository. It can be used to update
#' properties, such as `name` or whether it is `draft`, or it can be used to point the release
#' at a new commit by providing a Git reference, which can be either a SHA or branch.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/releases/#edit-a-release>
#'
#' @param release (string) The id or current tag of the release.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param tag (string, optional) The name of the new tag.
#' @param name (string, optional) The name of the release.
#' @param body (string, optional) The description of the release.
#' @param ref (string, optional) Either a SHA or branch used to identify the commit. Ignored
#'   if the `tag` exists this `ref` is ignored. Default: "master".
#' @param draft (boolean, optional) Whether the release is a draft. Default: `FALSE`.
#' @param prerelease (boolean, optional) Whether the release is a pre-release.
#'   Default: `FALSE`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_release()` returns a list of the release properties.
#'
#' **Release Properties:**
#'
#' - **id**: The id of the release.
#' - **tag**: The tags associated with the release.
#' - **name**: The name of the release.
#' - **body**: The description of the release.
#' - **commit**: The commit associated with the release.
#' - **draft**: Whether the release is draft.
#' - **prerelease**:  Whether it is a pre-release.
#' - **author_login**: The author's account login.
#' - **assets**: The name of the assets associated with the release.
#' - **html_url**: The address of the release's web page.
#' - **created_at**: The time and date the release was created.
#' - **published_at**: The time and date the release was published.
#'
#' @examples
#' \dontrun{
#'
#'   update_release(
#'     release = "1.0.0",
#'     repo    = "ChadGoymer/githapi",
#'     tag     = "1.0.1",
#'     name    = "Updated production release",
#'     body    = "This release has been updated by update_release()")
#'
#'   update_release(
#'     release    = 1234567,
#'     tag        = "1.0.1",
#'     repo       = "ChadGoymer/githapi",
#'     name       = "Promoted draft release",
#'     body       = "This release has been updated by update_release()",
#'     draft      = FALSE,
#'     prerelease = FALSE)
#'
#' }
#'
#' @export
#'
update_release <- function(
  release,
  repo,
  tag,
  name,
  body,
  ref        = "master",
  draft      = FALSE,
  prerelease = FALSE,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
  assert(is_ref(ref), "'ref' must be a valid git reference - see help(is_ref):\n  ", ref)
  assert(is_scalar_logical(draft), "'draft' must be a boolean:\n  ", draft)
  assert(is_scalar_logical(prerelease), "'prerelease' must be a boolean:\n  ", prerelease)

  payload <- list(
    target_commitish = ref,
    draft            = draft,
    prerelease       = prerelease)

  if (!missing(tag))
  {
    assert(is_ref(tag), "'tag' must be a valid git reference - see help(is_ref):\n  ", tag)
    payload$tag_name <- tag
  }

  if (!missing(name))
  {
    assert(is_scalar_character(name), "'name' must be a string:\n  ", name)
    payload$name <- name
  }

  if (!missing(body))
  {
    assert(is_scalar_character(body), "'body' must be a string:\n  ", body)
    payload$body <- body
  }

  if (is_scalar_character(release))
  {
    release <- view_release(release = release, repo = repo, ...)$id
  }
  assert(is_scalar_integerish(release), "'release' must be an integer or a string:\n  ", release)

  info("Updating release '", release, "' in repository '", repo, "'")
  release_lst <- gh_url("repos", repo, "releases", release) %>%
    gh_request("PATCH", payload = payload, ...)

  info("Transforming results", level = 4)
  release_gh <- select_properties(release_lst, properties$release) %>%
    modify_list(assets = map_chr(release_lst$assets, "name"), .before = "html_url")

  info("Done", level = 7)
  structure(
    release_gh,
    class   = class(release_lst),
    url     = attr(release_lst, "url"),
    request = attr(release_lst, "request"),
    status  = attr(release_lst, "status"),
    header  = attr(release_lst, "header"))
}


#  FUNCTION: .view_releases -------------------------------------------------------------------
#
#' View releases within a repository
#'
#' `.view_releases()` summarises releases in a table with the properties as columns and a row
#' for each release in the repository. `view_release()` returns a list of all properties for
#' a single release. `browse_release()` opens the web page for the release in the default
#' browser.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/releases/#list-releases-for-a-repository>
#' - <https://developer.github.com/v3/repos/releases/#get-a-single-release>
#' - <https://developer.github.com/v3/repos/releases/#get-a-release-by-tag-name>
#'
#' @param release (string) The id or tag of the release.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param n_max (integer, optional) Maximum number to return. Default: `1000`.
#' @param ... Parameters passed to [gh_page()].
#'
#' @return `.view_releases()` returns a tibble of release properties. `view_release()`
#'   returns a list of properties for a single release. `browse_release` opens the
#'   default browser on the release page and returns the URL.
#'
#' **Release Properties:**
#'
#' - **id**: The id of the release.
#' - **tag**: The tags associated with the release.
#' - **name**: The name of the release.
#' - **body**: The description of the release.
#' - **commit**: The commit associated with the release.
#' - **draft**: Whether the release is draft.
#' - **prerelease**:  Whether it is a pre-release.
#' - **author_login**: The author's account login.
#' - **assets**: The name of the assets associated with the release.
#' - **html_url**: The address of the release's web page.
#' - **created_at**: The time and date the release was created.
#' - **published_at**: The time and date the release was published.
#'
#' @examples
#' \dontrun{
#'
#'   # View all releases in a repository
#'   .view_releases("ChadGoymer/githapi")
#'
#'   # View a single release
#'   view_release("1.0.1", "ChadGoymer/githapi")
#'
#'   # Browse the release web page
#'   browse_release("1.0.1", "ChadGoymer/githapi")
#'
#' }
#'
#' @export
#'
.view_releases <- function(
  repo,
  n_max = 1000,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  info("Viewing releases for repository '", repo, "'")
  releases_lst <- gh_url("repos", repo, "releases") %>%
    gh_page(n_max = n_max, ...)

  info("Transforming results", level = 4)
  releases_gh <- bind_properties(releases_lst, properties$release) %>%
    add_column(assets = map(releases_lst, ~ map_chr(.$assets, "name")), .before = "html_url")

  info("Done", level = 7)
  releases_gh
}


#  FUNCTION: view_release ---------------------------------------------------------------------
#
#' @rdname dot-view_releases
#' @export
#'
view_release <- function(
  release,
  repo,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  if (is_scalar_integerish(release))
  {
    url <- gh_url("repos", repo, "releases", release)
  }
  else if (is_ref(release))
  {
    url <- gh_url("repos", repo, "releases/tags", release)
  }
  else
  {
    error("'release' must be either an integer or a valid git reference - see help(is_ref):\n  ", release)
  }

  info("Viewing release '", release, "' in repository '", repo, "'")
  release_lst <- gh_request("GET", url = url, ...)

  info("Transforming results", level = 4)
  release_gh <- select_properties(release_lst, properties$release) %>%
    modify_list(assets = map_chr(release_lst$assets, "name"), .before = "html_url")

  info("Done", level = 7)
  structure(
    release_gh,
    class   = class(release_lst),
    url     = attr(release_lst, "url"),
    request = attr(release_lst, "request"),
    status  = attr(release_lst, "status"),
    header  = attr(release_lst, "header"))
}


#  FUNCTION: browse_release -------------------------------------------------------------------
#
#' @rdname dot-view_releases
#' @export
#'
browse_release <- function(
  release,
  repo,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  if (is_scalar_integerish(release))
  {
    url <- gh_url("repos", repo, "releases", release)
  }
  else if (is_ref(release))
  {
    url <- gh_url("repos", repo, "releases/tags", release)
  }
  else
  {
    error("'release' must be either an integer or a valid git reference - see help(is_ref):\n  ", release)
  }

  info("Browsing release '", release, "' in repository '", repo, "'")
  release <- gh_request("GET", url = url, ...)
  httr::BROWSE(release$html_url)

  info("Done", level = 7)
  structure(
    release$html_url,
    class   = c("github", "character"),
    url     = attr(release, "url"),
    request = attr(release, "request"),
    status  = attr(release, "status"),
    header  = attr(release, "header"))
}


#  FUNCTION: delete_release -------------------------------------------------------------------
#
#' Delete a release from a repository
#'
#' This function deletes a release from a repository, as long as you have appropriate
#' permissions. Care should be taken as it will not be recoverable.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/repos/releases/#delete-a-release>
#'
#' @param release (string) The id or tag of the release.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `delete_release()` returns a TRUE if successfully deleted.
#'
#' @examples
#' \dontrun{
#'   delete_release("1.0.0", repo = "ChadGoymer/githapi")
#' }
#'
#' @export
#'
delete_release <- function(
  release,
  repo,
  ...)
{
  assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

  if (is_scalar_character(release))
  {
    release <- view_release(release = release, repo = repo, ...)$id
  }
  assert(is_scalar_integerish(release), "'release' must be an integer or a string:\n  ", release)

  info("Deleting release '", release, "' in repository '", repo, "'")
  response <- gh_url("repos", repo, "releases", release) %>%
    gh_request("DELETE", ...)

  info("Done", level = 7)
  structure(
    TRUE,
    class   = c("github", "logical"),
    url     = attr(response, "url"),
    request = attr(response, "request"),
    status  = attr(response, "status"),
    header  = attr(response, "header"))
}
