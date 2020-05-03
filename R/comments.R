#  FUNCTION: create_comment -------------------------------------------------------------------
#
#' Create a comment in github
#'
#' This function creates a new comment for the specified gist, issue, pull request or commit
#' in GitHub. A standard comment is created for a gist or issue (note: since a pull request
#' is also an issue a standard comment is also created if the `issue` argument is set to the
#' pull request number) and a review comment is created for pull requests and commits.
#'
#' When creating a gist comment only the `body` and `gist` ID need to be specified; for an
#' issue the `body`, `issue` number and `repo` must be specified; and for a pull request or
#' commit the `body`, `pull_request` number or `commit` reference, `repo`, `path` and
#' `position` must be specified.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/gists/comments/#create-a-comment>
#' - <https://developer.github.com/v3/issues/comments/#create-a-comment>
#' - <https://developer.github.com/v3/pulls/comments/#create-a-comment>
#' - <https://developer.github.com/v3/repos/comments/#create-a-commit-comment>
#'
#' @param body (string) The content of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param issue (integer, optional) The issue number.
#' @param pull_request (integer, optional) The pull request number.
#' @param commit (string, optional) Either a SHA or branch used to identify the commit.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param path (string, optional) The path of the file to add the comment to.
#' @param position (integer, optional) The line number in the file to attach the comment to.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `create_comment()` returns a list of the comment's properties.
#'
#' **Comment Properties:**
#'
#' - **id**: The id of the comment.
#' - **body**: The content of the comment.
#' - **commit**: The commit associated with the comment (only for pull request or commit
#'   comments).
#' - **path**: The path of the file to add the comment to (only for pull request or commit
#'   comments).
#' - **position**: The line number in the file to attach the comment to (only for pull
#'   request or commit comments).
#' - **user**: The comment author's account login.
#' - **html_url**: The address of the comment's web page.
#' - **created_at**: The time and date the comment was created.
#' - **published_at**: The time and date the comment was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Create a comment on a gist
#'   create_comment(
#'     body = "This is a comment created by create_comment()",
#'     gist = "806dca6b09a39e7b6326a0c8137583e6")
#'
#'   # Create a comment on an issue
#'   create_comment(
#'     body  = "This is a comment created by create_comment()",
#'     issue = "test issue",
#'     repo  = "ChadGoymer/test-githapi")
#'
#'   # Create a comment on a pull request
#'   create_comment(
#'     body         = "This is a comment created by create_comment()",
#'     pull_request = "test pull request",
#'     commit       = "test-branch",
#'     repo         = "ChadGoymer/test-githapi",
#'     path         = "test-comments.txt",
#'     position     = 1)
#'
#'   # Create a comment on a commit
#'   create_comment(
#'     body     = "This is a comment created by create_comment()",
#'     commit   = "master",
#'     repo     = "ChadGoymer/test-githapi",
#'     path     = "README.md",
#'     position = 1)
#'
#' }
#'
#' @export
#'
create_comment <- function(
  body,
  gist,
  issue,
  pull_request,
  commit,
  repo,
  path,
  position,
  ...)
{
  assert(is_scalar_character(body), "'body' must be a string:\n  ", body)

  payload <- list(body = body)

  if (!missing(gist)) {
    assert(is_scalar_character(gist), "'gist' must be a string:\n  ", gist)

    info("Creating comment for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments")
  }
  else {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)

    if (!missing(issue)) {
      if (is_scalar_character(issue)) {
        issue <- view_issue(issue = issue, repo = repo, ...)$number
      }
      assert(is_scalar_integerish(issue), "'issue' must be a string or integer:\n  ", issue)

      info("Creating comment for issue '", issue, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "issues", issue, "comments")
    }
    else if (!missing(commit)) {
      if (!is_sha(commit)) {
        commit <- view_sha(ref = commit, repo = repo, ...)
      }
      assert(is_sha(commit), "'commit' must be a 40 character string:\n  ", commit)
      assert(is_scalar_character(path), "'path' must be a string:\n  ", path)
      assert(is_scalar_integerish(position), "'position' must be a string or integer:\n  ", position)

      payload$path <- path
      payload$position <- position

      if (missing(pull_request)) {
        info("Creating comment for commit '", commit, "' in repository '", repo, "'")
        url <- gh_url("repos", repo, "commits", commit, "comments")
      }
      else {
        if (is_scalar_character(pull_request)) {
          pull_request <- view_pull_request(pull_request = pull_request, repo = repo, ...)$number
        }
        assert(is_scalar_integerish(pull_request), "'pull_request' must be a string or integer:\n  ", pull_request)

        payload$commit_id <- commit

        info("Creating comment for pull request '", pull_request, "' in repository '", repo, "'")
        url <- gh_url("repos", repo, "pulls", pull_request, "comments")
      }
    }
    else {
      error("An 'issue', 'pull_request', 'commit' or 'gist' must be specified")
    }
  }

  comment_lst <- gh_request("POST", url = url, payload = payload, ...)

  info("Transforming results", level = 4)
  if (missing(commit) && missing(pull_request)) {
    comment_gh <- select_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- select_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}


#  FUNCTION: update_comment -------------------------------------------------------------------
#
#' Update a comment in a repository
#'
#' This function updates a comment for a gist, issue, pull request or commit. It can be used
#' to replace the content of the comment. The `repo` argument is not required if a gist is
#' specified.
#'
#' For more details see the GitHub API documentation:
#' - <https://developer.github.com/v3/gists/comments/#edit-a-comment>
#' - <https://developer.github.com/v3/issues/comments/#edit-a-comment>
#' - <https://developer.github.com/v3/pulls/comments/#edit-a-comment>
#' - <https://developer.github.com/v3/repos/comments/#update-a-commit-comment>
#'
#' @param comment (integer) The id of the comment.
#' @param body (string) The content of the comment.
#' @param gist (string, optional) The ID of the gist.
#' @param type (string, optional) Whether the comment is for a `"gist"`, `"issue"`,
#'   `"pull_request"` or `"commit"`. Default: `"gist"`.
#' @param repo (string) The repository specified in the format: `owner/repo`.
#' @param ... Parameters passed to [gh_request()].
#'
#' @return `update_comment()` returns a list of the comment properties.
#'
#' **Comment Properties:**
#'
#' - **id**: The id of the comment.
#' - **body**: The content of the comment.
#' - **commit**: The commit associated with the comment (only for pull request or commit
#'   comments).
#' - **path**: The path of the file to add the comment to (only for pull request or commit
#'   comments).
#' - **position**: The line number in the file to attach the comment to (only for pull
#'   request or commit comments).
#' - **user**: The comment author's account login.
#' - **html_url**: The address of the comment's web page.
#' - **created_at**: The time and date the comment was created.
#' - **published_at**: The time and date the comment was published.
#'
#' @examples
#' \dontrun{
#'
#'   # Update a gist comment
#'   update_comment(
#'     comment = 3281939,
#'     body    = "This comment has been updated by update_comment()",
#'     gist    = "8e5be270de9a88168372293a813543f9")
#'
#'   # Update an issue comment
#'   update_comment(
#'     comment = 622980929,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "issue",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # Update an pull request comment
#'   update_comment(
#'     comment = 418979473,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "pull_request",
#'     repo    = "ChadGoymer/test-githapi")
#'
#'   # Update a commit comment
#'   update_comment(
#'     comment = 38899533,
#'     body    = "This comment has been updated by update_comment()",
#'     type    = "commit",
#'     repo    = "ChadGoymer/test-githapi")
#'
#' }
#'
#' @export
#'
update_comment <- function(
  comment,
  body,
  gist,
  repo,
  type = "gist",
  ...)
{
  assert(is_scalar_integerish(comment), "'comment' must be a string or integer:\n  ", comment)
  assert(is_scalar_character(body), "'body' must be a string:\n  ", body)

  payload <- list(body = body)

  if (!missing(gist)) {
    info("Updating comment '", comment, "' for gist '", gist, "'")
    url <- gh_url("gists", gist, "comments", comment)
  }
  else {
    assert(is_repo(repo), "'repo' must be a string in the format 'owner/repo':\n  ", repo)
    assert(
      is_scalar_character(type) && type %in% values$comment$type,
      "'type' must be one of '", str_c(values$comment$type, collapse = "', '"), "':\n  ", type)

    if (identical(type, "issue")) {
      info("Updating issue comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "issues/comments", comment)
    }
    else if (identical(type, "pull_request")) {
      info("Updating pull request comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "pulls/comments", comment)
    }
    else {
      info("Updating commit comment '", comment, "' in repository '", repo, "'")
      url <- gh_url("repos", repo, "comments", comment)
    }
  }

  comment_lst <- gh_request("PATCH", url = url, payload = payload, ...)

  info("Transforming results", level = 4)
  if (!missing(gist) || identical(type, "issue")) {
    comment_gh <- select_properties(comment_lst, properties$issue_comment)
  }
  else {
    comment_gh <- select_properties(comment_lst, properties$commit_comment)
  }

  info("Done", level = 7)
  comment_gh
}
