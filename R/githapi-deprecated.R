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
#'
NULL
