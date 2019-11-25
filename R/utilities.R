#  FUNCTION: is_sha ---------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid SHA
#'
#' A valid SHA is 40 characters long and only contains the characters 0-9 & a-f.
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid SHA, FALSE otherwise
#'
#' @export
#'
is_sha <- function(x)
{
  is_scalar_character(x) &&
    identical(str_length(x), 40L) &&
    all(str_split(x, "")[[1]] %in% c(0:9, letters[1:6]))
}

#  FUNCTION: is_repo --------------------------------------------------------------------------
#
#' Checks whether the supplied object is a valid repository name
#'
#' A valid repository name is comprised of two strings separated by a "/".
#'
#' @param x Object to check
#'
#' @return TRUE if x is a valid repository name, FALSE otherwise
#'
#' @export
#'
is_repo <- function(x)
{
  is_scalar_character(x) &&
    identical(length(str_split(x, "/")[[1]]), 2L)
}
