is_vector <- function(x) {
  is.vector(x) && !is.list(x)
}

is_scalar <- function(x) {
  is_vector(x) && identical(length(x), 1L)
}

is_logical <- function(x) {
  is.logical(x)
}

is_boolean <- function(x) {
  is.logical(x) && is_scalar(x)
}

is_numeric <- function(x) {
  is.numeric(x)
}

is_number <- function(x) {
  is.numeric(x) && is_scalar(x)
}

is_integer <- function(x) {
  is.numeric(x) && all(x == as.integer(x))
}

is_count <- function(x) {
  is_integer(x) && is_scalar(x) && (x > 0)
}

is_character <- function(x) {
  is.character(x)
}

is_string <- function(x) {
  is.character(x) && is_scalar(x)
}

is_list <- function(x) {
  is.list(x)
}

is_data_frame <- function(x) {
  is.data.frame(x)
}

is_url <- function(x) {
  is_string(x) && grepl("^http", x)
}

is_dir <- function(x) {
  is_string(x) && dir.exists(x)
}

is_file <- function(x) {
  is_string(x) && file.exists(x) && !file.info(x)$isdir
}

is_readable <- function(x) {
  is_string(x) && file.exists(x) && file.access(path, mode = 4)[[1]] == 0
}

is_writeable <- function(x) {
  is_string(x) && file.exists(x) && file.access(x, mode = 2)[[1]] == 0
}

is_null <- function(x) {
  is.null(x)
}

is_na <- function(x) {
  is.na(x)
}
