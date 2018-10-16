# FUNCTION: parse_vector ----------------------------------------------------------------------
#
# Parse a vector to aid in producing tests
#
# Unnamed vectors are quoted and separated with the `sep` argument. Named vectors have the
# format `name = "value"` separated by the `sep` argument.
#
# @param v (vector) The vector to parse
# @param sep (string) The separator
#
# @return The printed result of parsing the input
#
parse_vector <- function(v, sep = ",\n") {
  assert(is_vector(v))
  assert(is_string(sep))

  if (!is_null(names(v))) {
    padded_names <- format(names(v), width = max(nchar(names(v))))
    v <- paste0(padded_names, " = \"", v, "\"")
  } else {
    v <- paste0("\"", v, "\"")
  }
  cat(paste(v, collapse = sep))
}

# FUNCTION: info ------------------------------------------------------------------------------
#
# Display a message, and record it in a log file.
#
# `info()` is similar to [base::message()], but it also has the option to write to a log file.
# It also allows options for the `type` and `level` of messages outputted. The `type` refers
# to either "INFO", "WARNING" or "ERROR" and the level refers to the amount of detail to
# display to the user. For example, you may just want to display warnings and error messages
# from the top level, or you may want to display all messages from all levels for debugging.
#
# @param ... (strings) message to be displayed or written to file.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
info <- function(
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  stopifnot((is_null(debug_types) || is_character(debug_types)) && all(debug_types %in% c("INFO", "WARNING", "ERROR")))
  stopifnot(is_count(level) && level <= 5)
  stopifnot(is_count(debug_level) && debug_level <= 5)
  stopifnot(is_string(log_path))

  if ("INFO" %in% debug_types && level <= debug_level) {
    if (!identical(log_path, "")) {
      if (!is_dir(basename(log_path))) dir.create(basename(log_path), recursive = TRUE)
      if (!is_writeable(log_path)) stop("Specified 'log_path' is not writeable: '", log_path, "'")

      log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
      write(paste0("[", Sys.time(), "] ", "INFO", ": ", ...), file.path(log_path, log_name), append = TRUE)
    }
    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...))
  }

  invisible()
}

# FUNCTION: warn ------------------------------------------------------------------------------
#
# Display a warning, and record it in a log file.
#
# `warn()` is similar to [base::warning()], but it also has the option to write to a log file.
# It also allows options for the `type` and `level` of messages outputted. The `type` refers
# to either "INFO", "WARNING" or "ERROR" and the level refers to the amount of detail to
# display to the user. For example, you may just want to display warnings and error messages
# from the top level, or you may want to display all messages from all levels for debugging.
#
# @param ... (strings) message to be displayed or written to file.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
warn <- function(
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  stopifnot((is_null(debug_types) || is_character(debug_types)) && all(debug_types %in% c("INFO", "WARNING", "ERROR")))
  stopifnot(is_count(level) && level <= 5)
  stopifnot(is_count(debug_level) && debug_level <= 5)
  stopifnot(is_string(log_path))

  if ("WARNING" %in% debug_types && level <= debug_level) {
    if (!identical(log_path, "")) {
      if (!is_dir(basename(log_path))) dir.create(basename(log_path), recursive = TRUE)
      if (!is_writeable(log_path)) stop("Specified 'log_path' is not writeable: '", log_path, "'")

      log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
      write(paste0("[", Sys.time(), "] ", "WARNING", ": ", ...), file.path(log_path, log_name), append = TRUE)
    }
    warning(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...), call. = FALSE)
  }

  invisible()
}

# FUNCTION: error -----------------------------------------------------------------------------
#
# Display an error, and record it in a log file.
#
# `error()` is similar to [base::stop()], but it also has the option to write to a log file.
# It also allows options for the `type` and `level` of messages outputted. The `type` refers
# to either "INFO", "WARNING" or "ERROR" and the level refers to the amount of detail to
# display to the user. For example, you may just want to display warnings and error messages
# from the top level, or you may want to display all messages from all levels for debugging.
#
# @param ... (strings) message to be displayed or written to file.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
error <- function(
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  stopifnot((is_null(debug_types) || is_character(debug_types)) && all(debug_types %in% c("INFO", "WARNING", "ERROR")))
  stopifnot(is_count(level) && level <= 5)
  stopifnot(is_count(debug_level) && debug_level <= 5)
  stopifnot(is_string(log_path))

  if ("ERROR" %in% debug_types && level <= debug_level) {
    if (!identical(log_path, "")) {
      if (!is_dir(basename(log_path))) dir.create(basename(log_path), recursive = TRUE)
      if (!is_writeable(log_path)) stop("Specified 'log_path' is not writeable: '", log_path, "'")

      log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
      write(paste0("[", Sys.time(), "] ", "ERROR", ": ", ...), file.path(log_path, log_name), append = TRUE)
    }
    stop(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...), call. = FALSE)
  }
}

# FUNCTION: info_if ---------------------------------------------------------------------------
#
# Display a message, and record in a log file, if a condition is true.
#
# @param condition (boolean) The condition to check.
# @param ... (strings, optional) message to write when condition is true. If not supplied a
#   generic message is produced.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
info_if <- function(
  condition,
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)
    calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
    } else {
      msg <- as.character(msg)
    }
    info(
      "In ", calling_function, "(): ", msg,
      level = level, debug_types  = debug_types, debug_level = debug_level, log_path = log_path)
  }
}

# FUNCTION: warn_if ---------------------------------------------------------------------------
#
# Display a warning, and record in a log file, if a condition is true.
#
# @param condition (boolean) The condition to check.
# @param ... (strings, optional) message to write when condition is true. If not supplied a
#   generic message is produced.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
warn_if <- function(
  condition,
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)
    calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
    } else {
      msg <- as.character(msg)
    }
    warn(
      "In ", calling_function, "(): ", msg,
      level = level, debug_types  = debug_types, debug_level = debug_level, log_path = log_path)
  }
}

error_if <- function(
  condition,
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  if (isTRUE(condition)) {
    uneval_condition <- substitute(condition)
    calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is true")
    } else {
      msg <- as.character(msg)
    }
    error(
      "In ", calling_function, "(): ", msg,
      level = level, debug_types  = debug_types, debug_level = debug_level, log_path = log_path)
  }
}

# FUNCTION: assert ----------------------------------------------------------------------------
#
# Display an error, and record in a log file, if a condition is false.
#
# @param condition (boolean) The condition to check.
# @param ... (strings, optional) message to write when condition is true. If not supplied a
#   generic message is produced.
# @param level (integer, optional) The level of the message, from 1 to 5. Default: 1.
# @param debug_types (character, optional) The type to write or display. Must either NULL or one
#  or more from "INFO", "WARNING" or "ERROR". Default: set in the option "githapi.debug_types".
# @param debug_level (integer, optional) The maximum level of messages to output. Default: set
#   in the option "githapi.debug_level".
# @param log_path (string, optional) The file path to the text log file. If set to "", then no
#   logs are written.
#
# @return NULL
#
# @export
#
assert <- function(
  condition,
  ...,
  level       = 1,
  debug_types = getOption("githapi.debug_types"),
  debug_level = getOption("githapi.debug_level"),
  log_path    = getOption("githapi.logs"))
{
  if (!isTRUE(condition)) {
    uneval_condition <- substitute(condition)
    calling_function <- deparse(sys.calls()[[sys.nframe() - 1]][[1]])

    msg <- list(...)
    if (identical(length(msg), 0L)) {
      msg <- paste(deparse(uneval_condition), "is not true")
    } else {
      msg <- as.character(msg)
    }
    error(
      "In ", calling_function, "(): ", msg,
      level = level, debug_types  = debug_types, debug_level = debug_level, log_path = log_path)
  }
}

# FUNCTION: collate_errors --------------------------------------------------------------------
#
# Search a list for errors and collate into a nice message
#
# @param x (list) List of things to check for errors.
# @param msg (string) Message to prepend to the error message.
# @param on_error (string, optional) The action to take when an error occurs. Options are:
#  - `error`: Throw an error and stop (default).
#  - `warn`: Show a warning and continue.
#  - `info`: Show a message and continue.
#
# @return NULL - produces an error, warning or message as a side effect.
#
collate_errors <- function(x, msg, on_error = "error")
{
  stopifnot(is_list(x))
  stopifnot(is_string(msg))
  stopifnot(is_string(on_error) && on_error %in% c("info", "warn", "error"))

  error_msg <- x[sapply(x, is, "error")] %>%
    sapply(getElement, "message") %>%
    sub("^\n", "", .) %>%
    paste0("\n'", names(x)[sapply(x, is, "error")], "': ", .) %>%
    paste(collapse = "\n")

  if (!missing(msg)) {
    error_msg <- paste0(msg, "\n", error_msg)
  }

  switch(
    on_error,
    error = error(error_msg),
    warn = warn(error_msg),
    info = msg(error_msg))
}
