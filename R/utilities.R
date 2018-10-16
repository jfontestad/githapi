parse_vector <- function(v, sep = ",\n") {
  if (!is.null(names(v))) {
    padded_names <- format(names(v), width = max(nchar(names(v))))
    v <- paste0(padded_names, " = \"", v, "\"")
  } else {
    v <- paste0("\"", v, "\"")
  }
  cat(paste(v, collapse = sep))
}

msg <- function(
  ...,
  show_msg = getOption("githapi.debug_messages"),
  log_path = getOption("githapi.log_path"))
{
  if (show_msg) {
    message(paste0("[", format(Sys.time(), "%H:%M:%S"), "] ", ...))
  }
  if (!is.null(log_path) && is_writeable(log_path)) {
    log_name <- paste0("githapi-", Sys.info()[["login"]], "-", format(Sys.time(), "%Y-%m-%d"), ".log")
    write(paste0("[", Sys.time(), "] INFO: ", ...), file.path(log_path, log_name), append = TRUE)
  }
}

}

field_names <- function(fields) {
  names <- sapply(fields, paste, collapse = "_")
  if (!is.null(names(fields))) {
    for (field in seq_along(fields)) {
      if (!identical(names(fields)[[field]], "")) {
        names[[field]] <- names(fields)[[field]]
      }
    }
  }
  names
}

convert_field <- function(field, as) {
  if (is.na(as)) {
    field
  } else if (identical(as, "POSIXct")) {
    as.POSIXct(field)
  } else {
    as(field, as)
  }
}

select_fields <- function(x, fields) {
  stopifnot(is.list(x))
  stopifnot(is.list(fields), length(fields) > 0)

  selected_fields <- lapply(fields, x, FUN = function(field, x) {
    selected_field <- x
    for (level in seq_along(field)) {
      if (is.null(selected_field[[field[[level]]]]))
        return(NA)
      else
        selected_field <- selected_field[[field[[level]]]]
    }
    selected_field
  })

  names(selected_fields) <- field_names(fields)
  selected_fields
}

bind_fields <- function(x, fields) {
  stopifnot(is.list(x))
  stopifnot(is.list(fields), length(fields) > 0)

  conversions <- sapply(fields, function(f) f["as"])
  raw_fields <- lapply(fields, function(f) if (is.null(names(f))) f else f[names(f) != "as"])

  if (identical(length(x), 0L)) {
    names(fields) <- field_names(fields)
    binded_fields <- bind_rows(lapply(field_names(fields), function(f) logical()))
  } else {
    binded_fields <- bind_rows(lapply(x, select_fields, fields = raw_fields))
  }

  for (col in 1:ncol(binded_fields)) {
    if (!is.na(conversions[[col]])) {
      if (identical(conversions[[col]], "datetime"))
        binded_fields[[col]] <- as.POSIXct(binded_fields[[col]])
      else
        binded_fields[[col]] <- as(binded_fields[[col]], conversions[[col]])
    }
  }

  binded_fields
}
