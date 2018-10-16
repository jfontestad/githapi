field_names <- function(fields) {
  names <- sapply(fields, paste, collapse = "_")
  if (!is_null(names(fields))) {
    for (field in seq_along(fields)) {
      if (!identical(names(fields)[[field]], "")) {
        names[[field]] <- names(fields)[[field]]
      }
    }
  }
  names
}

convert_field <- function(field, as) {
  if (is_na(as)) {
    field
  } else if (identical(as, "POSIXct")) {
    as.POSIXct(field)
  } else {
    as(field, as)
  }
}

select_fields <- function(x, fields) {
  assert(is_list(x))
  assert(is_list(fields), length(fields) > 0)

  selected_fields <- lapply(fields, x, FUN = function(field, x) {
    selected_field <- x
    for (level in seq_along(field)) {
      if (is_null(selected_field[[field[[level]]]]))
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
  assert(is_list(x))
  assert(is_list(fields), length(fields) > 0)

  conversions <- sapply(fields, function(f) f["as"])
  raw_fields <- lapply(fields, function(f) if (is_null(names(f))) f else f[names(f) != "as"])

  if (identical(length(x), 0L)) {
    names(fields) <- field_names(fields)
    binded_fields <- bind_rows(lapply(field_names(fields), function(f) logical()))
  } else {
    binded_fields <- bind_rows(lapply(x, select_fields, fields = raw_fields))
  }

  for (col in 1:ncol(binded_fields)) {
    if (!is_na(conversions[[col]])) {
      if (identical(conversions[[col]], "datetime"))
        binded_fields[[col]] <- as.POSIXct(binded_fields[[col]])
      else
        binded_fields[[col]] <- as(binded_fields[[col]], conversions[[col]])
    }
  }

  binded_fields
}
