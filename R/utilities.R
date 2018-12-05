map <- function(x, f, ..., use_names = TRUE) {
  result <- sapply(X = x, FUN = f, ..., simplify = FALSE, USE.NAMES = use_names)
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

map_vec <- function(x, f, ..., use_names = TRUE) {
  result <- sapply(X = x, FUN = f, ..., simplify = TRUE, USE.NAMES = use_names)
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

pmap <- function(l, f, ..., use_names = TRUE) {
  result <- do.call(mapply, c(FUN = f, l, list(MoreArgs = list(...)), SIMPLIFY = FALSE, USE.NAMES = use_names))
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

pmap_vec <- function(l, f, ..., use_names = TRUE) {
  result <- do.call(mapply, c(FUN = f, l, list(MoreArgs = list(...)), SIMPLIFY = TRUE, USE.NAMES = use_names))
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

set_names <- function(x, ...) {
  names(x) <- as.character(unlist(list(...)))
  x
}

field_names <- function(fields) {
  names <- map_vec(fields, paste, collapse = "_")
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

  selected_fields <- map(fields, .x = x, function(field, .x) {
    selected_field <- .x
    for (level in seq_along(field)) {
      if (is_null(selected_field[[field[[level]]]]))
        return(NA)
      else
        selected_field <- selected_field[[field[[level]]]]
    }
    selected_field
  })

  set_names(selected_fields, field_names(fields))
}

bind_fields <- function(x, fields) {
  assert(is_list(x))
  assert(is_list(fields), length(fields) > 0)

  conversions <- map_vec(fields, function(f) f["as"])
  raw_fields <- map(fields, function(f) if (is_null(names(f))) f else f[names(f) != "as"])

  if (identical(length(x), 0L)) {
    fields <- set_names(fields, field_names(fields))
    binded_fields <- bind_rows(map(field_names(fields), function(f) logical()))
  } else {
    binded_fields <- bind_rows(map(x, select_fields, fields = raw_fields))
  }

  for (col in 1:ncol(binded_fields)) {
    if (!is_na(conversions[[col]])) {
      if (identical(conversions[[col]], "datetime"))
        binded_fields[[col]] <- as.POSIXct(binded_fields[[col]], format = "%Y-%m-%dT%H:%M:%OSZ")
      else
        binded_fields[[col]] <- as(binded_fields[[col]], conversions[[col]])
    }
  }

  binded_fields
}

remove_missing <- function(x) {
  assert(is_list(x))
  is_empty <- map_vec(x, function(e) {
    is_null(e) || is_na(e) || identical(length(e), 0L)
  })
  x[!is_empty]
}
