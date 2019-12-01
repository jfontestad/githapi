gh_map <- function(x, f, ..., simplify = FALSE, use_names = TRUE) {
  result <- sapply(X = x, FUN = f, ..., simplify = simplify, USE.NAMES = use_names)
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

gh_pmap <- function(l, f, ..., simplify = FALSE, use_names = TRUE) {
  result <- do.call(mapply, c(FUN = f, l, list(MoreArgs = list(...)), SIMPLIFY = simplify, USE.NAMES = use_names))
  if (!use_names) {
    names(result) <- NULL
  }
  result
}

field_names <- function(fields) {
  names <- gh_map(fields, paste, collapse = "_", simplify = TRUE)
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

list_fields <- function(x, sublist, field) {
  assert(is_list(x))
  assert(is_character(sublist))
  assert(is_scalar_character(field))

  if (!missing(sublist) || !is_null(sublist)) {
    for (i in sublist) {
      x <- x[[i]]
    }
  }

  if (is_null(x) || identical(length(x), 0L)) {
    NULL
  } else {
    gh_map(x, getElement, field, simplify = TRUE)
  }
}

select_fields <- function(x, fields) {
  assert(is_list(x))
  assert(is_list(fields), length(fields) > 0)

  selected_fields <- gh_map(fields, .x = x, function(field, .x) {
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

  conversions <- gh_map(fields, function(f) f["as"], simplify = TRUE)
  raw_fields <- gh_map(fields, function(f) if (is_null(names(f))) f else f[names(f) != "as"])

  if (identical(length(x), 0L)) {
    fields <- set_names(fields, field_names(fields))
    binded_fields <- bind_rows(gh_map(field_names(fields), function(f) logical()))
  } else {
    binded_fields <- bind_rows(gh_map(x, select_fields, fields = raw_fields))
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

  is_empty <- gh_map(x, simplify = TRUE, function(e) {
    is_null(e) || all(is_na(e)) || identical(length(e), 0L)
  })

  x[!is_empty]
}
