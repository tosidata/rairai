is_nonempty_string <- function(value) {
  is.character(value) && length(value) == 1L && !is.na(value) && nzchar(value)
}

has_tool_error <- function(value) {
  is.list(value) && !is.null(value$error)
}

tool_error_record <- function(code, message, data = NULL) {
  error <- list(code = code, message = message)
  if (!is.null(data)) {
    error$data <- data
  }

  list(error = error)
}

normalize_optional_string <- function(value, default = NULL) {
  if (length(value) == 0L) {
    return(default)
  }

  value <- as.character(value[[1]])
  if (!nzchar(value)) {
    return(default)
  }

  value
}

split_lines <- function(text) {
  lines <- str_split_1(text, fixed("\n"))
  if (length(lines) == 1L && !nzchar(lines)) {
    return(character())
  }
  if (length(lines) > 0L && !nzchar(lines[[length(lines)]])) {
    return(lines[-length(lines)])
  }

  lines
}

normalize_logical_value <- function(value, default) {
  if (length(value) == 0L || is.na(value[[1]])) {
    return(default)
  }

  isTRUE(value[[1]])
}

normalize_limit_value <- function(limit, default) {
  if (length(limit) == 0L) {
    return(as.integer(default))
  }

  limit <- suppressWarnings(as.integer(limit[[1]]))
  if (is.na(limit) || limit <= 0L) {
    return(as.integer(default))
  }

  limit
}

# Callers supply a positive integer limit normalized at the tool boundary.
limited_records <- function(records, limit) {
  total_count <- length(records)
  records <- records[seq_len(min(total_count, limit))]

  list(
    records = records,
    count = length(records),
    total_count = total_count,
    truncated = total_count > length(records),
    limit = limit
  )
}
