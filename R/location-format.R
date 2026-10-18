format_location <- function(
  path,
  line = NULL,
  column = NULL,
  end_line = NULL,
  end_column = NULL
) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }

  line <- normalize_location_part(line)
  column <- normalize_location_part(column)
  end_line <- normalize_location_part(end_line)
  end_column <- normalize_location_part(end_column)

  if (is.null(line)) {
    return(path)
  }

  start <- if (is.null(column)) {
    as.character(line)
  } else {
    paste0(line, ":", column)
  }

  if (is.null(end_line)) {
    return(paste0(path, ":", start))
  }

  if (identical(line, end_line) && is.null(column) && is.null(end_column)) {
    return(paste0(path, ":", start))
  }

  end <- if (is.null(end_column)) {
    as.character(end_line)
  } else {
    paste0(end_line, ":", end_column)
  }

  paste0(path, ":", start, "-", end)
}

normalize_location_part <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NULL)
  }

  x <- suppressWarnings(as.integer(x[[1L]]))
  if (is.na(x)) {
    return(NULL)
  }

  x
}

compact_location_fields <- function(
  record,
  field = "location",
  include_columns = TRUE,
  include_leading_comment = FALSE,
  source_lines = NULL,
  source_path = NULL,
  source_root = NULL
) {
  if (is.null(record)) {
    return(NULL)
  }

  path <- record$path %||% record$file
  line <- record$line %||% record$start_line
  end_line <- record$end_line

  if (isTRUE(include_leading_comment)) {
    record <- add_leading_comment_lines(
      record,
      source_lines = source_lines,
      source_path = source_path_for_record(source_path %||% path, source_root)
    )
  }

  location <- format_location(
    path = path,
    line = line,
    column = if (isTRUE(include_columns)) record$column else NULL,
    end_line = end_line,
    end_column = if (isTRUE(include_columns)) record$end_column else NULL
  )

  if (!is.null(location)) {
    record[[field]] <- location
  }

  record$path <- NULL
  record$file <- NULL
  record$line <- NULL
  record$column <- NULL
  record$start_line <- NULL
  record$end_line <- NULL
  record$end_column <- NULL

  record
}

compact_location_records <- function(
  records,
  field = "location",
  include_columns = TRUE,
  include_leading_comment = FALSE,
  source_paths = NULL,
  source_root = NULL
) {
  map2(
    records,
    source_paths %||% rep(list(NULL), length(records)),
    \(record, source_path) {
      compact_location_fields(
        record,
        field = field,
        include_columns = include_columns,
        include_leading_comment = include_leading_comment,
        source_path = source_path,
        source_root = source_root
      )
    }
  )
}

format_same_file_line_range <- function(start_line, end_line = start_line) {
  start_line <- normalize_location_part(start_line)
  end_line <- normalize_location_part(end_line) %||% start_line
  if (is.null(start_line)) {
    return(NULL)
  }
  if (identical(start_line, end_line)) {
    return(as.character(start_line))
  }

  paste0(start_line, "-", end_line)
}

add_leading_comment_lines <- function(
  record,
  source_lines = NULL,
  source_path = NULL,
  field = "leading_comment_lines"
) {
  if (is.null(record)) {
    return(NULL)
  }
  start_line <- record$line %||% record$start_line
  range <- leading_comment_block_range(
    source_lines = source_lines %||% read_source_lines(source_path),
    object_line = start_line
  )
  if (!is.null(range)) {
    record[[field]] <- format_same_file_line_range(
      range$start_line,
      range$end_line
    )
  }

  record
}

leading_comment_block_range <- function(source_lines, object_line) {
  object_line <- normalize_location_part(object_line)
  if (is.null(object_line) || object_line <= 1L || length(source_lines) == 0L) {
    return(NULL)
  }

  scan_line <- min(object_line - 1L, length(source_lines))
  if (!is_comment_line(source_lines[[scan_line]])) {
    return(NULL)
  }

  comment_start <- scan_line
  scan_line <- scan_line - 1L
  while (scan_line >= 1L) {
    if (!is_comment_line(source_lines[[scan_line]])) {
      break
    }
    comment_start <- scan_line
    scan_line <- scan_line - 1L
  }

  list(start_line = comment_start, end_line = object_line - 1L)
}

read_source_lines <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    return(character())
  }

  tryCatch(
    readLines(path, warn = FALSE),
    error = function(cnd) character()
  )
}

source_path_for_record <- function(path, source_root = NULL) {
  if (is.null(path) || !nzchar(path)) {
    return(NULL)
  }
  if (is.null(source_root) || !nzchar(source_root)) {
    return(path)
  }

  contained_source_path(path, source_root)
}

is_comment_line <- function(line) {
  str_detect(line, "^\\s*#")
}
