r6_classes <- function(root_path = ".") {
  parsed <- r6_collect_source_facts(root_path = root_path)
  classes <- parsed$classes |>
    map(r6_class_summary_record) |>
    r6_compact_source_records(source_root = root_path)

  response <- list(count = length(classes), classes = classes)
  if (length(parsed$parse_errors) > 0L) {
    response$parse_errors <- r6_tool_parse_errors(parsed$parse_errors)
  }
  tool_value_response(!!!response)
}

r6_class <- function(name, root_path = ".") {
  name <- trimws(as.character(name %||% ""))
  if (!nzchar(name)) {
    return(tool_error_response(
      error = list(
        code = "missing_name",
        message = "Class name must be a non-empty string."
      )
    ))
  }

  parsed <- r6_collect_source_facts(root_path = root_path)
  matches <- keep(parsed$classes, \(class) identical(class$name, name))

  if (length(matches) == 1L) {
    return(tool_value_response(
      !!!r6_class_detail_record(
        matches[[1L]],
        source_root = root_path
      )
    ))
  }

  if (length(matches) > 1L) {
    return(tool_value_response(
      status = "ambiguous",
      name = name,
      count = length(matches),
      matches = matches |>
        map(r6_class_summary_record) |>
        r6_compact_source_records(source_root = root_path)
    ))
  }

  error <- list(
    code = "class_not_found",
    message = glue("No R6 class named '{name}' found in R/ directory.")
  )
  if (length(parsed$parse_errors) > 0L) {
    error$parse_errors <- r6_tool_parse_errors(parsed$parse_errors)
  }
  tool_error_response(error = error)
}

r6_method <- function(class, method, root_path = ".") {
  class <- trimws(as.character(class %||% ""))
  method <- trimws(as.character(method %||% ""))

  if (!nzchar(class)) {
    return(tool_error_response(
      error = list(
        code = "missing_class",
        message = "Class name must be a non-empty string."
      )
    ))
  }

  if (!nzchar(method)) {
    return(tool_error_response(
      error = list(
        code = "missing_method",
        message = "Method name must be a non-empty string."
      )
    ))
  }

  parsed <- r6_collect_source_facts(root_path = root_path)
  class_matches <- keep(parsed$classes, \(fact) identical(fact$name, class))
  matches <- class_matches |>
    map(\(fact) {
      r6_method_match_record(
        fact,
        method,
        root_path = root_path
      )
    }) |>
    compact()

  if (length(matches) == 1L) {
    if (!is.null(matches[[1L]]$error)) {
      return(tool_error_response(error = matches[[1L]]$error))
    }
    return(tool_value_response(
      !!!r6_compact_source_record(
        matches[[1L]],
        source_root = root_path
      )
    ))
  }

  if (length(matches) > 1L) {
    return(tool_value_response(
      status = "ambiguous",
      class = class,
      method = method,
      count = length(matches),
      matches = r6_compact_source_records(matches, source_root = root_path)
    ))
  }

  if (length(class_matches) == 0L) {
    error <- list(
      code = "class_not_found",
      message = glue("No R6 class named '{class}' found in R/ directory.")
    )
    if (length(parsed$parse_errors) > 0L) {
      error$parse_errors <- r6_tool_parse_errors(parsed$parse_errors)
    }
    return(tool_error_response(error = error))
  }

  error <- list(
    code = "method_not_found",
    message = glue("No method '{method}' found on R6 class '{class}'.")
  )
  if (length(parsed$parse_errors) > 0L) {
    error$parse_errors <- r6_tool_parse_errors(parsed$parse_errors)
  }
  tool_error_response(error = error)
}

r6_method_references <- function(class, method, root_path = ".") {
  class <- trimws(as.character(class %||% ""))
  method <- trimws(as.character(method %||% ""))

  if (!nzchar(class)) {
    return(tool_error_response(
      error = list(
        code = "missing_class",
        message = "Class name must be a non-empty string."
      )
    ))
  }

  if (!nzchar(method)) {
    return(tool_error_response(
      error = list(
        code = "missing_method",
        message = "Method name must be a non-empty string."
      )
    ))
  }

  source_facts <- r6_collect_source_facts(root_path = root_path)
  target_resolution <- r6_target_resolution(
    source_facts$classes,
    class,
    method,
    source_root = root_path
  )
  test_facts <- r6_test_files(root_path) |>
    map(\(path) r6_syntax_parse_file(root_path, path)) |>
    r6_combine_parsed_facts()
  references <- c(
    source_facts$member_references,
    test_facts$member_references
  ) |>
    keep(\(reference) {
      isTRUE(reference$is_call) && identical(reference$name, method)
    }) |>
    map(r6_reference_record) |>
    compact_location_records(include_columns = FALSE)

  response <- list(
    class = class,
    method = method,
    target_resolution = target_resolution,
    note = paste(
      "Results are name-only syntax matches for $method() calls and do not",
      "prove the receiver type or class membership. Calls on unrelated",
      "object types may be included."
    ),
    count = length(references),
    references = references
  )
  parse_errors <- c(source_facts$parse_errors, test_facts$parse_errors)
  if (length(parse_errors) > 0L) {
    response$parse_errors <- r6_tool_parse_errors(parse_errors)
  }
  tool_value_response(!!!response)
}

r6_collect_source_facts <- function(root_path = ".") {
  r6_syntax_source_files(root_path) |>
    map(\(path) r6_syntax_parse_file(root_path, path)) |>
    r6_combine_parsed_facts()
}

r6_combine_parsed_facts <- function(parsed_files) {
  list(
    classes = parsed_files |>
      map("classes") |>
      list_flatten(),
    member_references = parsed_files |>
      map("member_references") |>
      list_flatten(),
    parse_errors = parsed_files |>
      map("parse_errors") |>
      list_flatten()
  )
}

r6_class_summary_record <- function(class) {
  record <- list(
    name = class$name,
    file = class$file,
    line = class$line,
    end_line = class$end_line
  )
  if (!is.null(class$inherit)) {
    record$inherit <- class$inherit
  }
  record
}

r6_class_detail_record <- function(class, source_root = NULL) {
  record <- r6_class_summary_record(class)
  record$public <- r6_member_records(
    class$public %||% list(),
    source_root = source_root
  )
  record$private <- r6_member_records(
    class$private %||% list(),
    source_root = source_root
  )
  record$active <- r6_member_records(
    class$active %||% list(),
    source_root = source_root
  )
  r6_compact_source_record(record, source_root = source_root)
}

r6_member_records <- function(members, source_root = NULL) {
  members |>
    map(r6_member_record) |>
    r6_compact_source_records(source_root = source_root)
}

r6_member_record <- function(member) {
  record <- list(
    name = member$name,
    kind = member$kind,
    file = member$file,
    line = member$line,
    end_line = member$end_line
  )
  if (!is.null(member$signature)) {
    record$signature <- member$signature
  }
  if (!is.null(member$default)) {
    record$default <- member$default
  }
  record
}

r6_method_match_record <- function(class, method, root_path = ".") {
  member <- r6_class_members(class) |>
    detect(\(candidate) {
      identical(candidate$name, method) &&
        candidate$kind %in% c("method", "active")
    })
  if (is.null(member)) {
    return(NULL)
  }

  range <- list(
    start_line = member$source_start_line,
    start_column = member$source_start_column,
    end_line = member$source_end_line,
    end_column = member$source_end_column
  )
  source_path <- source_path_for_record(member$file, root_path)
  source <- if (!is.null(source_path)) {
    r6_syntax_source_text(readLines(source_path, warn = FALSE), range)
  }
  if (is.null(source)) {
    return(list(
      error = list(
        code = "method_source_unavailable",
        message = glue(
          "Method '{method}' was found on R6 class '{class$name}', ",
          "but exact source range could not be resolved."
        )
      )
    ))
  }

  list(
    class = class$name,
    method = method,
    section = member$section,
    signature = member$signature,
    source = source,
    file = member$file,
    line = member$source_start_line,
    end_line = member$source_end_line
  )
}

r6_target_resolution <- function(
  classes,
  class,
  method,
  source_root = "."
) {
  class_matches <- keep(classes, \(fact) identical(fact$name, class))
  method_matches <- class_matches |>
    map(\(fact) r6_method_match_target(fact, method)) |>
    compact()

  list(
    class = list(
      name = class,
      status = r6_match_status(class_matches),
      count = length(class_matches),
      matches = class_matches |>
        map(r6_class_summary_record) |>
        r6_compact_source_records(source_root = source_root)
    ),
    method = list(
      name = method,
      status = if (length(class_matches) == 0L) {
        "not_checked"
      } else {
        r6_match_status(method_matches)
      },
      count = length(method_matches),
      matches = r6_compact_source_records(
        method_matches,
        source_root = source_root
      )
    )
  )
}

r6_method_match_target <- function(class, method) {
  member <- r6_class_members(class) |>
    detect(\(candidate) {
      identical(candidate$name, method) &&
        candidate$kind %in% c("method", "active")
    })
  if (is.null(member)) {
    return(NULL)
  }

  list(
    class = class$name,
    method = method,
    section = member$section,
    file = member$file,
    line = member$line,
    end_line = member$end_line
  )
}

r6_match_status <- function(matches) {
  count <- length(matches)
  if (count == 0L) {
    return("missing")
  }
  if (count == 1L) {
    return("unique")
  }
  "ambiguous"
}

r6_class_members <- function(class) {
  c(
    class$public %||% list(),
    class$private %||% list(),
    class$active %||% list()
  )
}

r6_reference_record <- function(reference) {
  list(
    file = reference$file,
    line = reference$line,
    end_line = reference$line,
    object = reference$object %||% NA_character_,
    call = reference$call %||% NA_character_
  )
}

r6_compact_source_record <- function(
  record,
  source_lines = NULL,
  source_root = NULL
) {
  compact_location_fields(
    record,
    include_columns = FALSE,
    include_leading_comment = TRUE,
    source_lines = source_lines,
    source_path = source_path_for_record(
      record$file %||% record$path,
      source_root
    ),
    source_root = source_root
  )
}

r6_compact_source_records <- function(
  records,
  source_lines = NULL,
  source_root = NULL
) {
  map(
    records,
    r6_compact_source_record,
    source_lines = source_lines,
    source_root = source_root
  )
}

r6_test_files <- function(root_path = ".") {
  test_dirs <- c(
    fs::path(root_path, "tests", "testthat"),
    fs::path(root_path, "tests")
  )
  test_dir <- test_dirs[fs::dir_exists(test_dirs)][1L]
  if (length(test_dir) == 0L || is.na(test_dir)) {
    return(character())
  }

  fs::dir_ls(
    test_dir,
    recurse = TRUE,
    regexp = "[.][Rr]$",
    type = "file",
    fail = FALSE
  ) |>
    map_chr(\(path) r6_syntax_root_relative_path(path, root_path)) |>
    unname()
}

r6_tool_parse_errors <- function(parse_errors) {
  parse_errors |>
    map(\(error) {
      list(
        file = error$file %||% error$path,
        message = error$message
      )
    }) |>
    unique()
}

tool_r6_classes <- function(service) {
  mcplite::tool(
    function(package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = \(selected_service) selected_service$r6_classes()
      )
    },
    name = "r6_classes",
    description = paste(
      "List R6 classes defined in the package, including locations and",
      "inheritance."
    ),
    arguments = list(
      package_root = package_root_argument()
    )
  )
}

tool_r6_class <- function(service) {
  mcplite::tool(
    function(name, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = \(selected_service) selected_service$r6_class(name = name)
      )
    },
    name = "r6_class",
    description = paste(
      "Inspect an R6 class, including public, private, and active members,",
      "source locations, and method signatures."
    ),
    arguments = list(
      name = mcplite::type_string(
        "R6 class name."
      ),
      package_root = package_root_argument()
    )
  )
}

tool_r6_method <- function(service) {
  mcplite::tool(
    function(class, method, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$r6_method(class = class, method = method)
        }
      )
    },
    name = "r6_method",
    description = "Read the source and location for one R6 class method.",
    arguments = list(
      class = mcplite::type_string(
        "R6 class name."
      ),
      method = mcplite::type_string(
        "Method name."
      ),
      package_root = package_root_argument()
    )
  )
}

tool_r6_method_references <- function(service) {
  mcplite::tool(
    function(class, method, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$r6_method_references(
            class = class,
            method = method
          )
        }
      )
    },
    name = "r6_method_references",
    description = paste(
      "Find $method() call sites that may refer to an R6 method. Results are",
      "name-based and may include calls on other object types."
    ),
    arguments = list(
      class = mcplite::type_string(
        "R6 class name."
      ),
      method = mcplite::type_string(
        "Method name."
      ),
      package_root = package_root_argument()
    )
  )
}
