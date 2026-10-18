tests_for <- function(name, root_path = ".") {
  result <- tests_for_data(name, root_path = root_path)
  if (!is.null(result$error)) {
    return(tool_error_response(!!!result))
  }

  tool_value_response(!!!result)
}

tests_for_data <- function(name, root_path = ".") {
  if (is.null(name) || !nzchar(str_trim(name))) {
    return(list(
      error = list(
        code = "missing_name",
        message = "Symbol name must be a non-empty string."
      )
    ))
  }

  # Prefer tests/testthat when present; fall back to tests. Scanning both
  # recursively would double-count files under tests/testthat.
  test_dirs <- c(
    fs::path(root_path, "tests", "testthat"),
    fs::path(root_path, "tests")
  )
  existing_dirs <- test_dirs[fs::dir_exists(test_dirs)][1L]
  existing_dirs <- existing_dirs[!is.na(existing_dirs)]

  files <- suppressWarnings(fs::dir_ls(
    existing_dirs,
    recurse = TRUE,
    regexp = "[.][Rr]$",
    type = "file",
    fail = FALSE
  )) |>
    as.character()

  if (length(files) == 0L) {
    return(list(
      name = name,
      count = 0L,
      matches = list()
    ))
  }

  pattern <- paste0("\\b", str_escape(name), "\\b")

  matches <- map(files, function(file) {
    file_path <- contained_source_path(fs::path_abs(file), root_path)
    lines <- tryCatch(
      if (!is.null(file_path)) readLines(file_path, warn = FALSE),
      error = function(cnd) NULL
    )
    if (is.null(lines)) {
      return(list())
    }

    hit_lines <- which(str_detect(lines, pattern))
    if (length(hit_lines) == 0L) {
      return(list())
    }

    relative_path <- fs::path_rel(file, start = root_path) |>
      fs::path_norm()
    blocks <- test_file_test_blocks(lines, relative_path)

    map(hit_lines, function(ln) {
      block <- test_that_block_for_line(blocks, ln)
      if (is.null(block)) {
        start_line <- ln
        end_line <- ln
        desc <- NULL
      } else {
        start_line <- block$start_line
        end_line <- block$end_line
        desc <- block$desc
      }
      list(
        key = paste(relative_path, start_line, end_line, sep = ":"),
        path = relative_path,
        line = start_line,
        end_line = end_line,
        desc = desc,
        hit_location = format_location(relative_path, line = ln)
      ) |>
        add_leading_comment_lines(source_lines = lines) |>
        compact_location_fields(include_columns = FALSE)
    })
  }) |>
    list_flatten() |>
    grouped_test_matches()

  list(
    name = name,
    count = length(matches),
    matches = matches
  )
}

grouped_test_matches <- function(matches) {
  if (length(matches) == 0L) {
    return(list())
  }

  keys <- map_chr(matches, "key")
  groups <- split(matches, factor(keys, levels = unique(keys))) |>
    unname()

  map(groups, function(group) {
    first <- group[[1L]]
    match <- list(
      location = first$location,
      desc = first$desc,
      hit_count = length(group),
      hits = as.list(map_chr(group, "hit_location"))
    )
    if (!is.null(first$leading_comment_lines)) {
      match$leading_comment_lines <- first$leading_comment_lines
    }

    match
  })
}

test_file_test_blocks <- function(lines, path) {
  if (length(lines) == 0L) {
    return(list())
  }

  root <- tryCatch(
    treesitter::text_parse(
      paste(enc2utf8(lines), collapse = "\n"),
      treesitter.r::language()
    ),
    error = function(cnd) NULL
  )
  if (is.null(root) || node_has_error(root)) {
    return(list())
  }

  query <- treesitter::query(treesitter.r::language(), "(call) @call")
  matches <- treesitter::query_matches(query, root)
  if (length(matches) == 0L) {
    return(list())
  }

  matches[[1L]] |>
    map(\(match) r6_syntax_capture_node(match, "call")) |>
    keep(package_overview_is_test_that_call_node) |>
    map(\(node) {
      range <- r6_syntax_node_range(node)
      list(
        path = path,
        start_line = range$start_line,
        end_line = range$end_line,
        desc = test_that_call_desc(node)
      )
    }) |>
    discard(\(block) is.null(block$start_line) || is.null(block$end_line))
}

test_that_call_desc <- function(call_node) {
  arguments <- r6_syntax_call_argument_nodes(call_node)
  desc_argument <- detect(arguments, \(argument) {
    identical(r6_syntax_argument_name(argument), "desc")
  })
  desc_argument <- desc_argument %||%
    detect(arguments, \(argument) {
      is.null(r6_syntax_argument_name(argument))
    })
  if (is.null(desc_argument)) {
    return(NULL)
  }

  r6_syntax_string_value(r6_syntax_argument_value(desc_argument))
}

test_that_block_for_line <- function(blocks, line_number) {
  if (length(blocks) == 0L) {
    return(NULL)
  }

  starts <- map_int(blocks, "start_line")
  idx <- findInterval(line_number, starts)
  if (idx == 0L || line_number > blocks[[idx]]$end_line) {
    return(NULL)
  }

  blocks[[idx]]
}

tool_tests_for <- function(service) {
  mcplite::tool(
    function(name, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = \(selected_service) selected_service$tests_for(name = name)
      )
    },
    name = "tests_for",
    description = paste(
      "Find test files and test blocks that reference a symbol. Use this",
      "before changing behavior to locate related tests."
    ),
    arguments = list(
      name = mcplite::type_string(
        "Symbol name to search for in tests."
      ),
      package_root = package_root_argument()
    )
  )
}
