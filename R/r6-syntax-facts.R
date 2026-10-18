r6_syntax_source_files <- function(root_path = ".") {
  dir <- fs::path(root_path, "R")
  if (!fs::dir_exists(dir)) {
    return(character())
  }

  fs::dir_ls(
    dir,
    recurse = FALSE,
    regexp = "[.][Rr]$",
    type = "file",
    fail = FALSE
  ) |>
    map_chr(\(path) r6_syntax_root_relative_path(path, root_path)) |>
    unname()
}

r6_syntax_parse_file <- function(root_path, path) {
  absolute_path <- contained_source_path(path, root_path)
  lines <- tryCatch(
    if (!is.null(absolute_path)) readLines(absolute_path, warn = FALSE),
    error = function(cnd) NULL
  )
  if (is.null(lines)) {
    return(list(
      path = path,
      lines = character(),
      root = NULL,
      classes = list(),
      member_references = list(),
      parse_errors = list(list(
        path = path,
        message = "Could not read source file."
      ))
    ))
  }

  lines <- enc2utf8(lines)
  text <- paste(lines, collapse = "\n")
  root <- treesitter::text_parse(text, treesitter.r::language())
  parse_errors <- if (node_has_error(root)) {
    list(list(
      path = path,
      message = "Tree-sitter reported parse errors; results may be partial."
    ))
  } else {
    list()
  }

  list(
    path = path,
    lines = lines,
    root = root,
    classes = r6_syntax_class_facts(root, path),
    member_references = r6_syntax_member_reference_facts(root, path),
    parse_errors = parse_errors
  )
}

r6_syntax_class_facts <- function(root, path) {
  r6_syntax_assignment_matches(root) |>
    keep(\(match) r6_syntax_is_top_level_assignment(match$assignment)) |>
    map(\(match) r6_syntax_class_fact(match, path)) |>
    compact()
}

r6_syntax_assignment_matches <- function(root) {
  query <- treesitter::query(
    treesitter.r::language(),
    paste(
      "(binary_operator lhs: (_) @lhs operator: _ @operator",
      "rhs: (_) @rhs) @assignment"
    )
  )
  matches <- treesitter::query_matches(query, root)[[1]]
  matches |>
    map(\(match) {
      list(
        assignment = r6_syntax_capture_node(match, "assignment"),
        lhs = r6_syntax_capture_node(match, "lhs"),
        operator = r6_syntax_capture_node(match, "operator"),
        rhs = r6_syntax_capture_node(match, "rhs")
      )
    }) |>
    keep(\(match) {
      !is.null(match$lhs) &&
        identical(node_type(match$lhs), "identifier") &&
        node_text(match$operator) %in% c("<-", "=", "<<-")
    })
}

r6_syntax_class_fact <- function(match, path) {
  if (!r6_syntax_is_r6_class_call(match$rhs)) {
    return(NULL)
  }

  var_name <- node_text(match$lhs)
  arguments <- r6_syntax_call_argument_nodes(match$rhs)
  classname <- r6_syntax_classname(arguments)
  inherit <- r6_syntax_inherit(arguments)
  class_name <- classname %||% var_name
  range <- r6_syntax_node_range(match$assignment)

  list(
    name = class_name,
    var_name = var_name,
    file = path,
    path = path,
    line = range$start_line,
    column = range$start_column,
    end_line = range$end_line,
    end_column = range$end_column,
    inherit = inherit,
    public = r6_syntax_section_members(arguments, "public", class_name, path),
    private = r6_syntax_section_members(arguments, "private", class_name, path),
    active = r6_syntax_section_members(arguments, "active", class_name, path)
  ) |>
    compact()
}

r6_syntax_is_r6_class_call <- function(node) {
  if (is.null(node) || !identical(node_type(node), "call")) {
    return(FALSE)
  }

  function_node <- node_child_by_field_name(node, "function")
  if (is.null(function_node)) {
    return(FALSE)
  }

  type <- node_type(function_node)
  if (identical(type, "identifier")) {
    return(identical(node_text(function_node), "R6Class"))
  }
  if (!identical(type, "namespace_operator")) {
    return(FALSE)
  }

  lhs <- node_child_by_field_name(function_node, "lhs")
  rhs <- node_child_by_field_name(function_node, "rhs")
  operator <- r6_syntax_extract_operator(function_node)
  !is.null(lhs) &&
    !is.null(rhs) &&
    identical(operator, "::") &&
    identical(node_text(lhs), "R6") &&
    identical(node_text(rhs), "R6Class")
}

r6_syntax_is_call_named <- function(node, name) {
  if (is.null(node) || !identical(node_type(node), "call")) {
    return(FALSE)
  }
  call_name <- r6_syntax_call_name(node)
  !is.null(call_name) && identical(call_name, name)
}

r6_syntax_call_name <- function(node) {
  function_node <- node_child_by_field_name(node, "function")
  if (is.null(function_node)) {
    return(NULL)
  }
  type <- node_type(function_node)
  if (identical(type, "identifier")) {
    return(node_text(function_node))
  }
  if (identical(type, "namespace_operator")) {
    rhs <- node_child_by_field_name(function_node, "rhs")
    if (!is.null(rhs)) {
      return(node_text(rhs))
    }
  }

  NULL
}

r6_syntax_call_argument_nodes <- function(call_node) {
  arguments <- node_child_by_field_name(call_node, "arguments")
  if (is.null(arguments)) {
    return(list())
  }
  node_named_children(arguments) |>
    keep(\(node) identical(node_type(node), "argument"))
}

r6_syntax_argument_name <- function(argument_node) {
  name <- node_child_by_field_name(argument_node, "name")
  if (is.null(name)) {
    return(NULL)
  }
  r6_syntax_normalize_name(node_text(name))
}

r6_syntax_argument_value <- function(argument_node) {
  node_child_by_field_name(argument_node, "value")
}

r6_syntax_classname <- function(arguments) {
  named <- detect(arguments, \(argument) {
    identical(r6_syntax_argument_name(argument), "classname")
  })
  if (!is.null(named)) {
    return(r6_syntax_string_value(r6_syntax_argument_value(named)))
  }

  first <- detect(arguments, \(argument) {
    is.null(r6_syntax_argument_name(argument))
  })
  if (is.null(first)) {
    return(NULL)
  }
  r6_syntax_string_value(r6_syntax_argument_value(first))
}

r6_syntax_inherit <- function(arguments) {
  inherit <- detect(arguments, \(argument) {
    identical(r6_syntax_argument_name(argument), "inherit")
  })
  if (is.null(inherit)) {
    return(NULL)
  }

  value <- r6_syntax_argument_value(inherit)
  if (is.null(value)) {
    return(NULL)
  }

  string_value <- r6_syntax_string_value(value)
  string_value %||% node_text(value)
}

r6_syntax_section_members <- function(arguments, section, class_name, path) {
  section_argument <- detect(arguments, \(argument) {
    identical(r6_syntax_argument_name(argument), section)
  })
  if (is.null(section_argument)) {
    return(list())
  }

  section_value <- r6_syntax_argument_value(section_argument)
  if (!r6_syntax_is_call_named(section_value, "list")) {
    return(list())
  }

  r6_syntax_call_argument_nodes(section_value) |>
    map(\(member) r6_syntax_member_fact(member, section, class_name, path)) |>
    compact()
}

r6_syntax_member_fact <- function(argument, section, class_name, path) {
  name_node <- node_child_by_field_name(argument, "name")
  if (is.null(name_node)) {
    return(NULL)
  }

  value_node <- r6_syntax_argument_value(argument)
  name <- r6_syntax_normalize_name(node_text(name_node))
  is_function <- r6_syntax_is_function_definition(value_node)
  kind <- if (is_function && identical(section, "active")) {
    "active"
  } else if (is_function) {
    "method"
  } else {
    "field"
  }
  range_node <- value_node %||% argument
  location_range <- r6_syntax_node_range(name_node)
  source_range <- r6_syntax_range_from_nodes(name_node, range_node)

  fact <- list(
    name = name,
    qualified_name = paste0(class_name, "$", name),
    class = class_name,
    section = section,
    kind = kind,
    file = path,
    path = path,
    line = location_range$start_line,
    column = location_range$start_column,
    end_line = source_range$end_line,
    end_column = source_range$end_column,
    start_line = source_range$start_line,
    start_column = source_range$start_column,
    source_start_line = source_range$start_line,
    source_start_column = source_range$start_column,
    source_end_line = source_range$end_line,
    source_end_column = source_range$end_column,
    value_node_type = if (!is.null(value_node)) {
      node_type(value_node)
    },
    value_text = if (!is.null(value_node)) node_text(value_node)
  )

  if (is_function) {
    fact$signature <- r6_syntax_signature(name, value_node)
    fact$parameters <- r6_syntax_parameter_labels(value_node)
  } else {
    fact$default <- if (is.null(value_node)) {
      "NULL"
    } else {
      node_text(value_node)
    }
  }

  compact(fact)
}

r6_syntax_member_reference_facts <- function(root, path) {
  query <- treesitter::query(
    treesitter.r::language(),
    "(extract_operator rhs: (_) @name) @reference"
  )
  matches <- treesitter::query_matches(query, root)[[1]]
  matches |>
    map(\(match) {
      reference <- r6_syntax_capture_node(match, "reference")
      name_node <- r6_syntax_capture_node(match, "name")
      r6_syntax_member_reference_fact(reference, name_node, path)
    }) |>
    compact()
}

r6_syntax_member_reference_fact <- function(reference, name_node, path) {
  if (is.null(reference) || is.null(name_node)) {
    return(NULL)
  }

  operator <- r6_syntax_extract_operator(reference)
  if (!identical(operator, "$")) {
    return(NULL)
  }

  parent <- node_parent(reference)
  is_call <- r6_syntax_is_call_function(parent, reference)
  range <- r6_syntax_node_range(name_node)
  reference_range <- r6_syntax_node_range(if (is_call) parent else reference)
  receiver <- node_child(reference, 1L)

  list(
    name = r6_syntax_normalize_name(node_text(name_node)),
    raw_name = node_text(name_node),
    file = path,
    path = path,
    line = range$start_line,
    column = range$start_column,
    end_line = reference_range$end_line,
    end_column = reference_range$end_column,
    object = if (!is.null(receiver)) node_text(receiver),
    call = if (is_call) node_text(parent),
    is_call = is_call
  ) |>
    compact()
}

r6_syntax_extract_operator <- function(reference) {
  children <- node_children(reference)
  operator <- detect(children, \(child) !node_is_named(child))
  if (is.null(operator)) {
    return(NULL)
  }
  node_text(operator)
}

r6_syntax_signature <- function(name, function_node) {
  params <- r6_syntax_parameter_labels(function_node)
  glue("{name}({paste(params, collapse = ', ')})")
}

r6_syntax_parameter_labels <- function(function_node) {
  parameters <- node_child_by_field_name(function_node, "parameters")
  if (is.null(parameters)) {
    return(character())
  }

  node_named_children(parameters) |>
    keep(\(node) identical(node_type(node), "parameter")) |>
    map_chr(node_text)
}

r6_syntax_source_text <- function(lines, range) {
  if (is.null(range) || length(lines) == 0L) {
    return(NULL)
  }

  lines <- enc2utf8(lines)
  start_line <- range$start_line
  end_line <- range$end_line
  start_column <- range$start_column
  end_column <- range$end_column
  if (
    start_line < 1L ||
      end_line < start_line ||
      end_line > length(lines)
  ) {
    return(NULL)
  }

  start_line_bytes <- nchar(lines[[start_line]], type = "bytes")
  end_line_bytes <- nchar(lines[[end_line]], type = "bytes")
  if (
    start_column < 1L ||
      start_column > start_line_bytes + 1L ||
      end_column < 0L ||
      end_column > end_line_bytes ||
      (start_line == end_line && end_column < start_column)
  ) {
    return(NULL)
  }

  text <- lines[start_line:end_line]
  if (start_line == end_line) {
    return(r6_syntax_substring_bytes(
      text[[1L]],
      start_column,
      end_column
    ))
  }

  text[[1L]] <- r6_syntax_substring_bytes(
    text[[1L]],
    start_column,
    nchar(text[[1L]], type = "bytes")
  )
  last_idx <- length(text)
  text[[last_idx]] <- r6_syntax_substring_bytes(
    text[[last_idx]],
    1L,
    end_column
  )
  paste(text, collapse = "\n")
}

r6_syntax_substring_bytes <- function(x, start, end) {
  # Tree-sitter point columns are byte offsets, so exact source slices must use
  # byte positions rather than R's character-indexed substring helpers.
  if (end < start) {
    return("")
  }
  bytes <- charToRaw(enc2utf8(x))
  rawToChar(bytes[start:end])
}

r6_syntax_is_function_definition <- function(node) {
  !is.null(node) && identical(node_type(node), "function_definition")
}

r6_syntax_is_call_function <- function(call_node, function_node) {
  if (is.null(call_node) || !identical(node_type(call_node), "call")) {
    return(FALSE)
  }
  r6_syntax_same_node(
    node_child_by_field_name(call_node, "function"),
    function_node
  )
}

r6_syntax_same_node <- function(x, y) {
  if (is.null(x) || is.null(y)) {
    return(FALSE)
  }

  identical(node_start_byte(x), node_start_byte(y)) &&
    identical(node_end_byte(x), node_end_byte(y))
}

r6_syntax_is_top_level_assignment <- function(node) {
  parent <- node_parent(node)
  !is.null(parent) && identical(node_type(parent), "program")
}

r6_syntax_node_range <- function(node) {
  start <- node_start_point(node)
  end <- node_end_point(node)
  list(
    start_line = as.integer(point_row(start)) + 1L,
    start_column = as.integer(point_column(start)) + 1L,
    end_line = as.integer(point_row(end)) + 1L,
    end_column = as.integer(point_column(end))
  )
}

r6_syntax_range_from_nodes <- function(start_node, end_node) {
  start <- r6_syntax_node_range(start_node)
  end <- r6_syntax_node_range(end_node)
  list(
    start_line = start$start_line,
    start_column = start$start_column,
    end_line = end$end_line,
    end_column = end$end_column
  )
}

r6_syntax_string_value <- function(node) {
  if (is.null(node) || !identical(node_type(node), "string")) {
    return(NULL)
  }

  node_named_children(node) |>
    keep(\(child) identical(node_type(child), "string_content")) |>
    map_chr(node_text) |>
    paste(collapse = "")
}

r6_syntax_normalize_name <- function(x) {
  str_remove_all(x, "^`|`$")
}

r6_syntax_capture_node <- function(match, name) {
  indexes <- which(match$name == name)
  if (length(indexes) == 0L) {
    return(NULL)
  }

  match$node[[indexes[[1]]]]
}

r6_syntax_root_relative_path <- function(path, root_path) {
  fs::path_rel(path, start = root_path) |>
    as.character()
}
