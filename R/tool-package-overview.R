package_overview <- function(root_path = ".") {
  description_result <- read_package_description(root_path = root_path)
  if (!is.null(description_result$error)) {
    return(tool_error_response(error = description_result$error))
  }

  desc <- description_result$description
  namespace <- package_namespace_data(
    compact_imports = FALSE,
    root_path = root_path
  )

  tool_value_response(
    description = package_overview_description(desc),
    namespace = package_overview_namespace(namespace),
    files = package_overview_files(root_path = root_path),
    documentation = package_overview_docs(root_path = root_path),
    tests = package_overview_tests(desc, root_path = root_path),
    source_objects = package_overview_source_objects(
      namespace,
      root_path = root_path
    )
  )
}

package_overview_description <- function(desc) {
  field_names <- colnames(desc)
  fields <- set_names(as.list(desc), field_names)

  for (name in c(
    "Depends",
    "Imports",
    "Suggests",
    "Enhances",
    "LinkingTo",
    "Remotes"
  )) {
    if (is.null(fields[[name]])) {
      fields[[name]] <- character()
    } else {
      fields[[name]] <- fields[[name]] |>
        str_split_1(",") |>
        str_trim() |>
        keep(nzchar) |>
        I()
    }
  }

  list(
    fields = fields,
    more = list(
      tool = "package_dependencies",
      reason = paste(
        "Use for dependency tree expansion and dependency-specific analysis."
      )
    )
  )
}

package_overview_namespace <- function(
  namespace = package_namespace_data(compact_imports = FALSE)
) {
  if (!is.null(namespace$error)) {
    return(list(
      unavailable = TRUE,
      error = namespace$error,
      more = list(
        tool = "package_namespace",
        reason = paste(
          "Use for full import declarations, importFrom directives, S3",
          "methods, native-code declarations, and namespace parse details."
        )
      )
    ))
  }

  imports_count <- length(namespace$imports)
  s3_methods_count <- length(namespace$s3_methods)
  use_dyn_lib_count <- length(namespace$use_dyn_lib)
  exports <- unlist(namespace$exports, use.names = FALSE) %||% character()

  list(
    exports = I(exports),
    imports_count = imports_count,
    s3_methods_count = s3_methods_count,
    use_dyn_lib_count = use_dyn_lib_count,
    more = list(
      tool = "package_namespace",
      reason = paste(
        "Use for full import declarations, importFrom directives, S3 method",
        "registrations, native-code declarations, and namespace parse details."
      )
    )
  )
}

package_overview_files <- function(root_path = ".") {
  package_file_keys <- c(
    "DESCRIPTION",
    "NAMESPACE",
    "README.md",
    "README.Rmd",
    "NEWS.md",
    "NEWS",
    "LICENSE",
    "LICENSE.md",
    ".Rbuildignore",
    "_pkgdown.yml"
  )
  package_dir_keys <- c(
    "R",
    "tests",
    "tests/testthat",
    "man",
    "vignettes",
    "data",
    "data-raw",
    "src",
    "inst"
  )

  file_index <- package_file_index(root_path)
  layout <- package_overview_layout(file_index)
  top_level_file_count <-
    package_overview_index_direct_file_count(file_index, ".")
  top_level_dir_count <- length(layout)
  total_file_count <- sum(file_index$is_file)

  list(
    summary = list(
      total_file_count = total_file_count,
      top_level_dir_count = top_level_dir_count,
      top_level_file_count = top_level_file_count
    ),
    has_package_files = package_file_keys |>
      map(
        ~ {
          path <- fs::path(root_path, .x)
          file.exists(path) && !fs::dir_exists(path)
        }
      ) |>
      set_names(package_file_keys),
    has_package_dirs = package_dir_keys |>
      map(~ fs::dir_exists(fs::path(root_path, .x))) |>
      set_names(package_dir_keys),
    layout = I(layout),
    more = list(
      tool = "package_files",
      reason = paste(
        "Use for complete file listings by scope and optional metadata such as",
        "line counts, sizes, or modification times."
      )
    )
  )
}

package_overview_layout <- function(file_index, path = ".") {
  file_index$path[file_index$is_dir & file_index$parent == path] |>
    sort() |>
    map(package_overview_layout_node, file_index = file_index)
}

package_overview_layout_node <- function(path, file_index) {
  list(
    path = path,
    direct_file_count = package_overview_index_direct_file_count(
      file_index,
      path
    ),
    recursive_file_count = sum(
      file_index$is_file & str_starts(file_index$path, fixed(paste0(path, "/")))
    ),
    children = I(package_overview_layout(file_index, path = path))
  )
}

package_overview_index_direct_file_count <- function(file_index, path) {
  sum(file_index$is_file & file_index$parent == path)
}

package_overview_docs <- function(root_path = ".") {
  vignette_files <- suppressWarnings(fs::dir_ls(
    fs::path(root_path, "vignettes"),
    recurse = FALSE,
    regexp = "[.](Rmd|rmd|qmd|Rnw|rnw)$",
    type = "file",
    fail = FALSE
  ))

  man_files <- suppressWarnings(fs::dir_ls(
    fs::path(root_path, "man"),
    recurse = FALSE,
    regexp = "[.]Rd$",
    type = "file",
    fail = FALSE
  ))

  list(
    man_pages_count = length(man_files),
    vignettes_count = length(vignette_files),
    more = list(
      tools = I(c(
        "package_docs",
        "help_page",
        "help_search",
        "package_vignettes",
        "package_news"
      )),
      reason = paste(
        "Use these for documentation maps, rendered help topics, help",
        "discovery, vignette contents, and NEWS entries."
      )
    )
  )
}

package_overview_tests <- function(desc, root_path = ".") {
  r_files <- suppressWarnings(fs::dir_ls(
    fs::path(root_path, "tests", "testthat"),
    recurse = TRUE,
    regexp = "[.][Rr]$",
    type = "file",
    fail = FALSE
  )) |>
    as.character()

  frameworks <- package_overview_test_frameworks(desc, root_path = root_path)

  list(
    frameworks = I(frameworks),
    test_count = sum(map_int(
      r_files,
      package_overview_test_that_count,
      root_path = root_path
    )),
    snapshot_count = package_overview_snapshot_count(root_path = root_path),
    helper_file_count = package_overview_helper_file_count(
      root_path = root_path
    ),
    fixture_dir_count = package_overview_fixture_dir_count(
      root_path = root_path
    ),
    more = list(
      tool = "tests_for",
      reason = paste(
        "Use before behavior changes to find tests related to a symbol."
      )
    )
  )
}

package_overview_test_frameworks <- function(desc, root_path = ".") {
  frameworks <- character()
  suggests <- if ("Suggests" %in% colnames(desc)) desc[1, "Suggests"] else NA

  if (!is.na(suggests) && str_detect(suggests, "\\btestthat\\b")) {
    frameworks <- c(frameworks, "testthat")
  }
  if (fs::dir_exists(fs::path(root_path, "tests", "testthat"))) {
    frameworks <- c(frameworks, "testthat")
  }

  unique(frameworks)
}

package_overview_test_that_count <- function(path, root_path = ".") {
  relative_path <- fs::path_rel(path, start = root_path) |>
    fs::path_norm()
  syntax <- r6_syntax_parse_file(root_path, relative_path)
  if (length(syntax$parse_errors) > 0L) {
    return(0L)
  }

  query <- treesitter::query(treesitter.r::language(), "(call) @call")
  matches <- treesitter::query_matches(query, syntax$root)[[1]]
  sum(map_lgl(matches, \(match) {
    package_overview_is_test_that_call_node(
      r6_syntax_capture_node(match, "call")
    )
  }))
}

package_overview_is_test_that_call_node <- function(node) {
  if (is.null(node) || !identical(node_type(node), "call")) {
    return(FALSE)
  }

  function_node <- node_child_by_field_name(node, "function")
  if (is.null(function_node)) {
    return(FALSE)
  }

  type <- node_type(function_node)
  if (identical(type, "identifier")) {
    return(identical(node_text(function_node), "test_that"))
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
    identical(node_text(lhs), "testthat") &&
    identical(node_text(rhs), "test_that")
}

package_overview_snapshot_count <- function(root_path = ".") {
  suppressWarnings(fs::dir_ls(
    fs::path(root_path, "tests", "testthat", "_snaps"),
    recurse = TRUE,
    type = "file",
    fail = FALSE
  )) |>
    length()
}

package_overview_helper_file_count <- function(root_path = ".") {
  suppressWarnings(fs::dir_ls(
    fs::path(root_path, "tests", "testthat"),
    recurse = FALSE,
    regexp = "(^|[/\\\\])helper-[^/\\\\]+[.][Rr]$",
    type = "file",
    fail = FALSE
  )) |>
    length()
}

package_overview_fixture_dir_count <- function(root_path = ".") {
  suppressWarnings(fs::dir_ls(
    fs::path(root_path, "tests", "testthat"),
    recurse = TRUE,
    type = "directory",
    fail = FALSE
  )) |>
    fs::path_file() |>
    keep(\(x) {
      x %in%
        c(
          "fixtures",
          "fixture",
          "testdata",
          "test-data",
          "data",
          "files",
          "_fixtures"
        )
    }) |>
    length()
}

package_overview_source_objects <- function(
  namespace = package_namespace_data(compact_imports = FALSE),
  root_path = "."
) {
  r_files <- suppressWarnings(r6_syntax_source_files(root_path)) |>
    fs::path_norm() |>
    sort()

  bare_r6class_is_imported <- package_overview_bare_r6class_is_imported(
    namespace
  )
  source_results <- map(
    r_files,
    package_overview_file_source_objects,
    bare_r6class_is_imported = bare_r6class_is_imported,
    root_path = root_path
  )
  objects <- source_results |>
    map("objects") |>
    list_flatten()
  parse_errors <- source_results |>
    map("parse_error") |>
    compact()

  kinds <- map_chr(objects, "kind")

  object_limit <- 20L
  object_preview <- objects[seq_len(min(length(objects), object_limit))]

  list(
    counts = list(
      functions = sum(kinds == "function"),
      r6_classes = sum(kinds == "r6_class"),
      other_objects = sum(kinds == "other_object")
    ),
    total_objects = length(objects),
    listed_objects = length(object_preview),
    objects = I(object_preview),
    objects_truncated = length(objects) > object_limit,
    parse_errors = I(parse_errors),
    more = list(
      tools = I(c("symbols", "definition", "r6_classes", "r6_class")),
      reason = paste(
        "Source objects are previewed only. Use symbols for searchable source",
        "symbols, definition for a specific object, and R6 tools for class",
        "structure."
      )
    )
  )
}

package_overview_file_source_objects <- function(
  path,
  bare_r6class_is_imported,
  root_path = "."
) {
  relative_path <- fs::path_norm(path)
  syntax <- r6_syntax_parse_file(root_path, relative_path)
  parse_error <- package_overview_source_parse_error(syntax)
  if (!is.null(parse_error)) {
    return(list(objects = I(list()), parse_error = parse_error))
  }

  source_lines <- read_source_lines(source_path_for_record(
    relative_path,
    source_root = root_path
  ))
  objects <- r6_syntax_assignment_matches(syntax$root) |>
    keep(\(match) r6_syntax_is_top_level_assignment(match$assignment)) |>
    keep(\(match) node_text(match$operator) %in% c("<-", "=")) |>
    map(
      package_overview_source_object,
      relative_path,
      bare_r6class_is_imported,
      source_lines
    ) |>
    compact()

  list(objects = I(objects), parse_error = parse_error)
}

package_overview_source_parse_error <- function(syntax) {
  if (length(syntax$parse_errors) == 0L) {
    return(NULL)
  }

  list(
    path = syntax$parse_errors[[1L]]$path,
    error = syntax$parse_errors[[1L]]$message
  )
}

package_overview_source_object <- function(
  match,
  relative_path,
  bare_r6class_is_imported,
  source_lines = NULL
) {
  range <- r6_syntax_node_range(match$assignment)

  list(
    name = r6_syntax_normalize_name(node_text(match$lhs)),
    kind = package_overview_source_object_kind(
      match$rhs,
      bare_r6class_is_imported = bare_r6class_is_imported
    ),
    path = relative_path,
    line = range$start_line,
    end_line = range$end_line
  ) |>
    add_leading_comment_lines(source_lines = source_lines) |>
    compact_location_fields(include_columns = FALSE)
}

package_overview_source_object_kind <- function(rhs, bare_r6class_is_imported) {
  if (r6_syntax_is_function_definition(rhs)) {
    return("function")
  }

  if (package_overview_is_r6_class_call_node(rhs, bare_r6class_is_imported)) {
    return("r6_class")
  }

  "other_object"
}

package_overview_is_r6_class_call_node <- function(
  node,
  bare_r6class_is_imported
) {
  if (is.null(node) || !identical(node_type(node), "call")) {
    return(FALSE)
  }

  function_node <- node_child_by_field_name(node, "function")
  if (is.null(function_node)) {
    return(FALSE)
  }

  type <- node_type(function_node)
  if (identical(type, "identifier")) {
    function_name <- r6_syntax_normalize_name(node_text(function_node))
    return(
      isTRUE(bare_r6class_is_imported) && identical(function_name, "R6Class")
    )
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
    identical(r6_syntax_normalize_name(node_text(lhs)), "R6") &&
    identical(r6_syntax_normalize_name(node_text(rhs)), "R6Class")
}

package_overview_bare_r6class_is_imported <- function(namespace) {
  if (!is.null(namespace$error)) {
    return(FALSE)
  }

  "R6Class" %in% (namespace$imports$R6 %||% character())
}

tool_package_overview <- function(service) {
  mcplite::tool(
    function(package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = \(selected_service) selected_service$package_overview()
      )
    },
    name = "package_overview",
    description = paste(
      "Get a compact orientation map of the local package, including metadata,",
      "namespace, file layout, docs, tests, and source objects."
    ),
    arguments = list(
      package_root = package_root_argument()
    )
  )
}
