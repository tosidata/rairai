source_index_fact_schema <- 1L

SourceIndex <- R6Class(
  "SourceIndex",
  public = list(
    root_path = NULL,

    # Consume the explicit canonical root selected by the invocation boundary.
    initialize = function(root_path) {
      self$root_path <- root_path
    },

    symbols = function(
      query = NULL,
      path = NULL,
      kind = NULL,
      exact = TRUE,
      limit = 50L
    ) {
      normalized_path <- self$normalize_path(path)
      if (has_tool_error(normalized_path)) {
        return(normalized_path)
      }

      index <- self$build()
      matching_symbols <- index$symbols |>
        self$filter_symbol_candidates(
          name = query,
          kind = kind,
          path = normalized_path,
          exact = exact
        )
      symbols <- limited_records(matching_symbols, limit)

      list(
        query = query %||% "",
        status = private$symbol_status(
          matching_symbols,
          query,
          normalized_path
        ),
        exact = isTRUE(exact),
        filters = self$symbol_filters(kind = kind, path = normalized_path),
        path = if (private$is_file_path(normalized_path)) normalized_path,
        count = symbols$count,
        total_count = symbols$total_count,
        truncated = symbols$truncated,
        limit = symbols$limit,
        symbols = symbols$records,
        parse_errors = index$parse_errors
      )
    },

    resolve_symbol_candidates = function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 20L
    ) {
      normalized_path <- self$normalize_path(path)
      if (has_tool_error(normalized_path)) {
        return(normalized_path)
      }

      index <- self$build()
      matching_candidates <- index$symbols |>
        self$filter_symbol_candidates(
          name = name,
          kind = kind,
          path = normalized_path,
          exact = exact
        )
      candidates <- limited_records(matching_candidates, limit)

      list(
        query = name,
        status = self$candidate_status(matching_candidates),
        candidates = candidates$records,
        exact = isTRUE(exact),
        filters = self$symbol_filters(kind = kind, path = normalized_path),
        count = candidates$count,
        total_count = candidates$total_count,
        truncated = candidates$truncated,
        limit = candidates$limit,
        parse_errors = index$parse_errors
      )
    },

    symbol_resolution_response = function(resolution) {
      candidates <- compact_location_records(
        resolution$candidates,
        include_leading_comment = TRUE,
        source_root = self$root_path
      )
      response <- list(
        query = resolution$query,
        status = resolution$status,
        exact = resolution$exact,
        filters = resolution$filters,
        count = length(candidates),
        total_count = resolution$total_count,
        truncated = resolution$truncated,
        limit = resolution$limit,
        candidates = candidates
      )
      if (length(resolution$parse_errors %||% list()) > 0L) {
        response$parse_errors <- resolution$parse_errors
      }
      response
    },

    references = function(resolved, limit = 100L) {
      if (!isTRUE(resolved$referencable)) {
        return(list(
          status = "not_referencable",
          references = list(),
          count = 0L,
          total_count = 0L,
          truncated = FALSE,
          limit = limit,
          reference_scope = private$reference_scope(),
          parse_errors = list()
        ))
      }

      index <- self$build(include_tests = TRUE)
      matching_references <- index$references |>
        keep(\(reference) {
          private$reference_matches_symbol(reference, resolved)
        }) |>
        private$drop_declaration_reference(resolved) |>
        map(\(reference) {
          reference$node <- NULL
          reference
        })
      references <- limited_records(matching_references, limit)

      list(
        status = "unique",
        references = references$records,
        count = references$count,
        total_count = references$total_count,
        truncated = references$truncated,
        limit = references$limit,
        reference_scope = private$reference_scope(),
        parse_errors = index$parse_errors
      )
    },

    local_function_formals = function(name) {
      # Context validation belongs to the service; resolution is package-wide.
      resolution <- self$resolve_symbol_candidates(
        name = name,
        kind = "Function",
        path = NULL,
        exact = TRUE,
        limit = 20L
      )

      if (identical(resolution$status, "none")) {
        return(list(status = "none", resolved = NULL))
      }
      if (identical(resolution$status, "ambiguous")) {
        response <- self$symbol_resolution_response(resolution)
        response$status <- "ambiguous"
        return(response)
      }

      resolved <- resolution$candidates[[1]]
      if (is.null(resolved$signature)) {
        return(list(status = "none", resolved = NULL))
      }

      list(
        status = "unique",
        resolved = resolved,
        signature = resolved$signature,
        source = "tree_sitter_source_index"
      )
    },

    filter_symbol_candidates = function(
      candidates,
      name = NULL,
      kind = NULL,
      path = NULL,
      exact = TRUE
    ) {
      if (!is.null(name) && nzchar(name)) {
        candidates <- private$filter_symbol_names(candidates, name, exact)
      }
      candidates <- private$filter_symbol_kind(candidates, kind)
      private$filter_symbol_path(candidates, path)
    },

    candidate_status = function(candidates) {
      count <- length(candidates)
      if (count == 0L) {
        return("none")
      }
      if (count == 1L) {
        return("unique")
      }

      "ambiguous"
    },

    symbol_filters = function(kind = NULL, path = NULL) {
      filters <- list()
      if (!is.null(kind) && nzchar(kind)) {
        filters$kind <- kind
      }
      if (!is.null(path) && nzchar(path)) {
        filters$path <- path
      }

      filters
    },

    normalize_path = function(path, argument = "path") {
      normalize_root_relative_path(
        path,
        root_path = self$root_path,
        argument = argument
      )
    },

    build = function(include_tests = FALSE) {
      files <- r6_syntax_source_files(self$root_path)
      if (isTRUE(include_tests)) {
        files <- c(files, private$test_files())
      }
      files <- unique(files)

      facts <- files |>
        map(private$file_facts)

      list(
        symbols = facts |>
          map("symbols") |>
          list_flatten(),
        references = facts |>
          map("references") |>
          list_flatten(),
        parse_errors = facts |>
          map("parse_errors") |>
          list_flatten()
      )
    }
  ),
  private = list(
    test_files = function() {
      dir <- fs::path(self$root_path, "tests", "testthat")
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
        map_chr(\(path) r6_syntax_root_relative_path(path, self$root_path)) |>
        unname()
    },

    file_facts = function(path) {
      # Check the current target before metadata or cache reuse. The parser
      # supplies the usual empty facts/read error for an escaped source path.
      file_path <- contained_source_path(path, self$root_path)
      if (is.null(file_path)) {
        return(private$parse_file(path))
      }
      cached_value(
        key = private$file_fact_key(path, file_path),
        namespace = "source_index",
        expr = private$parse_file(path)
      )
    },

    file_fact_key = function(path, file_path) {
      list(
        type = "file_fact",
        root = self$root_path,
        path = path,
        freshness = private$file_freshness_signature(file_path),
        schema_version = source_index_fact_schema
      )
    },

    file_freshness_signature = function(file_path) {
      info <- fs::file_info(file_path)
      list(
        target = file_path,
        exists = isTRUE(info$type == "file"),
        size = as.numeric(info$size),
        modification_time = as.numeric(info$modification_time)
      )
    },

    parse_file = function(path) {
      parsed <- r6_syntax_parse_file(self$root_path, path)
      if (is.null(parsed$root)) {
        return(list(
          symbols = list(),
          references = list(),
          parse_errors = parsed$parse_errors
        ))
      }

      references <- private$extract_references(
        parsed$root,
        path,
        parsed$member_references
      )

      list(
        symbols = private$extract_symbols(parsed$root, path, parsed$classes),
        references = private$strip_reference_nodes(references),
        parse_errors = parsed$parse_errors
      )
    },

    strip_reference_nodes = function(references) {
      references |>
        map(\(reference) {
          reference$node <- NULL
          reference
        })
    },

    extract_symbols = function(root, path, r6_classes = list()) {
      file_symbol <- list(private$file_symbol(path, root))
      assignments <- r6_syntax_assignment_matches(root) |>
        keep(\(match) node_text(match$operator) %in% c("<-", "=")) |>
        keep(\(match) r6_syntax_is_top_level_assignment(match$assignment))

      symbols <- assignments |>
        map(\(match) private$assignment_symbol(match, path)) |>
        compact()

      r6_members <- r6_classes |>
        map(private$r6_member_symbols) |>
        list_flatten()

      c(file_symbol, symbols, r6_members)
    },

    extract_references = function(root, path, r6_member_references = list()) {
      c(
        private$direct_call_references(root, path),
        private$namespace_references(root, path),
        private$member_references(r6_member_references),
        private$identifier_references(root, path)
      )
    },

    assignment_symbol = function(match, path) {
      name <- node_text(match$lhs)
      rhs_type <- node_type(match$rhs)
      is_function <- identical(rhs_type, "function_definition")
      is_r6 <- r6_syntax_is_r6_class_call(match$rhs)

      kind <- if (is_r6) {
        "Class"
      } else if (is_function) {
        "Function"
      } else {
        "Object"
      }

      record <- private$location_record(match$lhs, path)
      record$end_line <- private$node_end_line(match$assignment)
      record$end_column <- private$node_end_column(match$assignment)
      record$name <- name
      record$kind_name <- kind
      record$source <- "tree_sitter_source_index"
      record$referencable <- TRUE
      if (is_function) {
        record$signature <- private$function_signature(name, match$rhs)
      }
      record
    },

    file_symbol = function(path, root) {
      record <- private$location_record(root, path)
      record$name <- path
      record$kind_name <- "File"
      record$source <- "tree_sitter_source_index"
      record$referencable <- FALSE
      record
    },

    r6_member_symbols = function(class_fact) {
      r6_class_members(class_fact) |>
        map(\(member) {
          record <- list(
            path = member$path,
            line = member$line,
            column = member$column,
            end_line = member$end_line,
            end_column = member$end_column,
            name = member$name,
            qualified_name = member$qualified_name,
            container = member$class,
            member_scope = member$section,
            kind_name = if (member$kind %in% c("method", "active")) {
              "Method"
            } else {
              "Field"
            },
            source = "tree_sitter_source_index",
            referencable = TRUE
          )
          if (identical(record$kind_name, "Method")) {
            record$signature <- private$function_signature_from_labels(
              record$qualified_name,
              member$parameters %||% character()
            )
          }
          record
        }) |>
        compact()
    },

    direct_call_references = function(root, path) {
      query <- treesitter::query(
        treesitter.r::language(),
        "(call function: (identifier) @name) @reference"
      )
      private$query_reference_records(
        query = query,
        root = root,
        path = path,
        role = "direct_call"
      )
    },

    namespace_references = function(root, path) {
      query <- treesitter::query(
        treesitter.r::language(),
        "(namespace_operator rhs: (identifier) @name) @reference"
      )
      private$query_reference_records(
        query = query,
        root = root,
        path = path,
        role = NULL
      ) |>
        map(\(reference) {
          parent <- node_parent(reference$node)
          reference$role <- if (
            r6_syntax_is_call_function(parent, reference$node)
          ) {
            "namespace_call"
          } else {
            "namespace_access"
          }
          reference$node <- NULL
          reference
        })
    },

    member_references = function(member_reference_facts) {
      member_reference_facts |>
        map(\(fact) {
          list(
            path = fact$path,
            line = fact$line,
            column = fact$column,
            end_line = fact$end_line,
            end_column = fact$end_column,
            name = fact$name,
            role = if (isTRUE(fact$is_call)) "member_call" else "member_access",
            source = "tree_sitter_source_index"
          )
        })
    },

    identifier_references = function(root, path) {
      query <- treesitter::query(treesitter.r::language(), "(identifier) @name")
      captures <- treesitter::query_captures(query, root)
      seq_along(captures$node) |>
        map(\(index) captures$node[[index]]) |>
        keep(private$is_identifier_reference) |>
        map(\(node) {
          private$reference_record(
            name_node = node,
            reference_node = node,
            path = path,
            role = "identifier"
          )
        })
    },

    query_reference_records = function(query, root, path, role) {
      matches <- treesitter::query_matches(query, root)[[1]]
      matches |>
        map(\(match) {
          name_node <- r6_syntax_capture_node(match, "name")
          reference_node <- r6_syntax_capture_node(match, "reference")
          private$reference_record(
            name_node = name_node,
            reference_node = reference_node,
            path = path,
            role = role
          )
        }) |>
        compact()
    },

    reference_record = function(name_node, reference_node, path, role) {
      if (is.null(name_node) || is.null(reference_node)) {
        return(NULL)
      }

      record <- private$location_record(name_node, path)
      record$end_line <- private$node_end_line(reference_node)
      record$end_column <- private$node_end_column(reference_node)
      record$name <- node_text(name_node)
      record$role <- role
      record$source <- "tree_sitter_source_index"
      record$node <- reference_node
      record
    },

    reference_matches_symbol = function(reference, symbol) {
      if (!identical(reference$name, symbol$name)) {
        return(FALSE)
      }

      kind <- str_to_lower(symbol$kind_name)
      if (identical(kind, "function")) {
        return(
          reference$role %in% c("direct_call", "namespace_call", "identifier")
        )
      }
      if (identical(kind, "object")) {
        return(reference$role %in% c("identifier", "namespace_access"))
      }
      if (identical(kind, "class")) {
        return(
          reference$role %in%
            c(
              "direct_call",
              "namespace_call",
              "identifier",
              "namespace_access"
            )
        )
      }
      if (kind %in% c("method", "field")) {
        return(reference$role %in% c("member_call", "member_access"))
      }

      FALSE
    },

    drop_declaration_reference = function(references, resolved) {
      keep(references, \(reference) {
        !identical(reference$path, resolved$path) ||
          !identical(reference$line, resolved$line) ||
          !identical(reference$column, resolved$column)
      })
    },

    is_identifier_reference = function(node) {
      parent <- node_parent(node)
      if (is.null(parent)) {
        return(TRUE)
      }

      parent_type <- node_type(parent)
      if (identical(parent_type, "binary_operator")) {
        lhs <- node_child_by_field_name(parent, "lhs")
        if (r6_syntax_same_node(node, lhs)) {
          return(FALSE)
        }
      }
      if (parent_type %in% c("parameter", "namespace_operator")) {
        return(FALSE)
      }
      if (identical(parent_type, "argument")) {
        name <- node_child_by_field_name(parent, "name")
        if (r6_syntax_same_node(node, name)) {
          return(FALSE)
        }
      }
      if (r6_syntax_is_call_function(parent, node)) {
        return(FALSE)
      }
      if (identical(parent_type, "extract_operator")) {
        rhs <- node_child_by_field_name(parent, "rhs")
        if (r6_syntax_same_node(node, rhs)) {
          return(FALSE)
        }
      }

      TRUE
    },

    function_signature = function(name, function_node) {
      parameters <- node_child_by_field_name(
        function_node,
        "parameters"
      )
      if (is.null(parameters)) {
        return(NULL)
      }

      parameter_labels <- node_named_children(parameters) |>
        keep(\(node) identical(node_type(node), "parameter")) |>
        map_chr(node_text)

      private$function_signature_from_labels(name, parameter_labels)
    },

    function_signature_from_labels = function(name, parameter_labels) {
      list(
        label = glue("{name}({glue_collapse(parameter_labels, ', ')})"),
        documentation = "",
        parameters = parameter_labels |>
          map(private$source_parameter_record) |>
          compact()
      )
    },

    source_parameter_record = function(parameter_label) {
      if (!nzchar(parameter_label)) {
        return(NULL)
      }
      record <- list(label = parameter_label)
      name <- str_trim(str_split_fixed(parameter_label, fixed("="), 2L)[[1]])
      if (nzchar(name)) {
        record$name <- name
      }
      if (str_detect(parameter_label, fixed("="))) {
        record$default <-
          str_split_fixed(parameter_label, fixed("="), 2L)[[2]] |>
          str_trim()
      }

      record
    },

    filter_symbol_names = function(candidates, name, exact = TRUE) {
      if (isTRUE(exact)) {
        return(keep(candidates, \(candidate) identical(candidate$name, name)))
      }

      keep(candidates, \(candidate) {
        str_detect(str_to_lower(candidate$name), fixed(str_to_lower(name))) ||
          str_detect(
            str_to_lower(candidate$qualified_name %||% ""),
            fixed(str_to_lower(name))
          )
      })
    },

    filter_symbol_kind = function(candidates, kind = NULL) {
      if (is.null(kind) || !nzchar(kind)) {
        return(candidates)
      }

      normalized <- str_to_lower(kind)
      is_object_alias <- normalized %in% c("variable", "constant", "object")
      is_class_alias <- normalized %in% c("r6_class", "class")
      is_field_alias <- normalized %in% c("member", "field")
      keep(candidates, \(candidate) {
        candidate_kind <- str_to_lower(candidate$kind_name)
        identical(candidate_kind, normalized) ||
          (is_object_alias && identical(candidate_kind, "object")) ||
          (is_class_alias && identical(candidate_kind, "class")) ||
          (is_field_alias && identical(candidate_kind, "field"))
      })
    },

    filter_symbol_path = function(candidates, path = NULL) {
      if (is.null(path) || !nzchar(path) || identical(path, ".")) {
        return(candidates)
      }

      keep(candidates, \(candidate) {
        identical(candidate$path, path) ||
          str_starts(candidate$path, fixed(path))
      })
    },

    symbol_status = function(symbols, query, path) {
      if (private$is_file_path(path) && is.null(query)) {
        return("file_outline")
      }

      self$candidate_status(symbols)
    },

    is_file_path = function(path) {
      !is.null(path) && str_detect(path, "[.][Rr]$")
    },

    reference_scope = function() {
      list(paths = c("R", "tests/testthat"), includes_tests = TRUE)
    },

    location_record = function(node, path) {
      start <- node_start_point(node)
      end <- node_end_point(node)
      list(
        path = path,
        line = as.integer(point_row(start)) + 1L,
        column = as.integer(point_column(start)) + 1L,
        end_line = as.integer(point_row(end)) + 1L,
        end_column = as.integer(point_column(end)) + 1L
      )
    },

    node_end_line = function(node) {
      end <- node_end_point(node)
      as.integer(point_row(end)) + 1L
    },

    node_end_column = function(node) {
      end <- node_end_point(node)
      as.integer(point_column(end)) + 1L
    }
  )
)
