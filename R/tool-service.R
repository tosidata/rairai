PackageToolService <- R6Class(
  "PackageToolService",
  public = list(
    root_path = NULL,
    source_index = NULL,

    # Root selection and canonicalization belong to serve.R.
    initialize = function(root_path) {
      self$root_path <- root_path
      self$source_index <- SourceIndex$new(root_path = root_path)
    },

    package_overview = function() {
      package_overview(root_path = self$root_path)
    },

    package_dependencies = function(depth = 1L) {
      read_package_dependencies(depth = depth, root_path = self$root_path)
    },

    package_files = function(scope = "R", include = character()) {
      read_package_files(
        scope = scope,
        include = include,
        root_path = self$root_path
      )
    },

    package_namespace = function(package = NULL) {
      read_package_namespace(package = package, root_path = self$root_path)
    },

    package_docs = function(package = NULL, limit = 20L, offset = 0L) {
      package_docs(
        package = package,
        limit = limit,
        offset = offset,
        root_path = self$root_path
      )
    },

    package_vignettes = function(package = NULL, name = NULL) {
      package_vignettes(
        package = package,
        name = name,
        root_path = self$root_path
      )
    },

    package_news = function(package = NULL, version = NULL, keyword = NULL) {
      package_news(
        package = package,
        version = version,
        keyword = keyword,
        root_path = self$root_path
      )
    },

    tests_for = function(name) {
      tests_for(name = name, root_path = self$root_path)
    },

    help_page = function(
      topic,
      package = NULL,
      sections = NULL,
      max_chars = NULL
    ) {
      help_page(
        topic = topic,
        package = package,
        sections = sections,
        max_chars = max_chars,
        root_path = self$root_path
      )
    },

    help_search = function(query, package = NULL, limit = 20L, offset = 0L) {
      help_search(
        query = query,
        package = package,
        limit = limit,
        offset = offset,
        root_path = self$root_path
      )
    },

    r6_classes = function() {
      r6_classes(root_path = self$root_path)
    },

    r6_class = function(name) {
      r6_class(name = name, root_path = self$root_path)
    },

    r6_method = function(class, method) {
      r6_method(class = class, method = method, root_path = self$root_path)
    },

    r6_method_references = function(class, method) {
      r6_method_references(
        class = class,
        method = method,
        root_path = self$root_path
      )
    },

    symbols = function(
      query = NULL,
      path = NULL,
      kind = NULL,
      exact = TRUE,
      limit = 50L
    ) {
      query <- normalize_optional_string(query, default = NULL)
      path <- normalize_optional_string(path, default = NULL)
      kind <- normalize_optional_string(kind, default = NULL)
      if (identical(str_to_lower(kind %||% ""), "all")) {
        kind <- NULL
      }
      exact <- normalize_logical_value(exact, default = TRUE)
      limit <- normalize_limit_value(limit, default = 50L)

      result <- self$source_index$symbols(
        query = query,
        path = path,
        kind = kind,
        exact = exact,
        limit = limit
      )
      if (has_tool_error(result)) {
        return(tool_error_response(error = result$error))
      }

      response_symbols <- compact_location_records(
        result$symbols,
        include_leading_comment = TRUE,
        source_root = self$root_path
      )
      response <- list(
        query = result$query,
        status = result$status,
        exact = exact,
        filters = result$filters,
        count = length(response_symbols),
        total_count = result$total_count,
        truncated = result$truncated,
        limit = result$limit,
        symbols = response_symbols
      )
      if (!is.null(result$path)) {
        response$path <- result$path
      }
      if (length(result$parse_errors %||% list()) > 0L) {
        response$parse_errors <- result$parse_errors
      }
      if (identical(result$status, "none")) {
        response$next_tools <- private$symbols_no_match_next_tools(
          query = query,
          exact = exact
        )
      }

      tool_value_response(!!!response)
    },

    definition = function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 20L
    ) {
      exact <- normalize_logical_value(exact, default = TRUE)
      limit <- normalize_limit_value(limit, default = 20L)
      resolution <- self$source_index$resolve_symbol_candidates(
        name = name,
        kind = normalize_optional_string(kind, default = NULL),
        path = normalize_optional_string(path, default = NULL),
        exact = exact,
        limit = limit
      )
      if (has_tool_error(resolution)) {
        return(tool_error_response(error = resolution$error))
      }

      response <- self$source_index$symbol_resolution_response(resolution)
      if (!identical(response$status, "unique")) {
        return(tool_value_response(!!!response))
      }

      resolved <- resolution$candidates[[1]]
      response$resolved <- compact_location_fields(
        resolved,
        include_leading_comment = TRUE,
        source_root = self$root_path
      )
      response$candidates <- NULL

      tool_value_response(!!!response)
    },

    references = function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 100L
    ) {
      exact <- normalize_logical_value(exact, default = TRUE)
      limit <- normalize_limit_value(limit, default = 100L)
      resolution <- self$source_index$resolve_symbol_candidates(
        name = name,
        kind = normalize_optional_string(kind, default = NULL),
        path = normalize_optional_string(path, default = NULL),
        exact = exact,
        limit = limit
      )
      if (has_tool_error(resolution)) {
        return(tool_error_response(error = resolution$error))
      }

      response <- self$source_index$symbol_resolution_response(resolution)
      if (!identical(response$status, "unique")) {
        return(tool_value_response(!!!response))
      }

      resolved <- resolution$candidates[[1]]
      reference_result <- self$source_index$references(
        resolved = resolved,
        limit = limit
      )
      response$status <- reference_result$status
      response$resolved <- compact_location_fields(
        resolved,
        include_leading_comment = TRUE,
        source_root = self$root_path
      )
      response$reference_semantics <- "syntactic_impact_references"
      response$binding_identity <- "not_proven"
      response$declaration <- "resolved_only"
      response$reference_scope <- reference_result$reference_scope
      response$references <- compact_location_records(
        reference_result$references
      )
      response$count <- length(response$references)
      response$total_count <- reference_result$total_count
      response$truncated <- reference_result$truncated
      response$limit <- reference_result$limit
      member_match <- private$r6_member_reference_match(resolved)
      if (!is.null(member_match)) {
        response$r6_member_reference_match <- member_match
      }
      response$candidates <- NULL
      if (length(reference_result$parse_errors %||% list()) > 0L) {
        response$parse_errors <- reference_result$parse_errors
      }

      tool_value_response(!!!response)
    },

    function_formals = function(name, package = NULL, context_path = NULL) {
      package <- normalize_optional_string(package, default = NULL)
      context_path <- normalize_optional_string(context_path, default = NULL)
      context_path <- normalize_root_relative_path(
        context_path,
        root_path = self$root_path,
        argument = "context_path"
      )
      if (has_tool_error(context_path)) {
        return(tool_error_response(error = context_path$error))
      }
      if (
        !is.null(context_path) &&
          fs::dir_exists(normalize_absolute_path(
            context_path,
            root_path = self$root_path
          ))
      ) {
        return(tool_error_response(
          error = list(
            code = "path_not_file",
            message = "`context_path` must be a file, not a directory.",
            data = list(argument = "context_path", path = context_path)
          )
        ))
      }

      call_reference <- private$function_reference(name, package = package)
      if (has_tool_error(call_reference)) {
        return(tool_error_response(error = call_reference$error))
      }
      if (!is.null(call_reference$external)) {
        return(tool_value_response(
          !!!private$external_function_formals_response(call_reference)
        ))
      }

      local_formals <- self$source_index$local_function_formals(
        name = call_reference$name
      )
      if (identical(local_formals$status, "ambiguous")) {
        return(tool_value_response(!!!local_formals))
      }
      if (identical(local_formals$status, "unique")) {
        resolved <- local_formals$resolved
        resolved$source <- "tree_sitter_source_index"
        response <- list(
          query = name,
          status = "unique",
          source = "tree_sitter_source_index",
          resolved = compact_location_fields(
            resolved,
            include_leading_comment = TRUE,
            source_root = self$root_path
          ),
          count = 1L,
          signatures = list(local_formals$signature)
        )
        if (!is.null(context_path)) {
          response$context_path <- context_path
        }
        return(tool_value_response(!!!response))
      }

      response <- list(
        query = name,
        status = "none",
        source = "tree_sitter_source_index",
        count = 0L,
        signatures = list(),
        next_tools = list(list(
          tool = "help_search",
          reason = paste(
            "Search installed R help if this is not a package-local",
            "function. rairai does not execute target packages to inspect",
            "external formals."
          )
        ))
      )
      if (!is.null(context_path)) {
        response$context_path <- context_path
      }

      tool_value_response(!!!response)
    },

    refactor_context = function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 100L
    ) {
      exact <- normalize_logical_value(exact, default = TRUE)
      limit <- normalize_limit_value(limit, default = 100L)

      resolution <- self$source_index$resolve_symbol_candidates(
        name = name,
        kind = normalize_optional_string(kind, default = NULL),
        path = normalize_optional_string(path, default = NULL),
        exact = exact,
        limit = limit
      )
      if (has_tool_error(resolution)) {
        return(tool_error_response(error = resolution$error))
      }

      base_response <- self$source_index$symbol_resolution_response(resolution)
      if (!identical(base_response$status, "unique")) {
        base_response$next_tools <- private$refactor_context_next_tools(
          status = base_response$status,
          has_tests = FALSE
        )
        return(tool_value_response(!!!base_response))
      }

      resolved <- resolution$candidates[[1]]
      reference_result <- self$source_index$references(
        resolved = resolved,
        limit = limit
      )
      reference_locations <- reference_result$references
      related_tests <- private$related_tests_for_symbol(name)

      response_references <- compact_location_records(reference_locations)
      ns_exported <- private$is_symbol_exported(name)

      response <- list(
        query = name,
        status = base_response$status,
        exact = base_response$exact,
        filters = base_response$filters,
        resolved = compact_location_fields(
          resolved,
          include_leading_comment = TRUE,
          source_root = self$root_path
        ),
        reference_count = length(response_references),
        reference_total_count = reference_result$total_count,
        references_truncated = reference_result$truncated,
        reference_limit = reference_result$limit,
        references = response_references,
        reference_semantics = "syntactic_impact_references",
        binding_identity = "not_proven",
        declaration = "resolved_only",
        reference_scope = reference_result$reference_scope,
        exported = ns_exported,
        namespace = list(exported = ns_exported),
        related_test_count = related_tests$count,
        related_tests = related_tests$matches,
        test_text_hit_count = related_tests$hit_count,
        test_text_hits = related_tests$hits,
        next_tools = private$refactor_context_next_tools(
          status = base_response$status,
          has_tests = related_tests$count > 0L
        )
      )
      member_match <- private$r6_member_reference_match(resolved)
      if (!is.null(member_match)) {
        response$r6_member_reference_match <- member_match
      }

      tool_value_response(!!!response)
    }
  ),
  private = list(
    function_reference = function(name, package = NULL) {
      qualified <- private$parse_qualified_reference(name)
      if (!is.null(package) && !is.null(qualified)) {
        return(tool_error_record(
          code = "invalid_function_reference",
          message = paste(
            "Provide either a qualified name or a package argument,",
            "not both."
          )
        ))
      }

      if (!is.null(package)) {
        reference <- paste0(package, "::", name)
        if (!private$is_valid_function_reference(reference)) {
          return(private$invalid_function_reference_error())
        }
        return(list(
          name = name,
          external = TRUE,
          package = package,
          qualified_name = reference,
          resolved_source = "package_argument"
        ))
      }

      if (!is.null(qualified)) {
        return(list(
          name = qualified$name,
          external = TRUE,
          package = qualified$package,
          qualified_name = name,
          resolved_source = "qualified_name"
        ))
      }

      if (!private$is_valid_function_reference(name)) {
        return(private$invalid_function_reference_error())
      }

      list(name = name)
    },

    parse_qualified_reference = function(name) {
      match <- str_match(
        name,
        "^([A-Za-z.][A-Za-z0-9._]*)(::|:::)([A-Za-z.][A-Za-z0-9._]*)$"
      )
      if (is.na(match[[1]])) {
        return(NULL)
      }

      list(package = match[[2]], operator = match[[3]], name = match[[4]])
    },

    is_valid_function_reference = function(reference) {
      identifier <- "[A-Za-z.][A-Za-z0-9._]*"
      pattern <- paste0("^(", identifier, "(::|:::))?", identifier, "$")
      str_detect(reference, pattern)
    },

    invalid_function_reference_error = function() {
      tool_error_record(
        code = "invalid_function_reference",
        message = paste(
          "Function name must be an R identifier, optionally qualified",
          "as pkg::name or pkg:::name."
        )
      )
    },

    external_function_formals_response = function(call_reference) {
      list(
        query = call_reference$qualified_name,
        status = "unsupported",
        source = "external_formals_unavailable",
        resolved = list(
          name = call_reference$name,
          package = call_reference$package,
          qualified_name = call_reference$qualified_name,
          source = call_reference$resolved_source
        ),
        package = call_reference$package,
        count = 0L,
        signatures = list(),
        limitation = paste(
          "External and package-qualified function signatures are not",
          "available from static package source analysis. rairai does not",
          "execute or load target packages to inspect them."
        ),
        next_tools = list(
          list(
            tool = "help_page",
            reason = paste0(
              "Read the help page with topic = '",
              call_reference$name,
              "' and package = '",
              call_reference$package,
              "'."
            )
          ),
          list(
            tool = "help_search",
            reason = "Search R help topics if the exact help page is absent."
          )
        )
      )
    },

    r6_member_reference_match = function(resolved) {
      if (
        !identical(resolved$kind_name, "Method") &&
          !identical(resolved$kind_name, "Field")
      ) {
        return(NULL)
      }

      list(
        matched_by = "member_name_only",
        receiver_object_proven = FALSE,
        class_membership_proven = FALSE,
        may_include_unrelated_objects = TRUE
      )
    },

    related_tests_for_symbol = function(name) {
      result <- tests_for_data(name, root_path = self$root_path)
      if (!is.null(result$error)) {
        return(list(
          count = 0L,
          matches = list(),
          hit_count = 0L,
          hits = list()
        ))
      }

      matches <- result$matches %||% list()
      hits <- matches |>
        map("hits") |>
        list_flatten()

      list(
        count = result$count %||% 0L,
        matches = matches,
        hit_count = length(hits),
        hits = hits
      )
    },

    is_symbol_exported = function(name) {
      namespace_path <- file.path(self$root_path, "NAMESPACE")
      if (!file.exists(namespace_path)) {
        return(FALSE)
      }

      lines <- tryCatch(
        readLines(namespace_path, warn = FALSE),
        error = function(cnd) character()
      )

      any(str_detect(lines, paste0("^export\\(", str_escape(name), "\\)")))
    },

    refactor_context_next_tools = function(status, has_tests = FALSE) {
      if (!identical(status, "unique")) {
        return(list(list(
          tool = "symbols",
          reason = "Narrow the symbol by query, kind, or path before editing."
        )))
      }

      hints <- list(
        list(
          tool = "definition",
          reason = "Inspect the resolved definition before editing source."
        ),
        list(
          tool = "references",
          reason = "Review call sites and dependent code before refactoring."
        ),
        list(
          tool = "package_namespace",
          reason = "Check whether the symbol is part of the exported API."
        )
      )

      if (isTRUE(has_tests)) {
        hints <- append(
          hints,
          list(list(
            tool = "tests_for",
            reason = "Inspect related tests before changing behavior."
          ))
        )
      }

      hints
    },

    symbols_no_match_next_tools = function(query = NULL, exact = TRUE) {
      hints <- list()
      if (!is.null(query)) {
        hints <- append(
          hints,
          list(list(
            tool = "definition",
            reason = paste(
              "Try name-based resolution if symbol listing returned no",
              "result."
            )
          ))
        )
      }
      if (!is.null(query) && isTRUE(exact)) {
        hints <- append(
          hints,
          list(list(
            tool = "symbols",
            reason = "Retry with exact = FALSE to broaden symbol-name matching."
          ))
        )
      }

      append(
        hints,
        list(
          list(
            tool = "symbols",
            reason = "Retry with a path when the likely source file is known."
          ),
          list(
            tool = "package_files",
            reason = paste(
              "Inspect likely source files if the symbol index is",
              "incomplete."
            )
          )
        )
      )
    }
  )
)
