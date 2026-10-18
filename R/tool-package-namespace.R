read_package_namespace <- function(package = NULL, root_path = ".") {
  result <- package_namespace_data(package = package, root_path = root_path)
  if (!is.null(result$error)) {
    return(tool_error_response(!!!result))
  }

  tool_value_response(!!!result)
}

package_namespace_data <- function(
  package = NULL,
  compact_imports = TRUE,
  root_path = "."
) {
  namespace_path <- fs::path(root_path, "NAMESPACE")
  if (!file.exists(namespace_path)) {
    return(list(
      error = list(
        code = "namespace_not_found",
        message = "NAMESPACE file not found."
      )
    ))
  }

  directives <- namespace_directives(root_path = root_path)
  if (!is.null(directives$error)) {
    return(list(error = directives$error))
  }

  imports <- namespace_imports(directives)

  if (!is.null(package) && nzchar(package)) {
    return(namespace_focused_import(package, imports))
  }

  import_limit <- as.integer(getOption(
    "rairai.package_namespace.import_limit",
    20L
  ))
  response <- list(
    exports = sort(unique(directives$exports)) |> I(),
    imports = if (compact_imports) {
      namespace_compact_imports(imports$objects, limit = import_limit)
    } else {
      imports$objects
    },
    s3_methods = directives$s3_methods,
    use_dyn_lib = directives$use_dyn_lib
  )

  truncated <- namespace_imports_truncated(
    imports$objects,
    limit = import_limit
  )
  if (compact_imports && length(truncated) > 0L) {
    response$truncated <- truncated
  }
  if (length(imports$errors) > 0L) {
    response$errors <- imports$errors
  }

  response
}

namespace_directives <- function(root_path = ".") {
  parsed <- namespace_syntax_directives(root_path = root_path)
  if (!is.null(parsed$error)) {
    return(parsed)
  }

  result <- list(
    exports = character(),
    imports = list(),
    import_from = list(),
    s3_methods = list(),
    use_dyn_lib = list()
  )

  for (directive in parsed) {
    positional <- directive$positional
    named <- directive$named

    switch(
      directive$name,
      export = ,
      exportClasses = ,
      exportMethods = {
        result$exports <- c(result$exports, namespace_as_character(positional))
      },
      import = {
        if (length(positional) > 0L) {
          result$imports[[length(result$imports) + 1L]] <- list(
            package = namespace_first_character(positional),
            except = namespace_as_character(named[["except"]])
          )
        }
      },
      importFrom = ,
      importClassesFrom = ,
      importMethodsFrom = {
        if (length(positional) >= 2L) {
          result$import_from[[length(result$import_from) + 1L]] <- list(
            package = namespace_first_character(positional),
            objects = namespace_as_character(positional[-1])
          )
        }
      },
      S3method = {
        if (length(positional) >= 2L) {
          method <- list(
            generic = namespace_as_character(positional[[1L]]),
            class = namespace_as_character(positional[[2L]])
          )
          if (length(positional) >= 3L) {
            method$method <- namespace_as_character(positional[[3L]])
          }
          result$s3_methods[[length(result$s3_methods) + 1L]] <- method
        }
      },
      useDynLib = {
        if (length(positional) > 0L) {
          result$use_dyn_lib[[length(result$use_dyn_lib) + 1L]] <-
            namespace_use_dyn_lib(positional, named)
        }
      }
    )
  }

  result
}

namespace_syntax_directives <- function(root_path = ".") {
  lines <- tryCatch(
    readLines(fs::path(root_path, "NAMESPACE"), warn = FALSE),
    error = function(cnd) NULL
  )
  if (is.null(lines)) {
    return(list(
      error = list(
        code = "namespace_read_error",
        message = "NAMESPACE file could not be read."
      )
    ))
  }

  root <- lines |>
    enc2utf8() |>
    paste(collapse = "\n") |>
    treesitter::text_parse(treesitter.r::language())

  if (node_has_error(root)) {
    return(list(
      error = list(
        code = "namespace_parse_error",
        message = "NAMESPACE contains syntax errors and could not be parsed."
      )
    ))
  }

  namespace_syntax_calls(root) |>
    map(namespace_syntax_directive) |>
    compact()
}

namespace_syntax_calls <- function(node) {
  if (is.null(node)) {
    return(list())
  }

  type <- node_type(node)
  if (identical(type, "call")) {
    name <- namespace_syntax_call_name(node)
    if (!is.null(name) && name %in% namespace_supported_directives()) {
      return(list(node))
    }
    return(list())
  }

  if (identical(type, "if_statement")) {
    # Directives inside conditionals are discovered statically; conditions are
    # not evaluated.
    return(c(
      namespace_syntax_calls(node_child_by_field_name(
        node,
        "consequence"
      )),
      namespace_syntax_calls(node_child_by_field_name(
        node,
        "alternative"
      ))
    ))
  }

  if (type %in% c("program", "braced_expression", "parenthesized_expression")) {
    children <- node_named_children(node)
    calls <- lapply(children, namespace_syntax_calls)
    return(unlist(calls, recursive = FALSE, use.names = FALSE))
  }

  list()
}

namespace_syntax_directive <- function(call_node) {
  name <- namespace_syntax_call_name(call_node)
  if (is.null(name) || !name %in% namespace_supported_directives()) {
    return(NULL)
  }

  c(
    list(name = name),
    namespace_syntax_arguments(call_node)
  )
}

namespace_supported_directives <- function() {
  c(
    "export",
    "exportClasses",
    "exportMethods",
    "exportPattern",
    "import",
    "importFrom",
    "importClassesFrom",
    "importMethodsFrom",
    "S3method",
    "useDynLib"
  )
}

namespace_syntax_call_name <- function(call_node) {
  function_node <- node_child_by_field_name(call_node, "function")
  if (
    is.null(function_node) ||
      !identical(node_type(function_node), "identifier")
  ) {
    return(NULL)
  }

  namespace_syntax_name(node_text(function_node))
}

namespace_syntax_arguments <- function(call_node) {
  arguments <- node_child_by_field_name(call_node, "arguments")
  if (is.null(arguments)) {
    return(list(positional = list(), named = list()))
  }

  records <- node_named_children(arguments) |>
    keep(\(node) identical(node_type(node), "argument")) |>
    map(namespace_syntax_argument)
  named <- records |>
    keep(\(argument) !is.null(argument$name))
  named_values <- map(named, "value")
  names(named_values) <- map_chr(named, "name")

  list(
    positional = records |>
      keep(\(argument) is.null(argument$name)) |>
      map("value"),
    named = named_values
  )
}

namespace_syntax_argument <- function(argument_node) {
  name_node <- node_child_by_field_name(argument_node, "name")
  value_node <- node_child_by_field_name(argument_node, "value")

  list(
    name = if (!is.null(name_node)) {
      namespace_syntax_name(node_text(name_node))
    },
    value = namespace_syntax_value(value_node)
  )
}

namespace_syntax_value <- function(node) {
  if (is.null(node)) {
    return("")
  }

  switch(
    node_type(node),
    identifier = namespace_syntax_name(node_text(node)),
    string = namespace_syntax_string_value(node),
    true = TRUE,
    false = FALSE,
    integer = namespace_syntax_numeric_value(node),
    float = namespace_syntax_numeric_value(node),
    unary_operator = namespace_syntax_unary_value(node),
    call = namespace_syntax_call_value(node),
    namespace_syntax_name(node_text(node))
  )
}

namespace_syntax_call_value <- function(call_node) {
  if (!identical(namespace_syntax_call_name(call_node), "c")) {
    return(namespace_syntax_name(node_text(call_node)))
  }

  namespace_as_character(namespace_syntax_arguments(call_node)$positional)
}

namespace_syntax_string_value <- function(node) {
  node_named_children(node) |>
    keep(\(child) {
      node_type(child) %in% c("string_content", "escape_sequence")
    }) |>
    map_chr(node_text) |>
    paste(collapse = "")
}

namespace_syntax_numeric_value <- function(node) {
  value <- node_text(node) |>
    str_remove("[lL]$") |>
    as.numeric()
  if (is.na(value)) {
    return(node_text(node))
  }

  value
}

namespace_syntax_unary_value <- function(node) {
  operator <- namespace_syntax_operator(node)
  value <- namespace_syntax_value(node_child_by_field_name(node, "rhs"))
  if (identical(operator, "-") && is.numeric(value)) {
    return(-value)
  }

  namespace_syntax_name(node_text(node))
}

namespace_syntax_operator <- function(node) {
  operator <- node_children(node) |>
    detect(\(child) !node_is_named(child))
  if (is.null(operator)) {
    return(NULL)
  }

  node_text(operator)
}

namespace_syntax_name <- function(x) {
  str_remove_all(x, "^`|`$")
}

namespace_as_character <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(character())
  }

  if (is.list(x)) {
    x <- unlist(x, recursive = FALSE, use.names = FALSE)
  }

  as.character(x)
}

namespace_first_character <- function(x) {
  values <- namespace_as_character(x)
  if (length(values) == 0L) {
    return(NA_character_)
  }

  values[[1L]]
}

namespace_scalar_value <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  if (is.list(x)) {
    return(x[[1L]])
  }

  x[[1L]]
}

namespace_use_dyn_lib <- function(positional, named) {
  option_names <- c(".registration", ".fixes", ".useDynLib")
  option_args <- named[names(named) %in% option_names]
  native_args <- named[!names(named) %in% option_names]

  result <- list(library = namespace_first_character(positional))
  symbols <- c(namespace_as_character(positional[-1]), names(native_args))
  if (length(symbols) > 0L) {
    result$symbols <- sort(unique(symbols)) |> I()
  }
  if (length(native_args) > 0L) {
    result$native_symbols <- imap(native_args, \(routine, name) {
      list(name = name, routine = namespace_first_character(routine))
    })
  }

  for (name in names(option_args)) {
    result[[namespace_option_name(name)]] <- namespace_scalar_value(
      option_args[[name]]
    )
  }

  result
}

namespace_option_name <- function(x) {
  x |>
    str_remove("^\\.") |>
    str_replace_all(fixed("."), "_")
}

namespace_imports <- function(directives) {
  packages <- c(
    map_chr(directives$imports, "package"),
    map_chr(directives$import_from, "package")
  ) |>
    discard(is.na) |>
    unique() |>
    sort()

  objects <- setNames(vector("list", length(packages)), packages)
  errors <- list()

  for (entry in directives$imports) {
    package <- entry$package
    if (is.na(package)) {
      next
    }

    exports <- namespace_package_exports(package)
    if (is.list(exports) && !is.null(exports$error)) {
      errors[[package]] <- exports$error
      exports <- character()
    }

    objects[[package]] <- c(
      objects[[package]],
      setdiff(exports, entry$except %||% character())
    )
  }

  for (entry in directives$import_from) {
    package <- entry$package
    if (is.na(package)) {
      next
    }
    objects[[package]] <- c(objects[[package]], entry$objects)
  }

  objects <- objects |>
    map(~ sort(unique(.x)) |> I())

  list(objects = objects, errors = errors)
}

namespace_package_exports <- function(package) {
  tryCatch(
    {
      if (!requireNamespace(package, quietly = TRUE)) {
        stop("Package not available.", call. = FALSE)
      }
      sort(getNamespaceExports(package))
    },
    error = function(cnd) {
      list(
        error = list(
          code = "package_not_available",
          message = paste0(
            "Package '",
            package,
            "' is not available; bare import could not be expanded."
          )
        )
      )
    }
  )
}

namespace_compact_imports <- function(imports, limit = 20L) {
  imports |>
    map(~ head(.x, limit) |> I())
}

namespace_imports_truncated <- function(imports, limit = 20L) {
  imports |>
    keep(~ length(.x) > limit) |>
    imap(\(objects, package) list(shown = limit, total = length(objects)))
}

namespace_focused_import <- function(package, imports) {
  if (!package %in% names(imports$objects)) {
    return(list(
      package = package,
      error = list(
        code = "package_not_imported",
        message = paste0(
          "Package '",
          package,
          "' is not imported by this package."
        )
      )
    ))
  }

  response <- list(
    package = package,
    imports = imports$objects[package]
  )

  if (!is.null(imports$errors[[package]])) {
    response$error <- imports$errors[[package]]
  }

  response
}

tool_package_namespace <- function(service) {
  mcplite::tool(
    function(package = NULL, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_namespace(package = package)
        }
      )
    },
    name = "package_namespace",
    description = paste(
      "Read exports, imports, S3 registrations, and native-code declarations",
      "from NAMESPACE. Imported object lists are compact by default."
    ),
    arguments = list(
      package = mcplite::type_string(
        paste(
          "Imported package to inspect in full, such as 'dplyr' or 'stringr'.",
          "Leave empty for the namespace summary."
        ),
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
