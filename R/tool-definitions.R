tool_symbols <- function(service) {
  mcplite::tool(
    function(
      query = NULL,
      path = NULL,
      kind = NULL,
      exact = TRUE,
      limit = 50L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$symbols(
            query = query,
            path = path,
            kind = kind,
            exact = exact,
            limit = limit
          )
        }
      )
    },
    name = "symbols",
    description = "Find source symbols by name, kind, or file.",
    arguments = list(
      query = mcplite::type_string(
        "Symbol name or query.",
        required = FALSE
      ),
      path = mcplite::type_string(
        "File path for an outline, or path prefix to limit the search.",
        required = FALSE
      ),
      kind = mcplite::type_string(
        "Symbol kind, such as Function, Method, Class, or Variable.",
        required = FALSE
      ),
      exact = mcplite::type_boolean(
        "Require an exact symbol-name match. Default: true.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of symbols. Default: 50.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}

tool_definition <- function(service) {
  mcplite::tool(
    function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 20L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$definition(
            name = name,
            kind = kind,
            path = path,
            exact = exact,
            limit = limit
          )
        }
      )
    },
    name = "definition",
    description = paste(
      "Resolve a package-local symbol to one canonical Tree-sitter definition."
    ),
    arguments = list(
      name = mcplite::type_string(
        "Symbol name or query."
      ),
      kind = mcplite::type_string(
        "Symbol kind, such as Function, Method, Class, or Variable.",
        required = FALSE
      ),
      path = mcplite::type_string(
        "Path prefix to limit candidates.",
        required = FALSE
      ),
      exact = mcplite::type_boolean(
        "Require an exact symbol-name match. Default: true.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of candidates. Default: 20.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}

tool_references <- function(service) {
  mcplite::tool(
    function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 100L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$references(
            name = name,
            kind = kind,
            path = path,
            exact = exact,
            limit = limit
          )
        }
      )
    },
    name = "references",
    description = paste(
      "Find Tree-sitter syntactic impact references for a package-local",
      "symbol. The resolved declaration is reported separately."
    ),
    arguments = list(
      name = mcplite::type_string(
        "Symbol name or query."
      ),
      kind = mcplite::type_string(
        "Symbol kind, such as Function, Method, Class, or Variable.",
        required = FALSE
      ),
      path = mcplite::type_string(
        "Path prefix to limit candidates.",
        required = FALSE
      ),
      exact = mcplite::type_boolean(
        "Require an exact symbol-name match. Default: true.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of references. Default: 100.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}

tool_function_formals <- function(service) {
  mcplite::tool(
    function(name, package = NULL, context_path = NULL, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$function_formals(
            name = name,
            package = package,
            context_path = context_path
          )
        }
      )
    },
    name = "function_formals",
    description = paste(
      "Show a local function signature from package source, or report that",
      "external/package-qualified signatures require help tools."
    ),
    arguments = list(
      name = mcplite::type_string(
        "Function name, optionally qualified like 'stats::lm'."
      ),
      package = mcplite::type_string(
        "Package used to identify an external function name.",
        required = FALSE
      ),
      context_path = mcplite::type_string(
        "Source file used to validate local path context.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}

tool_refactor_context <- function(service) {
  mcplite::tool(
    function(
      name,
      kind = NULL,
      path = NULL,
      exact = TRUE,
      limit = 100L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$refactor_context(
            name = name,
            kind = kind,
            path = path,
            exact = exact,
            limit = limit
          )
        }
      )
    },
    name = "refactor_context",
    description = paste(
      "Gather refactor-readiness context for a symbol: definition,",
      "references, tests, and export status."
    ),
    arguments = list(
      name = mcplite::type_string(
        "Symbol name."
      ),
      kind = mcplite::type_string(
        "Symbol kind, such as Function, Method, Class, or Variable.",
        required = FALSE
      ),
      path = mcplite::type_string(
        "Path prefix to limit candidates.",
        required = FALSE
      ),
      exact = mcplite::type_boolean(
        "Require an exact symbol-name match. Default: true.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of references. Default: 100.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
