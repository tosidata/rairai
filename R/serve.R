#' Serve the MCP tools
#'
#' Runs a Model Context Protocol (MCP) server over standard input and output.
#'
#' @param path Existing package directory to inspect by default. Accepts
#'   absolute, relative, and tilde paths. Defaults to the current working
#'   directory.
#'
#' @returns Called for its side effect of serving MCP requests over standard
#'   input and output. Blocks the current R process until standard input closes.
#'
#' @details
#' Tools that inspect a local package can use `package_root` to select a
#' different directory for one request. Supply an absolute path; this does not
#' change the server's default directory.
#'
#' @section File access and security:
#' **The `path` argument is not a sandbox.** It selects a default directory but
#' does not restrict access to other directories. An MCP client can request
#' any directory readable by the R process. Responses may disclose file names,
#' file contents, and absolute paths to that client. The inspected package's
#' code is not loaded or executed.
#'
#' To limit access, run the server with restricted filesystem permissions or
#' in a container that exposes only the files you intend to share.
#'
#' @seealso `vignette("tools", package = "rairai")` for the tool guide.
#'
#' @export
serve <- function(path = ".") {
  path <- normalize_serve_path(path)
  service <- PackageToolService$new(root_path = path)
  tools <- serve_tools(service)

  mcplite::mcp_server(
    tools = tools,
    instructions = serve_instructions(path)
  )
}

normalize_serve_path <- function(path) {
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("`path` must be a single non-empty path.", call. = FALSE)
  }

  normalized <- tryCatch(
    normalizePath(path, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL
  )

  if (is.null(normalized) || !dir.exists(normalized)) {
    stop("`path` must be an existing directory: ", path, call. = FALSE)
  }

  normalized
}

package_root_argument <- function() {
  mcplite::type_string(
    "Absolute path to the target package root.",
    required = FALSE
  )
}

resolve_package_root <- function(default_package_root, package_root = NULL) {
  if (
    length(package_root) == 0L ||
      (length(package_root) == 1L &&
        !is.na(package_root) &&
        !nzchar(package_root))
  ) {
    return(default_package_root)
  }

  valid_form <- is_nonempty_string(package_root) &&
    !str_starts(package_root, fixed("~")) &&
    !is_file_uri(package_root) &&
    fs::is_absolute_path(package_root)

  if (!valid_form) {
    abort_invalid_package_root(
      package_root,
      default_package_root,
      paste(
        "`package_root` must be a native absolute path, not a relative path,",
        "tilde path, or file URI."
      )
    )
  }

  normalized <- tryCatch(
    normalizePath(package_root, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL
  )
  if (is.null(normalized) || !dir.exists(normalized)) {
    abort_invalid_package_root(
      package_root,
      default_package_root,
      "`package_root` must be an existing directory."
    )
  }

  normalized
}

abort_invalid_package_root <- function(
  package_root,
  default_package_root,
  message
) {
  tool_error_response(
    error = list(
      code = "invalid_package_root",
      message = message,
      requested_package_root = package_root,
      default_package_root = default_package_root
    )
  )
}

root_bound_tool_call <- function(
  default_service,
  call,
  package_root = NULL
) {
  effective_root <- resolve_package_root(
    default_service$root_path,
    package_root
  )

  tryCatch(
    {
      service <- if (identical(effective_root, default_service$root_path)) {
        default_service
      } else {
        PackageToolService$new(root_path = effective_root)
      }
      response <- call(service)
      add_package_root(response, effective_root)
    },
    error = function(cnd) {
      message <- conditionMessage(cnd)
      if (is_json_object_text(message)) {
        stop(add_package_root(message, effective_root), call. = FALSE)
      }

      tool_error_response(
        package_root = effective_root,
        error = list(
          code = "unexpected_tool_error",
          message = message
        )
      )
    }
  )
}

add_package_root <- function(response, package_root) {
  if (!is_json_object_text(response)) {
    stop(
      "Package-local tool responses must be top-level JSON objects.",
      call. = FALSE
    )
  }

  root_json <- as.character(toJSON(package_root, auto_unbox = TRUE))
  remainder <- substring(response, 2L)
  separator <- if (identical(remainder, "}")) "" else ","

  paste0('{"package_root":', root_json, separator, remainder)
}

is_json_object_text <- function(value) {
  is_nonempty_string(value) &&
    str_starts(value, fixed("{")) &&
    str_ends(value, fixed("}")) &&
    jsonlite::validate(value)
}

serve_instructions <- function(
  default_package_root = normalize_serve_path(".")
) {
  paste(
    "These tools provide static R package intelligence for the current",
    "repository. Use them before making package-specific claims, and base",
    "conclusions on tool output.",
    "Start with package_overview() for orientation.",
    paste0(
      "Default target: ",
      default_package_root,
      ". For another target, pass package_root on each package-local call."
    ),
    "Use package tools for DESCRIPTION, NAMESPACE, files, docs, NEWS, vignettes,",
    "dependencies, and tests. Use symbols(), definition(), references(), and",
    "function_formals() for package-local source symbols, definitions,",
    "references, and function signatures.",
    "Use tests_for() before behavior changes and refactor_context() when",
    "evaluating source changes.",
    "Prefer the smallest tool that answers the question. If results are",
    "ambiguous, narrow by symbol, kind, or path before choosing a result.",
    "Use returned source locations to inspect only the relevant code."
  )
}

serve_tools <- function(service) {
  root_bound_tool_constructors <- list(
    tool_package_overview,
    tool_package_dependencies,
    tool_package_files,
    tool_package_namespace,
    tool_package_docs,
    tool_package_vignettes,
    tool_package_news,
    tool_tests_for,
    tool_help_page,
    tool_help_search,
    tool_r6_classes,
    tool_r6_class,
    tool_r6_method,
    tool_r6_method_references,
    tool_symbols,
    tool_definition,
    tool_references,
    tool_function_formals,
    tool_refactor_context
  )

  root_bound_tools <- map(root_bound_tool_constructors, \(constructor) {
    constructor(service)
  })

  c(
    root_bound_tools,
    list(
      tool_cran_search(),
      tool_cran_package()
    )
  )
}
