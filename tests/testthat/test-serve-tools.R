mock_package_service <- function() {
  svc <- new.env(parent = emptyenv())
  svc$root_path <- normalizePath(".", winslash = "/", mustWork = TRUE)
  svc$package_dependencies <- function(depth = 1L) {
    tool_error_response(
      error = list(
        code = "description_not_found",
        message = "DESCRIPTION file not found.",
        path = "DESCRIPTION"
      )
    )
  }
  svc
}

expected_public_tools <- c(
  "package_overview",
  "package_dependencies",
  "package_files",
  "package_namespace",
  "package_docs",
  "package_vignettes",
  "package_news",
  "cran_search",
  "cran_package",
  "tests_for",
  "help_page",
  "help_search",
  "r6_classes",
  "r6_class",
  "r6_method",
  "r6_method_references",
  "symbols",
  "definition",
  "references",
  "function_formals",
  "refactor_context"
)

root_bound_public_tools <- setdiff(
  expected_public_tools,
  c("cran_search", "cran_package")
)

removed_public_tools <- c(
  "workspace_symbols",
  "document_symbols",
  "find_symbol",
  "symbol_definition_by_name",
  "symbol_references_by_name",
  "symbol_definition",
  "symbol_references",
  "find_test_for_symbol"
)

test_that("serve_tools exposes the single target public toolset", {
  tools <- rairai:::serve_tools(mock_package_service())
  tool_names <- mcp_tool_names(tools)

  expect_setequal(tool_names, expected_public_tools)
  expect_length(tool_names, 21L)
  expect_length(unique(tool_names), 21L)
  expect_length(setdiff(tool_names, c("cran_search", "cran_package")), 19L)
  expect_length(intersect(tool_names, c("cran_search", "cran_package")), 2L)
  expect_false(any(removed_public_tools %in% tool_names))
})

test_that("served tool schemas mark only required public arguments", {
  tools <- rairai:::serve_tools(mock_package_service())
  definitions <- setNames(
    map(tools, mcp_tool_definition),
    mcp_tool_names(tools)
  )
  expected_required <- list(
    package_overview = character(),
    package_dependencies = character(),
    package_files = character(),
    package_namespace = character(),
    package_docs = character(),
    package_vignettes = character(),
    package_news = character(),
    cran_search = "query",
    cran_package = "package_name",
    tests_for = "name",
    help_page = "topic",
    help_search = "query",
    r6_classes = character(),
    r6_class = "name",
    r6_method = c("class", "method"),
    r6_method_references = c("class", "method"),
    symbols = character(),
    definition = "name",
    references = "name",
    function_formals = "name",
    refactor_context = "name"
  )
  actual_required <- definitions |>
    map(\(definition) {
      required <- definition$inputSchema$required %||% character()
      unlist(required, use.names = FALSE)
    }) |>
    map(sort)

  expect_setequal(names(definitions), names(expected_required))
  expect_equal(
    actual_required[names(expected_required)],
    map(expected_required, sort)
  )
})

test_that("only package-local tools expose the optional package root", {
  tools <- rairai:::serve_tools(mock_package_service())
  definitions <- setNames(
    map(tools, mcp_tool_definition),
    mcp_tool_names(tools)
  )
  package_root_tools <- definitions |>
    keep(\(definition) {
      "package_root" %in% names(definition$inputSchema$properties)
    }) |>
    names()
  descriptions <- definitions[root_bound_public_tools] |>
    map_chr(\(definition) {
      definition$inputSchema$properties$package_root$description
    })
  required <- definitions[root_bound_public_tools] |>
    map(\(definition) definition$inputSchema$required %||% character())

  expect_setequal(package_root_tools, root_bound_public_tools)
  expect_length(unique(descriptions), 1L)
  expect_false(any(map_lgl(required, \(fields) "package_root" %in% fields)))
  expect_false(
    "package_root" %in%
      names(definitions$cran_search$inputSchema$properties)
  )
  expect_false(
    "package_root" %in%
      names(definitions$cran_package$inputSchema$properties)
  )
})

test_that("reference tools omit the declaration toggle", {
  tools <- rairai:::serve_tools(mock_package_service())
  definitions <- setNames(
    map(tools, mcp_tool_definition),
    mcp_tool_names(tools)
  )

  reference_arguments <- names(definitions$references$inputSchema$properties)
  context_arguments <- names(
    definitions$refactor_context$inputSchema$properties
  )

  expect_false("include_declaration" %in% reference_arguments)
  expect_false("include_declaration" %in% context_arguments)
  expect_contains(reference_arguments, "limit")
  expect_contains(context_arguments, "limit")
})

test_that("served tool errors are MCP tool-call errors with JSON text", {
  withr::local_dir(withr::local_tempdir())
  tools <- rairai:::serve_tools(mock_package_service())
  state <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 1,
      method = "initialize",
      params = list(
        protocolVersion = "2025-11-25",
        capabilities = structure(list(), names = character()),
        clientInfo = list(name = "rairai-tests", version = "0.0.0")
      )
    ),
    mcplite:::new_server_state(tools)
  )$state

  response <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 2,
      method = "tools/call",
      params = list(
        name = "package_dependencies",
        arguments = list(depth = 1L)
      )
    ),
    state
  )
  text <- response$response$result$content[[1]]$text

  payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  expect_null(response$response$error)
  expect_true(response$response$result$isError)
  expect_equal(payload$package_root, mock_package_service()$root_path)
  expect_equal(payload$error$code, "description_not_found")
})

test_that("root-bound responses preserve existing serialized JSON text", {
  service <- mock_package_service()
  original <- '{"null_value":null,"object":{},"array":[]}'

  result <- rairai:::root_bound_tool_call(
    service,
    call = function(selected_service) {
      expect_identical(selected_service, service)
      original
    }
  )
  root_json <- as.character(jsonlite::toJSON(
    service$root_path,
    auto_unbox = TRUE
  ))

  expect_identical(
    result,
    paste0('{"package_root":', root_json, ",", substring(original, 2L))
  )
})

test_that("root-bound calls reuse the immutable default service", {
  service <- mock_package_service()
  observe_service <- function(selected_service) {
    expect_identical(selected_service, service)
    "{}"
  }

  omitted <- rairai:::root_bound_tool_call(service, call = observe_service)
  null <- rairai:::root_bound_tool_call(
    service,
    call = observe_service,
    package_root = NULL
  )
  zero_length <- rairai:::root_bound_tool_call(
    service,
    call = observe_service,
    package_root = character()
  )
  empty <- rairai:::root_bound_tool_call(
    service,
    call = observe_service,
    package_root = ""
  )
  default_alias <- file.path(
    service$root_path,
    "..",
    basename(service$root_path)
  )
  canonical_default <- rairai:::root_bound_tool_call(
    service,
    call = observe_service,
    package_root = default_alias
  )

  expect_identical(omitted, null)
  expect_identical(null, zero_length)
  expect_identical(zero_length, empty)
  expect_identical(empty, canonical_default)
})

test_that("invalid JSON-like errors become structured MCP errors", {
  service <- mock_package_service()
  service$package_dependencies <- function(depth = 1L) {
    stop("{boom}", call. = FALSE)
  }
  tools <- rairai:::serve_tools(service)
  state <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 1,
      method = "initialize",
      params = list(
        protocolVersion = "2025-11-25",
        capabilities = structure(list(), names = character()),
        clientInfo = list(name = "rairai-tests", version = "0.0.0")
      )
    ),
    mcplite:::new_server_state(tools)
  )$state

  response <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 2,
      method = "tools/call",
      params = list(
        name = "package_dependencies",
        arguments = list(depth = 1L)
      )
    ),
    state
  )
  text <- response$response$result$content[[1]]$text
  payload <- jsonlite::fromJSON(text, simplifyVector = FALSE)

  expect_null(response$response$error)
  expect_true(response$response$result$isError)
  expect_equal(payload$package_root, service$root_path)
  expect_equal(payload$error$code, "unexpected_tool_error")
  expect_equal(payload$error$message, "{boom}")
})

test_that("serve_instructions recommends target workflow routes", {
  instructions <- rairai:::serve_instructions()

  expect_match(instructions, "package_overview()", fixed = TRUE)
  expect_match(
    instructions,
    "symbols(), definition(), references()",
    fixed = TRUE
  )
  expect_match(instructions, "tests_for()", fixed = TRUE)
  expect_match(instructions, "refactor_context()", fixed = TRUE)
  expect_no_match(instructions, "diagnostics()", fixed = TRUE)
  expect_match(instructions, "Default target:", fixed = TRUE)
  expect_match(instructions, "package_root", fixed = TRUE)
  expect_match(
    instructions,
    normalizePath(".", winslash = "/", mustWork = TRUE),
    fixed = TRUE
  )
  expect_match(
    instructions,
    "on each package-local call",
    fixed = TRUE
  )
  expect_no_match(instructions, "Start another server", fixed = TRUE)
  expect_no_match(
    instructions,
    "broad workspace and positional tools",
    fixed = TRUE
  )
})

mock_serve_runtime <- function(observed, env = parent.frame()) {
  testthat::local_mocked_bindings(
    serve_tools = function(service) {
      observed$root_path <- service$root_path
      observed$setup_wd <- getwd()
      list()
    },
    .package = "rairai",
    .env = env
  )
  testthat::local_mocked_bindings(
    mcp_server = function(tools, instructions) {
      list(
        server_wd = getwd(),
        tools = tools,
        instructions = instructions
      )
    },
    .package = "mcplite",
    .env = env
  )
}

test_that("serve uses explicit path without changing working directory", {
  parent <- withr::local_tempdir()
  target <- file.path(parent, "target")
  dir.create(target)
  withr::local_dir(parent)
  observed <- new.env(parent = emptyenv())
  original_wd <- getwd()
  expected_path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  mock_serve_runtime(observed)

  result <- rairai::serve(path = "target")

  expect_equal(observed$root_path, expected_path)
  expect_equal(observed$setup_wd, original_wd)
  expect_equal(result$server_wd, original_wd)
  expect_equal(result$tools, list())
  expect_match(result$instructions, expected_path, fixed = TRUE)
  expect_equal(getwd(), original_wd)
})

test_that("serve defaults to the current working directory", {
  target <- withr::local_tempdir()
  withr::local_dir(target)
  observed <- new.env(parent = emptyenv())
  original_wd <- getwd()
  expected_path <- normalizePath(target, winslash = "/", mustWork = TRUE)
  mock_serve_runtime(observed)

  result <- rairai::serve()

  expect_equal(observed$root_path, expected_path)
  expect_equal(observed$setup_wd, expected_path)
  expect_equal(result$server_wd, expected_path)
  expect_match(result$instructions, expected_path, fixed = TRUE)
  expect_equal(getwd(), original_wd)
})

test_that("serve path normalization expands common path forms", {
  expect_equal(
    rairai:::normalize_serve_path("~"),
    normalizePath("~", winslash = "/", mustWork = TRUE)
  )
})

test_that("serve path must be one existing directory", {
  for (path in list(NULL, character(), "", NA_character_, c("a", "b"))) {
    expect_error(
      rairai::serve(path = path),
      "`path` must be a single non-empty path.",
      fixed = TRUE
    )
  }
  file <- withr::local_tempfile(lines = "not a directory")
  for (path in list(file, tempfile("missing-serve-root-"), 42)) {
    expect_error(
      rairai::serve(path = path),
      "`path` must be an existing directory:",
      fixed = TRUE
    )
  }
})

test_that("root-bound response checks retain malformed value errors", {
  service <- mock_package_service()
  for (value in list(NULL, character(), "", NA_character_, 42, c("{}", "{}"))) {
    result <- tool_result_json(
      rairai:::root_bound_tool_call(service, function(selected_service) value),
      simplifyVector = FALSE
    )
    expect_equal(result$package_root, service$root_path)
    expect_equal(result$error$code, "unexpected_tool_error")
    expect_match(result$error$message, "top-level JSON objects", fixed = TRUE)
  }
})
