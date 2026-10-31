root_isolation_json <- function(result) {
  tool_result_json(result, simplifyVector = FALSE)
}

local_root_isolation_package <- function(parent, package, value) {
  root <- file.path(parent, package)
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  dir.create(file.path(root, "man"), recursive = TRUE)
  dir.create(file.path(root, "vignettes"), recursive = TRUE)

  writeLines(
    c(
      paste0("Package: ", package),
      "Title: Root Isolation Fixture",
      "Version: 0.0.0.9000",
      "Authors@R: person('Test', 'User', role = c('aut', 'cre'), email = 'test@example.com')",
      paste0("Description: Fixture package ", value, "."),
      "License: MIT",
      "Encoding: UTF-8",
      "Imports: R6",
      "Suggests: testthat"
    ),
    file.path(root, "DESCRIPTION")
  )
  writeLines(
    c(
      "export(shared_target)",
      "importFrom(R6,R6Class)"
    ),
    file.path(root, "NAMESPACE")
  )
  writeLines(
    c(
      paste0("#' ", value, " function docs"),
      "shared_target <- function() {",
      paste0("  '", value, "'"),
      "}",
      "",
      paste0(value, "Class <- R6::R6Class("),
      paste0("  '", value, "Class',"),
      "  public = list(",
      "    run = function() shared_target()",
      "  )",
      ")"
    ),
    file.path(root, "R", "core.R")
  )
  writeLines(
    c(
      paste0("# ", value, " test context"),
      paste0("test_that('", value, " shared target works', {"),
      "  expect_equal(shared_target(), 'unused')",
      "})"
    ),
    file.path(root, "tests", "testthat", "test-shared.R")
  )
  writeLines(
    c(
      "\\name{shared_target}",
      "\\alias{shared_target}",
      paste0("\\title{", value, " help title}"),
      paste0("\\description{", value, " help description.}"),
      "\\usage{shared_target()}",
      paste0("\\value{", value, " value.}")
    ),
    file.path(root, "man", "shared_target.Rd")
  )
  writeLines(
    c(
      "---",
      paste0("title: '", value, " guide'"),
      "---",
      "",
      paste0("# ", value, " guide"),
      paste0("Vignette content for ", value, ".")
    ),
    file.path(root, "vignettes", "guide.qmd")
  )
  writeLines(
    c(
      paste0("# ", package, " (development version)"),
      "",
      paste0("- ", value, " NEWS entry.")
    ),
    file.path(root, "NEWS.md")
  )

  normalizePath(root, winslash = "/", mustWork = TRUE)
}

root_isolation_server_state <- function(default_root) {
  tools <- rairai:::serve_tools(
    rairai:::PackageToolService$new(root_path = default_root)
  )

  mcplite:::dispatch_message(
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
}

root_isolation_tool_call <- function(state, name, arguments) {
  dispatched <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 2,
      method = "tools/call",
      params = list(name = name, arguments = arguments)
    ),
    state
  )$response
  result <- dispatched$result
  text <- result$content[[1L]]$text

  list(
    protocol_error = dispatched$error,
    is_error = isTRUE(result$isError),
    text = text,
    payload = jsonlite::fromJSON(text, simplifyVector = FALSE)
  )
}

test_that("PackageToolService package-local tools use the active root from any cwd", {
  parent <- withr::local_tempdir()
  target <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  sibling_cwd <- file.path(parent, "sibling-cwd")
  dir.create(sibling_cwd)
  withr::local_dir(sibling_cwd)

  service <- rairai:::PackageToolService$new(root_path = target)

  overview <- root_isolation_json(service$package_overview())
  expect_equal(overview$description$fields$Package, "alphaPkg")
  expect_equal(unlist(overview$namespace$exports), "shared_target")
  expect_equal(overview$tests$test_count, 1L)

  dependencies <- root_isolation_json(service$package_dependencies())
  expect_equal(unlist(dependencies$dependencies$imports), "R6")

  files <- root_isolation_json(service$package_files(scope = "R"))
  expect_equal(files$files[[1]]$path, "R/core.R")

  namespace <- root_isolation_json(service$package_namespace())
  expect_equal(unlist(namespace$exports), "shared_target")

  tests <- root_isolation_json(service$tests_for("shared_target"))
  expect_equal(tests$matches[[1]]$location, "tests/testthat/test-shared.R:2-4")
  expect_equal(tests$matches[[1]]$desc, "alpha shared target works")

  refactor <- root_isolation_json(service$refactor_context("shared_target"))
  expect_equal(refactor$related_tests[[1]]$desc, "alpha shared target works")
  expect_equal(refactor$test_text_hits, list("tests/testthat/test-shared.R:3"))

  docs <- root_isolation_json(service$package_docs())
  expect_equal(docs$package, "alphaPkg")
  expect_equal(docs$help$topics[[1]]$title, "alpha help title")

  help <- root_isolation_json(service$help_page("shared_target"))
  expect_equal(help$source, "local")
  expect_equal(help$title, "alpha help title")

  help_matches <- root_isolation_json(service$help_search("alpha help title"))
  local_match <- detect(help_matches$matches, \(match) {
    identical(match$source, "local")
  })
  expect_equal(local_match$package, "alphaPkg")

  vignettes <- root_isolation_json(service$package_vignettes())
  expect_equal(vignettes$vignettes[[1]]$title, "alpha guide")

  news <- root_isolation_json(service$package_news())
  expect_equal(news$source, "NEWS.md")
  expect_match(news$latest$entries[[1]], "alpha NEWS entry", fixed = TRUE)

  r6 <- root_isolation_json(service$r6_classes())
  expect_equal(r6$classes[[1]]$name, "alphaClass")
})

test_that("sibling package roots with matching relative paths stay isolated", {
  parent <- withr::local_tempdir()
  root_a <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  root_b <- local_root_isolation_package(parent, "betaPkg", "beta")
  neutral_cwd <- file.path(parent, "neutral")
  dir.create(neutral_cwd)
  withr::local_dir(neutral_cwd)

  service_a <- rairai:::PackageToolService$new(root_path = root_a)
  service_b <- rairai:::PackageToolService$new(root_path = root_b)

  overview_a <- root_isolation_json(service_a$package_overview())
  overview_b <- root_isolation_json(service_b$package_overview())
  expect_equal(overview_a$description$fields$Package, "alphaPkg")
  expect_equal(overview_b$description$fields$Package, "betaPkg")

  tests_a <- root_isolation_json(service_a$tests_for("shared_target"))
  tests_b <- root_isolation_json(service_b$tests_for("shared_target"))
  expect_equal(tests_a$matches[[1]]$desc, "alpha shared target works")
  expect_equal(tests_b$matches[[1]]$desc, "beta shared target works")

  docs_a <- root_isolation_json(service_a$package_docs())
  docs_b <- root_isolation_json(service_b$package_docs())
  expect_equal(docs_a$help$topics[[1]]$title, "alpha help title")
  expect_equal(docs_b$help$topics[[1]]$title, "beta help title")
})

test_that("package root selectors use defaults and reject unsupported forms", {
  parent <- withr::local_tempdir()
  default_root <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  default_root <- normalizePath(default_root, winslash = "/", mustWork = TRUE)
  selected_root <- local_root_isolation_package(parent, "betaPkg", "beta")
  state <- root_isolation_server_state(default_root)
  empty_arguments <- structure(list(), names = character())

  omitted <- root_isolation_tool_call(
    state,
    "package_overview",
    empty_arguments
  )
  empty <- root_isolation_tool_call(
    state,
    "package_overview",
    list(package_root = "")
  )
  default_alias <- file.path(default_root, "..", basename(default_root))
  canonical_default <- root_isolation_tool_call(
    state,
    "package_overview",
    list(package_root = default_alias)
  )

  expect_false(omitted$is_error)
  expect_false(empty$is_error)
  expect_false(canonical_default$is_error)
  expect_equal(omitted$payload$package_root, default_root)
  expect_equal(empty$payload$package_root, default_root)
  expect_equal(canonical_default$payload$package_root, default_root)

  selector_file <- file.path(parent, "not-a-directory")
  writeLines("not a package root", selector_file)
  invalid_selectors <- c(
    relative = "betaPkg",
    tilde = "~",
    uri = paste0("file://", selected_root),
    missing = file.path(parent, "missing"),
    file = selector_file
  )

  for (selector_name in names(invalid_selectors)) {
    requested <- unname(invalid_selectors[[selector_name]])
    result <- root_isolation_tool_call(
      state,
      "package_overview",
      list(package_root = requested)
    )

    expect_null(result$protocol_error, info = selector_name)
    expect_true(result$is_error, info = selector_name)
    expect_equal(
      result$payload$error$code,
      "invalid_package_root",
      info = selector_name
    )
    expect_equal(
      result$payload$error$requested_package_root,
      requested,
      info = selector_name
    )
    expect_equal(
      result$payload$error$default_package_root,
      default_root,
      info = selector_name
    )
  }
})

test_that("malformed MCP arguments reach the owning tool boundaries", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  state <- root_isolation_server_state(root)

  for (selector in list(42, NA_character_, list("/tmp", "/other"))) {
    result <- root_isolation_tool_call(
      state,
      "symbols",
      list(package_root = selector)
    )
    expect_null(result$protocol_error)
    expect_true(result$is_error)
    expect_equal(result$payload$error$code, "invalid_package_root")
    expect_equal(result$payload$error$default_package_root, root)
    expect_null(result$payload$package_root)
  }
  for (selector in list(NULL, "", list())) {
    result <- root_isolation_tool_call(
      state,
      "symbols",
      list(package_root = selector)
    )
    expect_false(result$is_error)
    expect_equal(result$payload$package_root, root)
  }

  missing <- root_isolation_tool_call(
    state,
    "r6_class",
    structure(list(), names = character())
  )
  expect_null(missing$protocol_error)
  expect_equal(missing$payload$error$code, "unexpected_tool_error")
  expect_match(missing$payload$error$message, 'argument "name" is missing')

  numeric_name <- root_isolation_tool_call(state, "r6_class", list(name = 42))
  expect_equal(numeric_name$payload$error$code, "class_not_found")
  array_name <- root_isolation_tool_call(
    state,
    "r6_class",
    list(name = list("X", "Y"))
  )
  expect_equal(array_name$payload$error$code, "unexpected_tool_error")
  null_name <- root_isolation_tool_call(state, "r6_class", list(name = NULL))
  expect_equal(null_name$payload$error$code, "missing_name")

  query <- root_isolation_tool_call(
    state,
    "symbols",
    list(query = list("X", "Y"), limit = "2")
  )
  expect_false(query$is_error)
  expect_equal(query$payload$query, "X")
  expect_equal(query$payload$limit, 2L)
  numeric_test <- root_isolation_tool_call(state, "tests_for", list(name = 42))
  expect_false(numeric_test$is_error)
  expect_equal(numeric_test$payload$name, 42)

  invalid_params <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 2,
      method = "tools/call",
      params = list(name = "symbols", arguments = 42)
    ),
    state
  )$response
  expect_equal(invalid_params$error$code, -32602L)
  unknown <- mcplite:::dispatch_message(
    list(
      jsonrpc = "2.0",
      id = 2,
      method = "tools/call",
      params = list(name = "symbols", arguments = list(unknown = 1))
    ),
    state
  )$response
  expect_null(unknown$error)
  expect_true(unknown$result$isError)
  expect_match(unknown$result$content[[1L]]$text, "unused argument")
})

test_that("bound source filters cannot adopt a replaced default root", {
  parent <- withr::local_tempdir()
  root <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  outside <- local_root_isolation_package(parent, "betaPkg", "beta")
  outside <- normalizePath(outside, winslash = "/", mustWork = TRUE)
  state <- root_isolation_server_state(root)
  before <- root_isolation_tool_call(
    state,
    "symbols",
    list(query = "shared_target", path = "R/core.R")
  )
  expect_equal(before$payload$symbols[[1L]]$location, "R/core.R:2:1-4:2")

  fs::dir_delete(root)
  absent <- root_isolation_tool_call(state, "symbols", list(path = "R/core.R"))
  expect_false(absent$is_error)
  expect_equal(absent$payload$package_root, root)
  expect_equal(absent$payload$count, 0L)
  create_test_symlink(outside, root)

  filters <- c(
    "R/core.R",
    file.path(root, "R/core.R"),
    rairai:::path_to_uri("R/core.R", root_path = root)
  )
  for (path in filters) {
    for (tool in c("symbols", "definition", "references", "refactor_context")) {
      arguments <- if (tool == "symbols") {
        list(path = path)
      } else {
        list(name = "shared_target", path = path)
      }
      result <- root_isolation_tool_call(state, tool, arguments)
      expect_true(result$is_error, info = paste(tool, path))
      expect_equal(result$payload$package_root, root)
      expect_equal(result$payload$error$code, "path_outside_root")
      expect_equal(result$payload$error$data$root_path, root)
    }
    for (name in c("shared_target", "stats::lm", "not a function")) {
      result <- root_isolation_tool_call(
        state,
        "function_formals",
        list(name = name, context_path = path)
      )
      expect_equal(result$payload$error$code, "path_outside_root")
      expect_equal(result$payload$package_root, root)
      expect_equal(result$payload$error$data$root_path, root)
    }
  }
  selected <- root_isolation_tool_call(
    state,
    "symbols",
    list(path = "R/core.R", package_root = root)
  )
  expect_false(selected$is_error)
  expect_equal(selected$payload$package_root, outside)
  after <- root_isolation_tool_call(state, "symbols", list(path = "R/core.R"))
  expect_equal(after$payload$error$code, "path_outside_root")
  expect_equal(after$payload$package_root, root)
})

test_that("symlink package root selectors report the canonical target", {
  parent <- withr::local_tempdir()
  default_root <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  selected_root <- local_root_isolation_package(parent, "betaPkg", "beta")
  selected_root <- normalizePath(selected_root, winslash = "/", mustWork = TRUE)
  alias <- file.path(parent, "beta-alias")
  create_test_symlink(selected_root, alias)
  state <- root_isolation_server_state(default_root)

  result <- root_isolation_tool_call(
    state,
    "package_overview",
    list(package_root = alias)
  )

  expect_null(result$protocol_error)
  expect_false(result$is_error)
  expect_equal(result$payload$package_root, selected_root)
  expect_equal(result$payload$description$fields$Package, "betaPkg")
})

test_that("served package-local tools route one call to a sibling root", {
  parent <- withr::local_tempdir()
  default_root <- local_root_isolation_package(parent, "alphaPkg", "alpha")
  selected_root <- local_root_isolation_package(parent, "betaPkg", "beta")
  default_root <- normalizePath(default_root, winslash = "/", mustWork = TRUE)
  selected_root <- normalizePath(selected_root, winslash = "/", mustWork = TRUE)
  state <- root_isolation_server_state(default_root)

  overview <- root_isolation_tool_call(
    state,
    "package_overview",
    list(package_root = selected_root)
  )
  symbols <- root_isolation_tool_call(
    state,
    "symbols",
    list(
      query = "shared_target",
      path = "R/core.R",
      package_root = selected_root
    )
  )
  help <- root_isolation_tool_call(
    state,
    "help_search",
    list(query = "beta help title", package_root = selected_root)
  )
  r6 <- root_isolation_tool_call(
    state,
    "r6_classes",
    list(package_root = selected_root)
  )

  routed <- list(overview, symbols, help, r6)
  expect_true(all(map_lgl(routed, \(result) is.null(result$protocol_error))))
  expect_false(any(map_lgl(routed, "is_error")))
  expect_true(all(
    map_chr(routed, c("payload", "package_root")) == selected_root
  ))
  expect_equal(overview$payload$description$fields$Package, "betaPkg")
  expect_equal(symbols$payload$symbols[[1L]]$location, "R/core.R:2:1-4:2")
  expect_equal(help$payload$search_scope, "local_plus_installed")
  local_help <- detect(help$payload$matches, \(match) {
    identical(match$source, "local")
  })
  expect_equal(local_help$package, "betaPkg")
  expect_equal(r6$payload$classes[[1L]]$name, "betaClass")

  outside_path <- file.path(default_root, "R", "core.R")
  contained <- root_isolation_tool_call(
    state,
    "symbols",
    list(path = outside_path, package_root = selected_root)
  )
  expect_null(contained$protocol_error)
  expect_true(contained$is_error)
  expect_equal(contained$payload$package_root, selected_root)
  expect_equal(contained$payload$error$code, "path_outside_root")

  after_override <- root_isolation_tool_call(
    state,
    "package_overview",
    structure(list(), names = character())
  )
  expect_false(after_override$is_error)
  expect_equal(after_override$payload$package_root, default_root)
  expect_equal(after_override$payload$description$fields$Package, "alphaPkg")
})
