test_that("function_formals reports external package signatures as unsupported", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$function_formals(name = "lm", package = "stats"),
    simplifyVector = FALSE
  )

  expect_equal(result$query, "stats::lm")
  expect_equal(result$status, "unsupported")
  expect_equal(result$source, "external_formals_unavailable")
  expect_equal(result$resolved$source, "package_argument")
  expect_equal(result$resolved$qualified_name, "stats::lm")
  expect_equal(result$package, "stats")
  expect_equal(result$count, 0L)
  expect_length(result$signatures, 0L)
  expect_match(result$limitation, "does not execute or load", fixed = TRUE)
  expect_setequal(
    map_chr(result$next_tools, "tool"),
    c("help_page", "help_search")
  )
})

test_that("function_formals reports qualified external signatures as unsupported", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$function_formals(name = "stats::lm"),
    simplifyVector = FALSE
  )

  expect_equal(result$query, "stats::lm")
  expect_equal(result$status, "unsupported")
  expect_equal(result$source, "external_formals_unavailable")
  expect_equal(result$resolved$source, "qualified_name")
  expect_equal(result$resolved$name, "lm")
  expect_equal(result$package, "stats")
  expect_length(result$signatures, 0L)
  expect_contains(map_chr(result$next_tools, "tool"), "help_page")
})

test_that("function_formals isolates package requests from local fallback", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c(
      "Package: localpkg",
      "Title: Local Package",
      "Version: 0.0.0",
      "Description: Local package fixture.",
      "License: MIT"
    ),
    file.path(root, "DESCRIPTION")
  )
  writeLines(
    c(
      "lm <- function(local_formula) local_formula",
      "local_helper <- function(local_arg) local_arg"
    ),
    file.path(root, "R", "helpers.R")
  )
  service <- rairai:::PackageToolService$new(root_path = root)

  stats_result <- tool_result_json(
    service$function_formals(name = "lm", package = "stats"),
    simplifyVector = FALSE
  )
  active_package_result <- tool_result_json(
    service$function_formals(name = "local_helper", package = "localpkg"),
    simplifyVector = FALSE
  )
  local_result <- tool_result_json(
    service$function_formals(name = "local_helper"),
    simplifyVector = FALSE
  )

  expect_equal(stats_result$status, "unsupported")
  expect_equal(stats_result$resolved$qualified_name, "stats::lm")
  expect_equal(active_package_result$status, "unsupported")
  expect_equal(
    active_package_result$resolved$qualified_name,
    "localpkg::local_helper"
  )
  expect_equal(local_result$status, "unique")
  expect_equal(local_result$source, "tree_sitter_source_index")
  expect_equal(local_result$signatures[[1]]$label, "local_helper(local_arg)")
})

test_that("PackageToolService function_formals validates references", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$function_formals(name = "not a function"),
    simplifyVector = FALSE
  )

  expect_equal(result$error$code, "invalid_function_reference")
})

test_that("PackageToolService function_formals rejects directory context paths", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  dir.create(file.path(root, "R"))
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$function_formals(name = "helper", context_path = "R"),
    simplifyVector = FALSE
  )

  expect_equal(result$error$code, "path_not_file")
  expect_equal(result$error$data$path, "R")
})

test_that("context errors precede local, external and invalid references", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  dir.create(file.path(root, "R"))
  writeLines("helper <- function(x) x", file.path(root, "R", "helper.R"))
  service <- rairai:::PackageToolService$new(root_path = root)
  references <- list(
    list(name = "helper"),
    list(name = "stats::lm"),
    list(name = "lm", package = "stats"),
    list(name = "not a function"),
    list(name = "stats::lm", package = "stats")
  )
  for (reference in references) {
    for (context in c("R", tempfile(tmpdir = dirname(root)))) {
      result <- tool_result_json(
        do.call(
          service$function_formals,
          c(reference, list(context_path = context))
        ),
        simplifyVector = FALSE
      )
      expect_equal(
        result$error$code,
        if (context == "R") "path_not_file" else "path_outside_root"
      )
      expect_equal(result$error$data$argument, "context_path")
    }
  }
  for (context in list(NULL, "", "R/missing.R")) {
    result <- tool_result_json(
      service$function_formals("helper", context_path = context),
      simplifyVector = FALSE
    )
    expect_equal(result$status, "unique")
    expect_equal(
      result$context_path,
      if (identical(context, "R/missing.R")) context
    )
  }
})

test_that("PackageToolService symbols rejects paths outside the active root", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$symbols(path = tempfile(tmpdir = dirname(root))),
    simplifyVector = FALSE
  )

  expect_equal(result$error$code, "path_outside_root")
})
