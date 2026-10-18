local_refactor_package <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  writeLines(
    c(
      "export(foo)",
      "export(ambiguous)"
    ),
    file.path(root, "NAMESPACE")
  )
  writeLines(
    c(
      "# exported foo context",
      "foo <- function(x) x",
      "bar <- function(x) foo(x)"
    ),
    file.path(root, "R", "foo.R")
  )
  writeLines(
    "baz <- function(x) foo(x)",
    file.path(root, "R", "bar.R")
  )
  writeLines(
    c(
      "# test context for related test discovery",
      "test_that(\"foo works\", {",
      "  foo(1)",
      "})"
    ),
    file.path(root, "tests", "testthat", "test-foo.R")
  )
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

test_that("refactor_context shares Tree-sitter source facts", {
  root <- local_refactor_package()
  service <- rairai:::PackageToolService$new(root_path = root)

  withr::with_dir(root, {
    result <- tool_result_json(
      service$refactor_context(name = "foo", kind = "Function", limit = 100),
      simplifyVector = FALSE
    )
  })

  expect_equal(result$status, "unique")
  expect_equal(result$resolved$location, "R/foo.R:2:1-2:21")
  expect_equal(result$resolved$leading_comment_lines, "1")
  expect_false("definition_count" %in% names(result))
  expect_false("definitions" %in% names(result))
  expect_equal(result$reference_count, 3L)
  expect_equal(result$reference_total_count, 3L)
  expect_false(result$references_truncated)
  expect_equal(result$reference_limit, 100L)
  expect_null(result$include_declaration)
  expect_equal(result$reference_semantics, "syntactic_impact_references")
  expect_equal(result$binding_identity, "not_proven")
  expect_equal(result$declaration, "resolved_only")
  expect_true(result$reference_scope$includes_tests)
  expect_setequal(
    map_chr(result$references, "location"),
    c(
      "R/foo.R:3:20-3:26",
      "R/bar.R:1:20-1:26",
      "tests/testthat/test-foo.R:3:3-3:9"
    )
  )
  expect_true(result$exported)
  expect_equal(result$namespace$exported, TRUE)
  expect_equal(result$related_test_count, 1L)
  expect_equal(
    result$related_tests[[1L]]$location,
    "tests/testthat/test-foo.R:2-4"
  )
  expect_equal(result$related_tests[[1L]]$leading_comment_lines, "1")
  expect_null(result$diagnostic_count)
  expect_null(result$diagnostics_path)
  expect_null(result$diagnostics)
  expect_contains(map_chr(result$next_tools, "tool"), "definition")
  expect_contains(map_chr(result$next_tools, "tool"), "references")
  expect_contains(map_chr(result$next_tools, "tool"), "tests_for")
  expect_false("diagnostics" %in% map_chr(result$next_tools, "tool"))
})

test_that("refactor_context reports reference truncation metadata", {
  root <- local_refactor_package()
  service <- rairai:::PackageToolService$new(root_path = root)

  withr::with_dir(root, {
    result <- tool_result_json(
      service$refactor_context(name = "foo", kind = "Function", limit = 2),
      simplifyVector = FALSE
    )
  })

  expect_equal(result$status, "unique")
  expect_equal(result$reference_count, 2L)
  expect_equal(result$reference_total_count, 3L)
  expect_true(result$references_truncated)
  expect_equal(result$reference_limit, 2L)
  expect_length(result$references, 2L)
})

test_that("refactor_context includes R6 member match caveats", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c(
      paste0(
        "Gen <- R6::R6Class(\"Gen\", public = list(",
        "run = function(x) x, value = 1))"
      ),
      "member_use <- function(obj) { obj$run(); obj$value }"
    ),
    file.path(root, "R", "gen.R")
  )
  service <- rairai:::PackageToolService$new(root_path = root)

  method_result <- tool_result_json(
    service$refactor_context(name = "run", kind = "Method", limit = 100),
    simplifyVector = FALSE
  )
  field_result <- tool_result_json(
    service$refactor_context(name = "value", kind = "Field", limit = 100),
    simplifyVector = FALSE
  )
  expected_match <- list(
    matched_by = "member_name_only",
    receiver_object_proven = FALSE,
    class_membership_proven = FALSE,
    may_include_unrelated_objects = TRUE
  )

  expect_equal(method_result$status, "unique")
  expect_equal(method_result$r6_member_reference_match, expected_match)
  expect_equal(field_result$status, "unique")
  expect_equal(field_result$r6_member_reference_match, expected_match)
})

test_that("refactor_context returns candidates when symbol is ambiguous", {
  root <- normalizePath(withr::local_tempdir(), winslash = "/", mustWork = TRUE)
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("ambiguous <- function() NULL", file.path(root, "R", "a.R"))
  writeLines("ambiguous <- function() NULL", file.path(root, "R", "b.R"))
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$refactor_context(name = "ambiguous", kind = "Function"),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "ambiguous")
  expect_equal(result$count, 2L)
  expect_setequal(
    map_chr(result$candidates, "location"),
    c("R/a.R:1:1-1:29", "R/b.R:1:1-1:29")
  )
  expect_equal(result$next_tools[[1]]$tool, "symbols")
})

test_that("refactor_context returns routing hint when no symbol matches", {
  root <- local_refactor_package()
  service <- rairai:::PackageToolService$new(root_path = root)

  result <- tool_result_json(
    service$refactor_context(name = "missing", kind = "Function"),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "none")
  expect_equal(result$count, 0L)
  expect_equal(result$next_tools[[1]]$tool, "symbols")
})
