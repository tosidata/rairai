local_source_index_package <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "tests", "testthat"), recursive = TRUE)
  writeLines(
    c(
      "# frequency validator context",
      "validate_serialized_frequency <- function(x) x",
      "same_file <- function(x) validate_serialized_frequency(x)",
      "SERIALIZED <- 1",
      paste0(
        "Gen <- R6::R6Class(\"Gen\", public = list(",
        "run = function(x) validate_serialized_frequency(x), ",
        "value = SERIALIZED))"
      ),
      "make_gen <- function() Gen$new()",
      "string_hit <- \"validate_serialized_frequency()\"",
      "# validate_serialized_frequency() in a comment",
      "local_target <- function() { validate_serialized_frequency <- 1 }",
      "parameter_target <- function(validate_serialized_frequency) NULL"
    ),
    file.path(root, "R", "frequency.R")
  )
  writeLines(
    c(
      "other_file <- function(x) validate_serialized_frequency(x)",
      "object_use <- function() SERIALIZED + pkg::SERIALIZED",
      "member_use <- function(obj) { obj$run(); obj$value }"
    ),
    file.path(root, "R", "other.R")
  )
  writeLines(
    "test_that(\"frequency\", { pkg:::validate_serialized_frequency(1) })",
    file.path(root, "tests", "testthat", "test-frequency.R")
  )
  normalizePath(root, winslash = "/", mustWork = TRUE)
}

source_index_service <- function(root) {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  rairai:::PackageToolService$new(root_path = root)
}

test_that("Tree-sitter symbols cover package source without a background service", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  result <- tool_result_json(
    service$symbols(path = "R/frequency.R", exact = TRUE, limit = 100),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "file_outline")
  locations <- map_chr(result$symbols, "location")
  expect_true(all(startsWith(locations, "R/frequency.R:")))
  expect_false(any(startsWith(locations, root)))

  names <- map_chr(result$symbols, "name")
  expect_contains(names, "R/frequency.R")
  expect_contains(names, "validate_serialized_frequency")
  expect_contains(names, "SERIALIZED")
  expect_contains(names, "Gen")
  expect_contains(names, "run")
  expect_contains(names, "value")

  kinds <- setNames(map_chr(result$symbols, "kind_name"), names)
  expect_equal(kinds[["R/frequency.R"]], "File")
  expect_equal(kinds[["validate_serialized_frequency"]], "Function")
  expect_equal(kinds[["SERIALIZED"]], "Object")
  expect_equal(kinds[["Gen"]], "Class")
  expect_equal(kinds[["run"]], "Method")
  expect_equal(kinds[["value"]], "Field")

  validator <- result$symbols[[match("validate_serialized_frequency", names)]]
  expect_equal(validator$location, "R/frequency.R:2:1-2:47")
  expect_equal(validator$leading_comment_lines, "1")
})

test_that("SourceIndex R6 facts use flat R source scope with .R and .r", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R", "nested"), recursive = TRUE)
  writeLines('Upper <- R6::R6Class("Upper")', file.path(root, "R", "upper.R"))
  writeLines('Lower <- R6::R6Class("Lower")', file.path(root, "R", "lower.r"))
  writeLines(
    'Nested <- R6::R6Class("Nested")',
    file.path(root, "R", "nested", "nested.R")
  )
  service <- source_index_service(root)

  result <- tool_result_json(
    service$symbols(kind = "Class", exact = TRUE, limit = 100),
    simplifyVector = FALSE
  )

  expect_setequal(map_chr(result$symbols, "name"), c("Upper", "Lower"))
})

test_that("SourceIndex ignores namespace-qualified R6Class lookalikes", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c(
      'Fake <- fakepkg::R6Class("Fake")',
      'Triple <- R6:::R6Class("Triple")',
      'Qualified <- R6::R6Class("Qualified")',
      'Bare <- R6Class("Bare")'
    ),
    file.path(root, "R", "classes.R")
  )
  service <- source_index_service(root)

  class_result <- tool_result_json(
    service$symbols(kind = "Class", exact = TRUE, limit = 100),
    simplifyVector = FALSE
  )
  object_result <- tool_result_json(
    service$symbols(query = "Fake", kind = "Object", exact = TRUE, limit = 10),
    simplifyVector = FALSE
  )

  expect_setequal(map_chr(class_result$symbols, "name"), c("Qualified", "Bare"))
  expect_equal(object_result$status, "unique")
})

test_that("shared syntax preserves assignment scope, roles and cross-tool facts", {
  root <- withr::local_tempdir()
  fs::dir_create(fs::path(root, "R", "nested"))
  fs::dir_create(fs::path(root, "tests", "testthat", "nested"))
  writeLines(
    c(
      '# target context',
      'target <- function(x, y = 1, ...) x',
      'equal_fn = function() target()',
      'super_fn <<- function() target()',
      '`odd name` <- function(x) target(x)',
      'Gen <<- R6::R6Class("Gen", public = list(',
      '  `run-now` = function(x = 1) target(x), omitted =',
      '), private = list(hidden = function() target()),',
      'active = list(value = function() 1))',
      'use <- function(obj, df) {',
      '  df[1, , drop = FALSE]',
      '  target(x = 1)',
      '  pkg::target(); alias <- pkg::target',
      '  obj$`run-now`(); obj$`run-now`',
      '  obj$value; obj@value',
      '  "target()"; # target()',
      '  alias <- target',
      '}'
    ),
    fs::path(root, "R", "syntax.R")
  )
  writeLines('lower <- function() target()', fs::path(root, "R", "z.r"))
  writeLines(
    'nested <- function() target()',
    fs::path(root, "R", "nested", "ignored.R")
  )
  writeLines('broken <- function(', fs::path(root, "R", "broken.R"))
  writeLines(
    'test_that("target", { target() })',
    fs::path(root, "tests", "testthat", "test-syntax.R")
  )
  writeLines(
    'target()',
    fs::path(root, "tests", "testthat", "nested", "ignored.R")
  )
  service <- source_index_service(root)

  symbols <- tool_result_json(
    service$symbols(limit = 100),
    simplifyVector = FALSE
  )
  definition <- tool_result_json(
    service$definition("target"),
    simplifyVector = FALSE
  )
  references <- tool_result_json(
    service$references("target"),
    simplifyVector = FALSE
  )
  formals <- tool_result_json(
    service$function_formals("target"),
    simplifyVector = FALSE
  )
  context <- tool_result_json(
    service$refactor_context("target"),
    simplifyVector = FALSE
  )
  method <- tool_result_json(
    service$references("run-now", kind = "Method"),
    simplifyVector = FALSE
  )
  active <- tool_result_json(
    service$references("value", kind = "Method"),
    simplifyVector = FALSE
  )
  class <- tool_result_json(service$r6_class("Gen"), simplifyVector = FALSE)

  # Superassignment is not a declaration in SourceIndex; its R6 members remain facts.
  expect_equal(
    map_chr(symbols$symbols, "name"),
    c(
      "R/broken.R",
      "R/syntax.R",
      "target",
      "equal_fn",
      "`odd name`",
      "use",
      "run-now",
      "omitted",
      "hidden",
      "value",
      "R/z.r",
      "lower"
    )
  )
  expect_equal(map_chr(symbols$parse_errors, "path"), "R/broken.R")
  target <- detect(symbols$symbols, \(symbol) identical(symbol$name, "target"))
  expect_equal(definition$resolved, target)
  expect_equal(references$resolved, target)
  expect_equal(context$resolved, target)
  expect_equal(formals$resolved, target)
  expect_equal(target$location, "R/syntax.R:2:1-2:36")
  expect_equal(target$leading_comment_lines, "1")
  expect_equal(formals$signatures[[1]]$label, "target(x, y = 1, ...)")
  expect_equal(context$references, references$references)
  expect_equal(
    map_chr(references$references, "role"),
    c(
      rep("direct_call", 6),
      "namespace_call",
      "identifier",
      rep("direct_call", 2)
    )
  )
  expect_equal(
    map_chr(references$references, "location"),
    c(
      "R/syntax.R:3:23-3:31",
      "R/syntax.R:4:25-4:33",
      "R/syntax.R:5:27-5:36",
      "R/syntax.R:7:31-7:40",
      "R/syntax.R:8:39-8:47",
      "R/syntax.R:12:3-12:16",
      "R/syntax.R:13:8-13:14",
      "R/syntax.R:17:12-17:18",
      "R/z.r:1:21-1:29",
      "tests/testthat/test-syntax.R:1:23-1:31"
    )
  )
  expect_equal(
    map_chr(method$references, "role"),
    c("member_call", "member_access")
  )
  expect_equal(
    map_chr(method$references, "location"),
    c("R/syntax.R:14:7-14:17", "R/syntax.R:14:24-14:32")
  )
  expect_equal(map_chr(active$references, "role"), "member_access")
  expect_equal(map_chr(class$public, "kind"), c("method", "field"))
  expect_equal(class$public[[2]]$default, "NULL")
  expect_equal(class$private[[1]]$name, "hidden")
  expect_equal(class$active[[1]]$kind, "active")
})

test_that("definition resolves the same Tree-sitter symbol records", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  symbol <- tool_result_json(
    service$symbols(
      query = "validate_serialized_frequency",
      kind = "Function",
      path = "R",
      limit = 10
    ),
    simplifyVector = FALSE
  )
  definition <- tool_result_json(
    service$definition(
      name = "validate_serialized_frequency",
      kind = "Function",
      path = "R"
    ),
    simplifyVector = FALSE
  )

  expect_equal(symbol$status, "unique")
  expect_equal(definition$status, "unique")
  expect_equal(definition$resolved$name, symbol$symbols[[1]]$name)
  expect_equal(definition$resolved$kind_name, symbol$symbols[[1]]$kind_name)
  expect_false("locations" %in% names(definition))
  expect_equal(definition$resolved$location, symbol$symbols[[1]]$location)
  expect_equal(definition$resolved$signature, symbol$symbols[[1]]$signature)
  expect_equal(definition$resolved$leading_comment_lines, "1")
  expect_equal(definition$count, 1L)
  expect_equal(definition$total_count, 1L)
  expect_false(definition$truncated)
  expect_equal(definition$limit, 20L)
})

test_that("references find package calls syntactically from source", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  result <- tool_result_json(
    service$references(
      name = "validate_serialized_frequency",
      kind = "Function",
      path = "R/frequency.R",
      limit = 100
    ),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "unique")
  expect_equal(result$reference_semantics, "syntactic_impact_references")
  expect_equal(result$binding_identity, "not_proven")
  expect_equal(result$declaration, "resolved_only")
  expect_null(result$include_declaration)
  expect_true(result$reference_scope$includes_tests)
  expect_equal(result$resolved$location, "R/frequency.R:2:1-2:47")
  expect_equal(result$resolved$leading_comment_lines, "1")

  reference_locations <- map_chr(result$references, "location")
  expect_setequal(
    reference_locations,
    c(
      "R/frequency.R:3:26-3:58",
      "R/frequency.R:5:59-5:91",
      "R/other.R:1:27-1:59",
      "tests/testthat/test-frequency.R:1:32-1:61"
    )
  )
  expect_false(any(reference_locations == result$resolved$location))
  expect_false(any(str_detect(reference_locations, "7:")))
  expect_false(any(str_detect(reference_locations, "8:")))
  expect_false(any(str_detect(reference_locations, "9:")))
  expect_false(any(str_detect(reference_locations, "10:")))

  roles <- setNames(map_chr(result$references, "role"), reference_locations)
  expect_equal(
    roles[["tests/testthat/test-frequency.R:1:32-1:61"]],
    "namespace_call"
  )
  expect_equal(roles[["R/frequency.R:3:26-3:58"]], "direct_call")
})

test_that("references surface parse errors that can affect completeness", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c("target <- function() NULL", "caller <- function() target()"),
    file.path(root, "R", "target.R")
  )
  writeLines(
    c("broken <- function() {", "target("),
    file.path(root, "R", "broken.R")
  )
  service <- source_index_service(root)

  result <- tool_result_json(
    service$references(name = "target", kind = "Function", limit = 100),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "unique")
  expect_equal(result$resolved$location, "R/target.R:1:1-1:26")
  expect_length(result$parse_errors, 1)
  expect_equal(result$parse_errors[[1]]$path, "R/broken.R")
  expect_match(result$parse_errors[[1]]$message, "parse errors")
  expect_contains(
    map_chr(result$references, "location"),
    "R/target.R:2:22-2:30"
  )
})

test_that("references support objects, R6 classes, and R6 members", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  object_refs <- tool_result_json(
    service$references(name = "SERIALIZED", kind = "Object", limit = 100),
    simplifyVector = FALSE
  )
  expect_equal(object_refs$status, "unique")
  expect_setequal(
    map_chr(object_refs$references, "role"),
    c("identifier", "identifier", "namespace_access")
  )
  expect_null(object_refs$r6_member_reference_match)

  class_refs <- tool_result_json(
    service$references(name = "Gen", kind = "Class", limit = 100),
    simplifyVector = FALSE
  )
  expect_equal(class_refs$status, "unique")
  expect_equal(
    map_chr(class_refs$references, "location"),
    "R/frequency.R:6:24-6:27"
  )
  expect_null(class_refs$r6_member_reference_match)

  method_refs <- tool_result_json(
    service$references(name = "run", kind = "Method", limit = 100),
    simplifyVector = FALSE
  )
  field_refs <- tool_result_json(
    service$references(name = "value", kind = "Field", limit = 100),
    simplifyVector = FALSE
  )
  expected_match <- list(
    matched_by = "member_name_only",
    receiver_object_proven = FALSE,
    class_membership_proven = FALSE,
    may_include_unrelated_objects = TRUE
  )
  expect_equal(map_chr(method_refs$references, "role"), "member_call")
  expect_equal(method_refs$r6_member_reference_match, expected_match)
  expect_equal(map_chr(field_refs$references, "role"), "member_access")
  expect_equal(field_refs$r6_member_reference_match, expected_match)
})

test_that("symbols report truncation metadata", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  result <- tool_result_json(
    service$symbols(path = "R/frequency.R", exact = TRUE, limit = 2),
    simplifyVector = FALSE
  )

  expect_equal(result$count, 2L)
  expect_gt(result$total_count, result$count)
  expect_true(result$truncated)
  expect_equal(result$limit, 2L)
  expect_length(result$symbols, 2L)
})

test_that("file outline symbols are not referencable", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  result <- tool_result_json(
    service$references(name = "R/frequency.R", kind = "File"),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "not_referencable")
  expect_equal(result$count, 0)
  expect_equal(result$total_count, 0)
  expect_false(result$truncated)
  expect_length(result$references, 0)
  expect_equal(result$resolved$kind_name, "File")
})

test_that("duplicate definitions are ambiguous until narrowed by path", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines(
    c("target <- function() NULL", "call_a <- function() target()"),
    file.path(root, "R", "a.R")
  )
  writeLines(
    c("target <- function() NULL", "call_b <- function() target()"),
    file.path(root, "R", "b.R")
  )
  service <- source_index_service(root)

  ambiguous <- tool_result_json(
    service$references(name = "target", kind = "Function", limit = 100),
    simplifyVector = FALSE
  )
  expect_equal(ambiguous$status, "ambiguous")
  expect_equal(ambiguous$count, 2)
  expect_null(ambiguous$references)

  narrowed <- tool_result_json(
    service$references(
      name = "target",
      kind = "Function",
      path = file.path(root, "R", "a.R"),
      limit = 100
    ),
    simplifyVector = FALSE
  )
  expect_equal(narrowed$status, "unique")
  expect_equal(narrowed$filters$path, "R/a.R")
  expect_setequal(
    map_chr(narrowed$references, "location"),
    c("R/a.R:2:22-2:30", "R/b.R:2:22-2:30")
  )
})

test_that("file URI filters normalize and refactor context shares facts", {
  root <- local_source_index_package()
  service <- source_index_service(root)
  uri <- paste0(
    "file://",
    utils::URLencode(
      file.path(root, "R", "frequency.R"),
      reserved = TRUE
    )
  ) |>
    str_replace_all(fixed("%2F"), "/")

  definition <- tool_result_json(
    service$definition(
      name = "validate_serialized_frequency",
      kind = "Function",
      path = uri
    ),
    simplifyVector = FALSE
  )
  context <- tool_result_json(
    service$refactor_context(
      name = "validate_serialized_frequency",
      kind = "Function",
      path = uri,
      limit = 100
    ),
    simplifyVector = FALSE
  )

  expect_equal(definition$filters$path, "R/frequency.R")
  expect_equal(context$status, "unique")
  expect_equal(context$resolved$location, definition$resolved$location)
  expect_equal(context$reference_semantics, "syntactic_impact_references")
  expect_setequal(
    map_chr(context$references, "location"),
    map_chr(
      tool_result_json(
        service$references(
          name = "validate_serialized_frequency",
          kind = "Function",
          path = uri,
          limit = 100
        ),
        simplifyVector = FALSE
      )$references,
      "location"
    )
  )
})

test_that("function_formals uses Tree-sitter for cross-file locals", {
  root <- local_source_index_package()
  service <- source_index_service(root)

  result <- tool_result_json(
    service$function_formals(
      name = "validate_serialized_frequency",
      context_path = "R/other.R"
    ),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "unique")
  expect_equal(result$source, "tree_sitter_source_index")
  expect_equal(result$resolved$source, "tree_sitter_source_index")
  expect_false("source.1" %in% names(result$resolved))
  expect_equal(result$resolved$location, "R/frequency.R:2:1-2:47")
  expect_equal(result$resolved$leading_comment_lines, "1")
  expect_equal(result$signatures[[1]]$label, "validate_serialized_frequency(x)")
  expect_equal(result$signatures[[1]]$parameters[[1]]$name, "x")
})

test_that("function_formals leaves duplicate local definitions ambiguous", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("target <- function(a) a", file.path(root, "R", "a.R"))
  writeLines("target <- function(b) b", file.path(root, "R", "b.R"))
  service <- source_index_service(root)

  result <- tool_result_json(
    service$function_formals(name = "target", context_path = "R/a.R"),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "ambiguous")
  expect_equal(result$count, 2L)
  expect_setequal(
    map_chr(result$candidates, "location"),
    c("R/a.R:1:1-1:24", "R/b.R:1:1-1:24")
  )
  expect_null(result$signatures)
})

test_that("same service reflects modified, added, and deleted source files", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "R"), recursive = TRUE)
  writeLines("target <- function() 1", file.path(root, "R", "target.R"))
  service <- source_index_service(root)

  initial <- tool_result_json(
    service$symbols(kind = "Function", limit = 100),
    simplifyVector = FALSE
  )
  expect_equal(initial$symbols[[1]]$location, "R/target.R:1:1-1:23")

  Sys.sleep(0.02)
  writeLines(
    c("# changed", "target <- function(x, y) x + y"),
    file.path(root, "R", "target.R")
  )
  modified <- tool_result_json(
    service$function_formals(name = "target"),
    simplifyVector = FALSE
  )
  expect_equal(modified$signatures[[1]]$label, "target(x, y)")
  expect_equal(modified$resolved$location, "R/target.R:2:1-2:31")

  writeLines(
    "added <- function() target(1, 2)",
    file.path(root, "R", "added.R")
  )
  added <- tool_result_json(
    service$symbols(kind = "Function", limit = 100),
    simplifyVector = FALSE
  )
  expect_contains(map_chr(added$symbols, "name"), "added")

  file.remove(file.path(root, "R", "target.R"))
  deleted <- tool_result_json(
    service$symbols(kind = "Function", limit = 100),
    simplifyVector = FALSE
  )
  expect_setequal(map_chr(deleted$symbols, "name"), "added")
})

test_that("source-index file fact cache is root-isolated", {
  rairai:::reset_internal_cache()
  withr::defer(rairai:::reset_internal_cache())

  root_a <- withr::local_tempdir()
  root_b <- withr::local_tempdir()
  dir.create(file.path(root_a, "R"), recursive = TRUE)
  dir.create(file.path(root_b, "R"), recursive = TRUE)
  file_a <- file.path(root_a, "R", "shared.R")
  file_b <- file.path(root_b, "R", "shared.R")
  writeLines("alpha <- function() 1", file_a)
  writeLines("bravo <- function() 2", file_b)
  shared_time <- Sys.time() - 10
  Sys.setFileTime(file_a, shared_time)
  Sys.setFileTime(file_b, shared_time)

  symbols_a <- tool_result_json(
    source_index_service(root_a)$symbols(kind = "Function", limit = 100),
    simplifyVector = FALSE
  )
  symbols_b <- tool_result_json(
    source_index_service(root_b)$symbols(kind = "Function", limit = 100),
    simplifyVector = FALSE
  )

  expect_equal(map_chr(symbols_a$symbols, "name"), "alpha")
  expect_equal(map_chr(symbols_b$symbols, "name"), "bravo")
})

test_that("unchanged source files are parsed once across resolve and refs", {
  rairai:::reset_internal_cache()
  withr::defer(rairai:::reset_internal_cache())
  root <- local_source_index_package()
  original_parse <- rairai:::r6_syntax_parse_file
  parsed_paths <- character()
  testthat::local_mocked_bindings(
    r6_syntax_parse_file = function(root_path, path) {
      parsed_paths <<- c(parsed_paths, path)
      original_parse(root_path, path)
    },
    .package = "rairai"
  )
  service <- source_index_service(root)

  result <- tool_result_json(
    service$references(
      name = "validate_serialized_frequency",
      kind = "Function",
      path = "R/frequency.R",
      limit = 100
    ),
    simplifyVector = FALSE
  )

  expect_equal(result$status, "unique")
  expect_count <- table(parsed_paths)
  expect_equal(as.integer(expect_count[["R/frequency.R"]]), 1L)
  expect_equal(as.integer(expect_count[["R/other.R"]]), 1L)
  expect_equal(
    as.integer(expect_count[["tests/testthat/test-frequency.R"]]),
    1L
  )
})

test_that("cached source-index file facts do not retain Tree-sitter nodes", {
  root <- local_source_index_package()
  index <- rairai:::SourceIndex$new(root_path = root)$build(
    include_tests = TRUE
  )

  expect_false(any(map_lgl(index$references, \(reference) {
    "node" %in% names(reference)
  })))
})
