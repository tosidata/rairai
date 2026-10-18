run_from_package_root <- function(expr) {
  pkg <- Sys.getenv("TESTTHAT_PKG", unset = "rairai")
  withr::with_dir(find.package(pkg), force(expr))
}

read_namespace_json <- function(package = NULL) {
  tool_result_json(
    rairai:::read_package_namespace(package = package),
    simplifyVector = FALSE
  )
}

test_that("read_package_namespace returns merged package-keyed imports", {
  result <- run_from_package_root(read_namespace_json())

  expect_contains(result$exports, "serve")
  expect_contains(names(result$imports), "purrr")
  expect_contains(names(result$imports), "stringr")
  expect_contains(result$imports$purrr, "accumulate")
  expect_contains(result$imports$jsonlite, "fromJSON")
  expect_contains(result$imports$jsonlite, "toJSON")
  expect_null(result$import_from)
})

test_that("read_package_namespace parses multiline directives and comments", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "export(my_fun) # trailing comment",
      "importFrom(stats,",
      "  median) # another comment"
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_contains(result$exports, "my_fun")
  expect_equal(result$imports$stats[[1]], "median")
})

test_that("read_package_namespace discovers conditional directives", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "if (TRUE) importFrom(methods, setRefClass)",
      "if (FALSE) {",
      "  export(conditionally_exported)",
      "  importFrom(stats, median)",
      "} else {",
      "  importFrom(utils, head)",
      "}"
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_contains(result$exports, "conditionally_exported")
  expect_equal(result$imports$methods[[1]], "setRefClass")
  expect_equal(result$imports$stats[[1]], "median")
  expect_equal(result$imports$utils[[1]], "head")
})

test_that("read_package_namespace keeps quoted and backticked names", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "export(\"quoted name\")",
      "export(`%=%`)",
      "importFrom(methods, `%=%`)",
      "importFrom(stats, \"setNames\")"
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json(package = "methods"))
  summary <- withr::with_dir(tmp, read_namespace_json())

  expect_contains(summary$exports, "quoted name")
  expect_contains(summary$exports, "%=%")
  expect_equal(result$imports$methods[[1]], "%=%")
  expect_equal(summary$imports$stats[[1]], "setNames")
})

test_that("read_package_namespace expands import except before returning objects", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "import(stats, except = c(lm, median))",
      "importFrom(stats, median)"
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json(package = "stats"))

  expect_true("stats" %in% names(result$imports))
  expect_false("lm" %in% result$imports$stats)
  expect_true("median" %in% result$imports$stats)
})

test_that("read_package_namespace matches import except exactly", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    "import(stats, exception = c(lm, median))",
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json(package = "stats"))

  expect_true("lm" %in% result$imports$stats)
  expect_true("median" %in% result$imports$stats)
})

test_that("read_package_namespace truncates default import lists", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines("import(stats)", file.path(tmp, "NAMESPACE"))

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_length(result$imports$stats, 20L)
  expect_equal(result$truncated$stats$shown, 20L)
  expect_gt(result$truncated$stats$total, 20L)
})

test_that("read_package_namespace uses import limit option", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines("import(stats)", file.path(tmp, "NAMESPACE"))

  withr::local_options(rairai.package_namespace.import_limit = 5L)
  result <- withr::with_dir(tmp, read_namespace_json())

  expect_length(result$imports$stats, 5L)
  expect_equal(result$truncated$stats$shown, 5L)
})

test_that("read_package_namespace focuses on one imported package", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c("export(my_fun)", "import(stats)", "S3method(print, my_class)"),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json(package = "stats"))

  expect_equal(result$package, "stats")
  expect_true("lm" %in% result$imports$stats)
  expect_null(result$exports)
  expect_null(result$s3_methods)
  expect_null(result$truncated)
})

test_that("read_package_namespace reports focused package that is not imported", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines("import(stats)", file.path(tmp, "NAMESPACE"))

  result <- withr::with_dir(tmp, read_namespace_json(package = "utils"))

  expect_equal(result$package, "utils")
  expect_equal(result$error$code, "package_not_imported")
})

test_that("read_package_namespace reports malformed namespace syntax", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines("export(", file.path(tmp, "NAMESPACE"))

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_equal(result$error$code, "namespace_parse_error")
  expect_null(result$exports)
  expect_null(result$imports)
})

test_that("read_package_namespace keeps exports and imports as arrays", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c("export(only_fn)", "importFrom(stats, setNames)"),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_true(is.list(result$exports))
  expect_equal(result$exports[[1]], "only_fn")
  expect_true(is.list(result$imports$stats))
  expect_equal(result$imports$stats[[1]], "setNames")
})

test_that("read_package_namespace returns stable empty summary sections", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines("export(my_fn)", file.path(tmp, "NAMESPACE"))

  raw <- withr::with_dir(tmp, rairai:::read_package_namespace())
  text <- tool_result_text(raw)

  expect_match(text, '"imports"\\s*:\\s*\\{\\s*\\}')
  expect_match(text, '"s3_methods"\\s*:\\s*\\[\\s*\\]')
  expect_match(text, '"use_dyn_lib"\\s*:\\s*\\[\\s*\\]')
})

test_that("read_package_namespace preserves S3method and useDynLib directives", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "S3method(print, my_class)",
      "S3method(format, my_class, format_my_class)",
      paste0(
        "useDynLib(mypkg, routine, ",
        "public_routine = C_private_routine, ",
        ".registration = TRUE, .fixes = C_, .useDynLib = FALSE)"
      )
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_equal(result$s3_methods[[1]]$generic, "print")
  expect_equal(result$s3_methods[[1]]$class, "my_class")
  expect_equal(result$s3_methods[[2]]$method, "format_my_class")
  expect_equal(result$use_dyn_lib[[1]]$library, "mypkg")
  expect_equal(
    result$use_dyn_lib[[1]]$symbols,
    list("public_routine", "routine")
  )
  expect_equal(
    result$use_dyn_lib[[1]]$native_symbols$public_routine$name,
    "public_routine"
  )
  expect_equal(
    result$use_dyn_lib[[1]]$native_symbols$public_routine$routine,
    "C_private_routine"
  )
  expect_equal(result$use_dyn_lib[[1]]$registration, TRUE)
  expect_equal(result$use_dyn_lib[[1]]$fixes, "C_")
  expect_equal(result$use_dyn_lib[[1]]$useDynLib, FALSE)
})

test_that("read_package_namespace reports unavailable imported packages", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c("import(notapkgdefinitelymissing)", "importFrom(stats, setNames)"),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_equal(
    result$errors$notapkgdefinitelymissing$code,
    "package_not_available"
  )
  expect_equal(result$imports$notapkgdefinitelymissing, list())
  expect_equal(result$imports$stats[[1]], "setNames")
})

test_that("read_package_namespace captures supported extra namespace directives", {
  tmp <- tempfile("namespace-fixture-")
  dir.create(tmp)

  writeLines(
    c(
      "exportPattern(\"^[[:alpha:]]+\")",
      "exportClasses(MyClass)",
      "exportMethods(show)",
      "importClassesFrom(methods, setRefClass)",
      "importMethodsFrom(methods, show)",
      "importFrom(stats, setNames)"
    ),
    file.path(tmp, "NAMESPACE")
  )

  result <- withr::with_dir(tmp, read_namespace_json())

  expect_null(result$error)
  expect_contains(result$exports, "MyClass")
  expect_contains(result$exports, "show")
  expect_contains(result$imports$methods, "setRefClass")
  expect_contains(result$imports$methods, "show")
  expect_equal(result$imports$stats[[1]], "setNames")
})

test_that("tool_package_namespace exposes optional package argument", {
  schema <- mcp_tool_definition(
    rairai:::tool_package_namespace(new.env(parent = emptyenv()))
  )$inputSchema

  expect_equal(schema$type, "object")
  expect_false("required" %in% names(schema))
  expect_equal(schema$additionalProperties, FALSE)
  expect_equal(schema$properties$package$type, "string")
})
