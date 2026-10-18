test_that("r6_classes returns empty list when no R directory exists", {
  withr::with_tempdir({
    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )
    expect_equal(result$count, 0L)
    expect_equal(result$classes, list())
  })
})

test_that("r6_classes discovers a simple R6 class", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'MyClass <- R6::R6Class(',
        '  "MyClass",',
        '  public = list(',
        '    x = NULL',
        '  )',
        ')'
      ),
      "R/my-class.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$classes[[1]]$name, "MyClass")
    expect_equal(result$classes[[1]]$location, "R/my-class.R:1-6")
    expect_null(result$classes[[1]]$inherit)
  })
})

test_that("r6_classes captures inheritance", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'ChildClass <- R6::R6Class(',
        '  "ChildClass",',
        '  inherit = ParentClass,',
        '  public = list()',
        ')'
      ),
      "R/child-class.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$classes[[1]]$name, "ChildClass")
    expect_equal(result$classes[[1]]$inherit, "ParentClass")
  })
})

test_that("r6_classes discovers multiple classes across files", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('A <- R6::R6Class("A", public = list())'),
      "R/a.R"
    )
    writeLines(
      c('B <- R6::R6Class("B", inherit = A, public = list())'),
      "R/b.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 2L)
    names_found <- map_chr(result$classes, "name")
    expect_setequal(names_found, c("A", "B"))
    locations_found <- map_chr(result$classes, "location")
    expect_setequal(locations_found, c("R/a.R:1", "R/b.R:1"))
  })
})

test_that("r6_classes handles R6Class without namespace prefix", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('Plain <- R6Class("Plain", public = list())'),
      "R/plain.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$classes[[1]]$name, "Plain")
  })
})

test_that("r6_classes ignores namespace-qualified R6Class lookalikes", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'Fake <- fakepkg::R6Class("Fake", public = list())',
        'Triple <- R6:::R6Class("Triple", public = list())',
        'Qualified <- R6::R6Class("Qualified", public = list())',
        'Bare <- R6Class("Bare", public = list())'
      ),
      "R/classes.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 2L)
    expect_setequal(map_chr(result$classes, "name"), c("Qualified", "Bare"))
  })
})

test_that("r6_classes separates exact class locations from leading comments", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        "#' Class title",
        "# regular comment",
        "#' @description Details",
        'Documented <- R6::R6Class(',
        '  "Documented",',
        '  public = list() ',
        ')'
      ),
      "R/documented.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$classes[[1]]$name, "Documented")
    expect_equal(result$classes[[1]]$location, "R/documented.R:4-7")
    expect_equal(result$classes[[1]]$leading_comment_lines, "1-3")
  })
})

test_that("r6_classes uses flat R source scope with .R and .r files", {
  withr::with_tempdir({
    dir.create("R")
    dir.create("R/nested")
    writeLines('Upper <- R6::R6Class("Upper")', "R/upper.R")
    writeLines('Lower <- R6::R6Class("Lower")', "R/lower.r")
    writeLines('Nested <- R6::R6Class("Nested")', "R/nested/nested.R")

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 2L)
    expect_setequal(map_chr(result$classes, "name"), c("Upper", "Lower"))
    expect_false("Nested" %in% map_chr(result$classes, "name"))
  })
})

test_that("r6_class returns error for missing name", {
  result <- tool_result_json(
    rairai:::r6_class(""),
    simplifyVector = FALSE
  )
  expect_equal(result$error$code, "missing_name")
})

test_that("r6_class returns error when class not found", {
  withr::with_tempdir({
    dir.create("R")

    result <- tool_result_json(
      rairai:::r6_class("NonExistent"),
      simplifyVector = FALSE
    )
    expect_equal(result$error$code, "class_not_found")
  })
})

test_that("r6_class returns full structure including fields and methods", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'MyClass <- R6::R6Class(',
        '  "MyClass",',
        '  inherit = Base,',
        '  public = list(',
        '    field1 = NULL,',
        '    field2 = 42L,',
        '    initialize = function(x, y = 1) {',
        '      self$field1 <- x',
        '    },',
        '    run = function(a, b = "hello") { a }',
        '  ),',
        '  private = list(',
        '    secret = 0,',
        '    helper = function() { invisible(NULL) }',
        '  ),',
        '  active = list(',
        '    value = function(v) { self$field2 }',
        '  )',
        ')'
      ),
      "R/my-class.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("MyClass"),
      simplifyVector = FALSE
    )

    expect_equal(result$name, "MyClass")
    expect_equal(result$location, "R/my-class.R:1-19")
    expect_equal(result$inherit, "Base")

    # public members
    public_names <- map_chr(result$public, "name")
    expect_equal(public_names, c("field1", "field2", "initialize", "run"))

    field1 <- result$public[[1]]
    expect_equal(field1$kind, "field")
    expect_equal(field1$default, "NULL")
    expect_equal(field1$location, "R/my-class.R:5")

    field2 <- result$public[[2]]
    expect_equal(field2$kind, "field")
    expect_equal(field2$default, "42L")
    expect_equal(field2$location, "R/my-class.R:6")

    init <- result$public[[3]]
    expect_equal(init$kind, "method")
    expect_equal(init$signature, "initialize(x, y = 1)")
    expect_equal(init$location, "R/my-class.R:7-9")

    run <- result$public[[4]]
    expect_equal(run$kind, "method")
    expect_equal(run$signature, 'run(a, b = "hello")')
    expect_equal(run$location, "R/my-class.R:10")

    # private members
    private_names <- map_chr(result$private, "name")
    expect_equal(private_names, c("secret", "helper"))

    secret <- result$private[[1]]
    expect_equal(secret$kind, "field")
    expect_equal(secret$default, "0")
    expect_equal(secret$location, "R/my-class.R:13")

    helper <- result$private[[2]]
    expect_equal(helper$kind, "method")
    expect_equal(helper$signature, "helper()")
    expect_equal(helper$location, "R/my-class.R:14")

    # active bindings
    expect_equal(length(result$active), 1L)
    value <- result$active[[1]]
    expect_equal(value$name, "value")
    expect_equal(value$kind, "active")
    expect_equal(value$signature, "value(v)")
    expect_equal(value$location, "R/my-class.R:17")
  })
})

test_that("r6_class returns empty sections when none defined", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('Minimal <- R6::R6Class("Minimal")'),
      "R/minimal.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("Minimal"),
      simplifyVector = FALSE
    )

    expect_equal(result$name, "Minimal")
    expect_equal(result$location, "R/minimal.R:1")
    expect_equal(result$public, list())
    expect_equal(result$private, list())
    expect_equal(result$active, list())
  })
})

test_that("dedicated R6 outputs expose compact locations only", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        '# class context',
        'A <- R6::R6Class(',
        '  "A",',
        '  public = list(',
        '    # run context',
        '    run = function() 1',
        '  )',
        ')',
        'use_it <- function(obj) obj$run()'
      ),
      "R/a.R"
    )

    class_result <- tool_result_json(
      rairai:::r6_class("A"),
      simplifyVector = FALSE
    )
    method_result <- tool_result_json(
      rairai:::r6_method("A", "run"),
      simplifyVector = FALSE
    )
    reference_result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )

    public_names <- unique(c(
      names(class_result),
      names(class_result$public[[1L]]),
      names(method_result),
      names(reference_result$references[[1L]])
    ))
    expect_false(any(
      public_names %in%
        c(
          "range",
          "file",
          "path",
          "line",
          "column",
          "start_line",
          "start_column",
          "end_line",
          "end_column"
        )
    ))
    expect_true(all(c(
      "location" %in% names(class_result),
      "location" %in% names(class_result$public[[1L]]),
      "location" %in% names(method_result),
      "location" %in% names(reference_result$references[[1L]])
    )))
    expect_equal(class_result$location, "R/a.R:2-8")
    expect_equal(class_result$leading_comment_lines, "1")
    expect_equal(class_result$public[[1L]]$location, "R/a.R:6")
    expect_equal(class_result$public[[1L]]$leading_comment_lines, "5")
    expect_equal(method_result$location, "R/a.R:6")
    expect_equal(method_result$leading_comment_lines, "5")
    expect_equal(
      reference_result$target_resolution$class$matches[[
        1L
      ]]$leading_comment_lines,
      "1"
    )
    expect_equal(
      reference_result$target_resolution$method$matches[[
        1L
      ]]$leading_comment_lines,
      "5"
    )
    expect_false(
      "leading_comment_lines" %in% names(reference_result$references[[1L]])
    )
  })
})

test_that("r6_class handles method with no parameters", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class(',
        '  "A",',
        '  public = list(',
        '    greet = function() { "hello" }',
        '  )',
        ')'
      ),
      "R/a.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("A"),
      simplifyVector = FALSE
    )

    greet <- result$public[[1]]
    expect_equal(greet$name, "greet")
    expect_equal(greet$kind, "method")
    expect_equal(greet$signature, "greet()")
  })
})

test_that("r6_class uses classname argument when provided", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'obj <- R6::R6Class(',
        '  classname = "ActualName",',
        '  public = list(x = 1)',
        ')'
      ),
      "R/named.R"
    )

    result_by_actual <- tool_result_json(
      rairai:::r6_class("ActualName"),
      simplifyVector = FALSE
    )
    expect_equal(result_by_actual$name, "ActualName")

    # Searching by var name should not find it
    result_by_var <- tool_result_json(
      rairai:::r6_class("obj"),
      simplifyVector = FALSE
    )
    expect_equal(result_by_var$error$code, "class_not_found")
  })
})

test_that("r6_class returns ambiguous when class name exists in multiple files", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('MyClass <- R6::R6Class("MyClass", public = list())'),
      "R/a.R"
    )
    writeLines(
      c('MyClass <- R6::R6Class("MyClass", public = list(x = 1))'),
      "R/b.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("MyClass"),
      simplifyVector = FALSE
    )

    expect_equal(result$status, "ambiguous")
    expect_equal(result$name, "MyClass")
    expect_equal(result$count, 2L)
    expect_equal(length(result$matches), 2L)
    match_locations <- map_chr(result$matches, "location")
    expect_setequal(match_locations, c("R/a.R:1", "R/b.R:1"))
  })
})

test_that("r6_class normalizes whitespace in name", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('MyClass <- R6::R6Class("MyClass", public = list())'),
      "R/my-class.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("  MyClass  "),
      simplifyVector = FALSE
    )

    expect_equal(result$name, "MyClass")
  })
})

test_that("r6_classes surfaces parse errors for broken files", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('ValidClass <- R6::R6Class("ValidClass", public = list())'),
      "R/valid.R"
    )
    writeLines(
      c("this is {{ broken R code"),
      "R/broken.R"
    )

    result <- tool_result_json(
      rairai:::r6_classes(),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$classes[[1]]$name, "ValidClass")
    expect_equal(length(result$parse_errors), 1L)
    expect_true(str_detect(result$parse_errors[[1]]$file, fixed("broken.R")))
    expect_true(nchar(result$parse_errors[[1]]$message) > 0)
  })
})

test_that("r6_class includes parse errors in class_not_found response", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("this is {{ broken R code"),
      "R/broken.R"
    )

    result <- tool_result_json(
      rairai:::r6_class("Missing"),
      simplifyVector = FALSE
    )

    expect_equal(result$error$code, "class_not_found")
    expect_equal(length(result$error$parse_errors), 1L)
    expect_true(str_detect(
      result$error$parse_errors[[1]]$file,
      fixed("broken.R")
    ))
  })
})

test_that("tool_r6_classes is in the served toolset", {
  tools <- rairai:::serve_tools(NULL)
  tool_names <- mcp_tool_names(tools)
  expect_true("r6_classes" %in% tool_names)
  expect_true("r6_class" %in% tool_names)
  expect_true("r6_method" %in% tool_names)
  expect_true("r6_method_references" %in% tool_names)
})

# r6_method tests ---------------------------------------------------------------

test_that("r6_method returns error for missing class", {
  result <- tool_result_json(
    rairai:::r6_method("", "run"),
    simplifyVector = FALSE
  )
  expect_equal(result$error$code, "missing_class")
})

test_that("r6_method returns error for missing method", {
  result <- tool_result_json(
    rairai:::r6_method("MyClass", ""),
    simplifyVector = FALSE
  )
  expect_equal(result$error$code, "missing_method")
})

test_that("r6_method returns error when method not found", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('A <- R6::R6Class("A", public = list(x = 1))'),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "nonexistent"),
      simplifyVector = FALSE
    )
    expect_equal(result$error$code, "method_not_found")
  })
})

test_that("r6_method returns exact source for a public method", {
  withr::with_tempdir({
    dir.create("R")
    method_source <- c(
      '    run = function(x, y = 1) {',
      '      # keep this comment',
      '      total <- x  +  y',
      '',
      '      total',
      '    }'
    )
    writeLines(
      c(
        'A <- R6::R6Class(',
        '  "A",',
        '  public = list(',
        '    # run method context',
        method_source,
        '  )',
        ')'
      ),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$class, "A")
    expect_equal(result$method, "run")
    expect_equal(result$section, "public")
    expect_equal(result$signature, "run(x, y = 1)")
    expected_source <- method_source
    expected_source[[1L]] <- str_remove(expected_source[[1L]], "^    ")
    expect_equal(result$source, paste(expected_source, collapse = "\n"))
    expect_equal(result$location, "R/a.R:5-10")
    expect_equal(result$leading_comment_lines, "4")
    expect_false(str_detect(result$source, fixed("run method context")))
    expect_null(result$file)
    expect_null(result$line)
    expect_null(result$start_line)
    expect_null(result$end_line)
  })
})

test_that("r6_method returns line range for one-line methods", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class(',
        '  "A",',
        '  public = list(',
        '    run = function() 1',
        '  )',
        ')'
      ),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$source, "run = function() 1")
    expect_equal(result$location, "R/a.R:4")
    expect_null(result$line)
    expect_null(result$start_line)
    expect_null(result$end_line)
  })
})

test_that("r6_method extracts only the requested inline method", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      'A <- R6::R6Class("A", public = list(run = function() 1, other = function() 2))',
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$source, "run = function() 1")
    expect_equal(result$location, "R/a.R:1")
  })
})

test_that("r6_method slices inline source after UTF-8 text on the same line", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      paste0(
        "A <- R6::R6Class(\"A\", public = list(",
        "label = \"é\", run = function() 1, other = function() 2))"
      ),
      "R/a.R",
      useBytes = TRUE
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$source, "run = function() 1")
    expect_equal(result$location, "R/a.R:1")
  })
})

test_that("r6_method finds private methods", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class(',
        '  "A",',
        '  private = list(',
        '    helper = function() { invisible(NULL) }',
        '  )',
        ')'
      ),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "helper"),
      simplifyVector = FALSE
    )
    expect_equal(result$section, "private")
    expect_equal(result$method, "helper")
    expect_match(result$source, "invisible")
  })
})

test_that("r6_method finds active bindings", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class(',
        '  "A",',
        '  active = list(',
        '    value = function(v) { self$x }',
        '  )',
        ')'
      ),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "value"),
      simplifyVector = FALSE
    )
    expect_equal(result$section, "active")
    expect_equal(result$signature, "value(v)")
    expect_match(result$source, "self\\$x")
  })
})

test_that("r6_method returns error when class not found", {
  withr::with_tempdir({
    dir.create("R")
    result <- tool_result_json(
      rairai:::r6_method("NoSuchClass", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$error$code, "class_not_found")
  })
})

test_that("r6_method returns method_not_found when class exists but method absent", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c('A <- R6::R6Class("A", public = list(x = 1))'),
      "R/a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method("A", "nonexistent"),
      simplifyVector = FALSE
    )
    expect_equal(result$error$code, "method_not_found")
  })
})

# r6_method_references tests ---------------------------------------------------

test_that("r6 member search parses each file once and targets only package classes", {
  root <- withr::local_tempdir()
  fs::dir_create(fs::path(root, "R", "nested"))
  fs::dir_create(fs::path(root, "tests", "testthat", "nested"))
  writeLines(
    c(
      'A <- R6::R6Class("A", public = list(`run-now` = function() 1))',
      'obj$`run-now`()',
      '# obj$`run-now`()',
      '"obj$`run-now`()"'
    ),
    fs::path(root, "R", "a.R")
  )
  writeLines('other$`run-now`(, 1)', fs::path(root, "R", "b.r"))
  writeLines('broken <- function(', fs::path(root, "R", "z-broken.R"))
  writeLines('ignored$`run-now`()', fs::path(root, "R", "nested", "ignored.R"))
  writeLines(
    c(
      'A <- R6::R6Class("A", public = list(`run-now` = function() 2))',
      'TestOnly <- R6::R6Class("TestOnly", public = list(`run-now` = function() 3))',
      'test_obj$`run-now`()'
    ),
    fs::path(root, "tests", "testthat", "test-a.R")
  )
  writeLines(
    'nested_obj$`run-now`()',
    fs::path(root, "tests", "testthat", "nested", "test-b.r")
  )
  writeLines(
    'broken <- function(',
    fs::path(root, "tests", "testthat", "nested", "test-broken.R")
  )
  writeLines('ignored$`run-now`()', fs::path(root, "tests", "ignored.R"))
  original_parse <- rairai:::r6_syntax_parse_file
  parsed_paths <- character()
  testthat::local_mocked_bindings(
    r6_syntax_parse_file = function(root_path, path) {
      parsed_paths <<- c(parsed_paths, path)
      original_parse(root_path, path)
    },
    .package = "rairai"
  )

  result <- tool_result_json(
    rairai:::r6_method_references("A", "run-now", root_path = root),
    simplifyVector = FALSE
  )

  expect_equal(
    parsed_paths,
    c(
      "R/a.R",
      "R/b.r",
      "R/z-broken.R",
      "tests/testthat/nested/test-b.r",
      "tests/testthat/nested/test-broken.R",
      "tests/testthat/test-a.R"
    )
  )
  expect_equal(result$target_resolution$class$status, "unique")
  expect_equal(result$target_resolution$method$status, "unique")
  expect_equal(result$target_resolution$class$matches[[1]]$location, "R/a.R:1")
  expect_equal(
    map_chr(result$references, "location"),
    c(
      "R/a.R:2",
      "R/b.r:1",
      "tests/testthat/nested/test-b.r:1",
      "tests/testthat/test-a.R:3"
    )
  )
  expect_equal(
    map_chr(result$references, "object"),
    c("obj", "other", "nested_obj", "test_obj")
  )
  expect_equal(
    map_chr(result$parse_errors, "file"),
    c("R/z-broken.R", "tests/testthat/nested/test-broken.R")
  )

  missing <- tool_result_json(
    rairai:::r6_method_references("TestOnly", "run-now", root_path = root),
    simplifyVector = FALSE
  )
  expect_equal(missing$target_resolution$class$status, "missing")
  expect_equal(missing$target_resolution$method$status, "not_checked")
  expect_equal(missing$references, result$references)
})

test_that("r6_method_references returns error for missing class", {
  result <- tool_result_json(
    rairai:::r6_method_references("", "run"),
    simplifyVector = FALSE
  )
  expect_equal(result$error$code, "missing_class")
})

test_that("r6_method_references returns error for missing method", {
  result <- tool_result_json(
    rairai:::r6_method_references("MyClass", ""),
    simplifyVector = FALSE
  )
  expect_equal(result$error$code, "missing_method")
})

test_that("r6_method_references returns empty when no matches", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(c('x <- 1'), "R/a.R")
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$count, 0L)
    expect_equal(result$references, list())
    expect_true(nchar(result$note) > 0)
    expect_equal(result$target_resolution$class$status, "missing")
    expect_equal(result$target_resolution$method$status, "not_checked")
  })
})

test_that("r6_method_references reports target resolution metadata", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class("A", public = list(run = function() 1))',
        'B <- R6::R6Class("B", public = list(other = function() 1))',
        'Dup <- R6::R6Class("Dup", public = list(run = function() 1))'
      ),
      "R/a.R"
    )
    writeLines(
      c('Dup <- R6::R6Class("Dup", public = list(run = function() 2))'),
      "R/b.R"
    )
    writeLines("use_it <- function(obj) obj$run()", "R/use.R")

    unique_result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    missing_method <- tool_result_json(
      rairai:::r6_method_references("B", "run"),
      simplifyVector = FALSE
    )
    ambiguous_class <- tool_result_json(
      rairai:::r6_method_references("Dup", "run"),
      simplifyVector = FALSE
    )

    expect_equal(unique_result$target_resolution$class$status, "unique")
    expect_equal(unique_result$target_resolution$method$status, "unique")
    expect_equal(missing_method$target_resolution$class$status, "unique")
    expect_equal(missing_method$target_resolution$method$status, "missing")
    expect_equal(ambiguous_class$target_resolution$class$status, "ambiguous")
    expect_equal(ambiguous_class$target_resolution$method$status, "ambiguous")
    expect_equal(missing_method$count, 1L)
    expect_equal(ambiguous_class$count, 1L)
  })
})

test_that("r6_method_references finds obj$method() calls in R files", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'use_it <- function(obj) {',
        '  obj$run(1, 2)',
        '}'
      ),
      "R/use.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$class, "A")
    expect_equal(result$method, "run")
    expect_equal(result$count, 1L)
    ref <- result$references[[1L]]
    expect_equal(ref$location, "R/use.R:2")
    expect_equal(ref$object, "obj")
    expect_match(ref$call, "run")
  })
})

test_that("r6_method_references ignores comments and strings", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'use_it <- function(obj) {',
        '  "obj$run()"',
        '  # obj$run()',
        '  obj$run()',
        '}'
      ),
      "R/use.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$references[[1L]]$location, "R/use.R:4")
  })
})

test_that("r6_method_references finds calls with self and private", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        'A <- R6::R6Class("A", public = list(',
        '  go = function() {',
        '    self$run()',
        '    private$helper()',
        '  },',
        '  run = function() invisible(NULL)',
        '), private = list(',
        '  helper = function() invisible(NULL)',
        '))'
      ),
      "R/a.R"
    )
    run_result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_gte(run_result$count, 1L)

    helper_result <- tool_result_json(
      rairai:::r6_method_references("A", "helper"),
      simplifyVector = FALSE
    )
    expect_gte(helper_result$count, 1L)
  })
})

test_that("r6_method_references searches test files", {
  withr::with_tempdir({
    dir.create("R")
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c('A <- R6::R6Class("A", public = list(run = function() 1))'),
      "R/a.R"
    )
    writeLines(
      c(
        'test_that("run works", {',
        '  obj <- A$new()',
        '  obj$run()',
        '})'
      ),
      "tests/testthat/test-a.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_gte(result$count, 1L)
    locations <- map_chr(result$references, "location")
    expect_true(any(str_detect(locations, "test-a")))
  })
})

test_that("r6_method_references handles omitted AST arguments", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        "use_it <- function(df, obj) {",
        "  df[1, , drop = FALSE]",
        "  obj$run()",
        "}"
      ),
      "R/use.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$count, 1L)
    ref <- result$references[[1L]]
    expect_equal(ref$location, "R/use.R:3")
    expect_equal(ref$object, "obj")
    expect_match(ref$call, "run")
  })
})

test_that("r6_method_references finds backticked method calls", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        "use_it <- function(obj) {",
        "  obj$`foo-bar`()",
        "}"
      ),
      "R/use.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "foo-bar"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$references[[1L]]$location, "R/use.R:2")
  })
})

test_that("r6_method_references reports distinct lines for repeated calls", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c(
        "use_it <- function(obj) {",
        "  obj$run()",
        "  obj$run(1)",
        "  obj$run(2)",
        "}"
      ),
      "R/use.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )
    expect_equal(result$count, 3L)
    expect_equal(
      map_chr(result$references, "location"),
      c("R/use.R:2", "R/use.R:3", "R/use.R:4")
    )
    expect_equal(map_chr(result$references, "object"), rep("obj", 3L))
  })
})

test_that("r6_method_references dogfoods PackageToolService symbols", {
  if (file.exists("R/tool-definitions.R")) {
    source_root <- "."
  } else if (file.exists("../../R/tool-definitions.R")) {
    source_root <- "../.."
  } else {
    source_root <- NA_character_
  }
  skip_if_not(!is.na(source_root))
  withr::local_dir(source_root)

  result <- tool_result_json(
    rairai:::r6_method_references("PackageToolService", "symbols"),
    simplifyVector = FALSE
  )

  expect_gte(result$count, 1L)
  calls <- map_chr(result$references, "call")
  locations <- map_chr(result$references, "location")
  expect_true(any(
    str_detect(locations, "R/tool-definitions[.]R") &
      str_detect(calls, "service[$]symbols")
  ))
})

test_that("r6_method_references reports parse errors without text fallback", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("this is {{ broken R code\n\"obj$run()\""),
      "R/broken.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 0L)
    expect_equal(length(result$parse_errors), 1L)
    expect_true(str_detect(result$parse_errors[[1L]]$file, fixed("broken.R")))
  })
})

test_that("r6_method_references surfaces irrelevant parse errors", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(
      c("this is {{ broken R code\nobj$other()"),
      "R/broken.R"
    )
    result <- tool_result_json(
      rairai:::r6_method_references("A", "run"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 0L)
    expect_equal(length(result$parse_errors), 1L)
  })
})
