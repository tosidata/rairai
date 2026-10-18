expect_has_names <- function(object, expected) {
  expect_true(
    all(expected %in% names(object)),
    info = paste(
      "Missing names:",
      paste(setdiff(expected, names(object)), collapse = ", ")
    )
  )
}

expect_exact_names <- function(object, expected) {
  object_names <- names(object)
  if (is.null(object_names)) {
    expect_true(
      FALSE,
      info = paste("Expected names:", paste(expected, collapse = ", "))
    )
    return(invisible(NULL))
  }
  expect_setequal(object_names, expected)
}

expect_scalar_character <- function(object) {
  is_scalar_character <- is.character(object) && length(object) == 1L
  expect_true(is_scalar_character)
}

expect_scalar_logical <- function(object) {
  is_scalar_logical <- is.logical(object) && length(object) == 1L
  expect_true(is_scalar_logical)
}

expect_non_negative_count <- function(object) {
  is_count <- (is.numeric(object) || is.integer(object)) && length(object) == 1L
  expect_true(is_count)
  if (is_count) {
    expect_gte(object, 0)
  }
}

expect_more_tool <- function(more, tool) {
  expect_has_names(more, c("tool", "reason"))
  expect_equal(more$tool, tool)
  expect_scalar_character(more$reason)
}

expect_more_tools <- function(more, tools) {
  expect_has_names(more, c("tools", "reason"))
  has_tools <- is.list(more$tools) || is.character(more$tools)
  expect_true(has_tools)
  if (has_tools) {
    expect_setequal(unlist(more$tools, use.names = FALSE), tools)
  }
  expect_scalar_character(more$reason)
}

read_package_overview_result <- function() {
  root <- tryCatch(pkgload::pkg_path(), error = function(cnd) NULL)
  testthat::skip_if(is.null(root), "Not in source tree")
  withr::local_dir(root)
  tool_result_json(
    rairai:::package_overview(),
    simplifyVector = FALSE
  )
}

write_description <- function(package = "fixture", suggests = NULL) {
  lines <- c(
    paste0("Package: ", package),
    "Title: Fixture Package",
    "Version: 0.0.1",
    "Description: A fixture package.",
    "License: MIT"
  )
  if (!is.null(suggests)) {
    lines <- c(lines, paste0("Suggests: ", suggests))
  }
  writeLines(lines, "DESCRIPTION")
}

read_local_overview <- function() {
  tool_result_json(rairai:::package_overview(), simplifyVector = FALSE)
}

expect_json_array_member <- function(json, key) {
  expect_match(tool_result_text(json), paste0('"', key, '":\\['))
}

expect_layout_node <- function(node) {
  expect_has_names(
    node,
    c("path", "direct_file_count", "recursive_file_count", "children")
  )
  expect_scalar_character(node$path)
  expect_non_negative_count(node$direct_file_count)
  expect_non_negative_count(node$recursive_file_count)
  expect_true(is.list(node$children))
  for (child in node$children) {
    expect_layout_node(child)
  }
}

test_that("package_overview returns only the redesigned top-level sections", {
  result <- read_package_overview_result()

  required_sections <- c(
    "description",
    "namespace",
    "files",
    "documentation",
    "tests",
    "source_objects"
  )
  removed_sections <- c("package", "dependencies", "source", "r6", "next_tools")

  expect_setequal(names(result), required_sections)
  expect_false(any(removed_sections %in% names(result)))
})

test_that("package_overview exposes the DESCRIPTION contract", {
  result <- read_package_overview_result()
  description <- result$description
  fields <- description$fields

  expect_has_names(description, c("fields", "more"))
  expect_has_names(
    fields,
    c("Package", "Title", "Version", "Description", "License")
  )
  expect_equal(fields$Package, "rairai")
  expect_scalar_character(fields$Title)
  expect_scalar_character(fields$Version)
  expect_scalar_character(fields$Description)
  expect_scalar_character(fields$License)

  dependency_fields <- c(
    "Depends",
    "Imports",
    "Suggests",
    "Enhances",
    "LinkingTo",
    "Remotes"
  )
  expect_has_names(fields, dependency_fields)
  for (field in dependency_fields) {
    expect_true(
      is.list(fields[[field]]) || is.character(fields[[field]]),
      info = field
    )
  }

  expect_more_tool(description$more, "package_dependencies")
})

test_that("package_overview exposes the namespace contract", {
  result <- read_package_overview_result()
  namespace <- result$namespace

  expect_has_names(
    namespace,
    c(
      "exports",
      "imports_count",
      "s3_methods_count",
      "use_dyn_lib_count",
      "more"
    )
  )
  expect_true(is.list(namespace$exports) || is.character(namespace$exports))
  expect_true("serve" %in% unlist(namespace$exports, use.names = FALSE))
  expect_non_negative_count(namespace$imports_count)
  expect_non_negative_count(namespace$s3_methods_count)
  expect_non_negative_count(namespace$use_dyn_lib_count)
  expect_more_tool(namespace$more, "package_namespace")
})

test_that("package_overview exposes the files layout contract", {
  result <- read_package_overview_result()
  files <- result$files

  expect_has_names(
    files,
    c("summary", "has_package_files", "has_package_dirs", "layout", "more")
  )
  expect_has_names(
    files$summary,
    c(
      "total_file_count",
      "top_level_dir_count",
      "top_level_file_count"
    )
  )
  expect_non_negative_count(files$summary$total_file_count)
  expect_non_negative_count(files$summary$top_level_dir_count)
  expect_non_negative_count(files$summary$top_level_file_count)

  package_file_keys <- c(
    "DESCRIPTION",
    "NAMESPACE",
    "README.md",
    "README.Rmd",
    "NEWS.md",
    "NEWS",
    "LICENSE",
    "LICENSE.md",
    ".Rbuildignore",
    "_pkgdown.yml"
  )
  package_dir_keys <- c(
    "R",
    "tests",
    "tests/testthat",
    "man",
    "vignettes",
    "data",
    "data-raw",
    "src",
    "inst"
  )

  expect_exact_names(files$has_package_files, package_file_keys)
  expect_exact_names(files$has_package_dirs, package_dir_keys)
  for (key in package_file_keys) {
    expect_scalar_logical(files$has_package_files[[key]])
  }
  for (key in package_dir_keys) {
    expect_scalar_logical(files$has_package_dirs[[key]])
  }

  expect_true(is.list(files$layout))
  for (node in files$layout) {
    expect_layout_node(node)
  }
  expect_more_tool(files$more, "package_files")
})

test_that("package_overview exposes documentation and test-suite contracts", {
  result <- read_package_overview_result()
  documentation <- result$documentation
  tests <- result$tests

  expect_has_names(
    documentation,
    c("man_pages_count", "vignettes_count", "more")
  )
  expect_non_negative_count(documentation$man_pages_count)
  expect_non_negative_count(documentation$vignettes_count)
  expect_more_tools(
    documentation$more,
    c(
      "package_docs",
      "help_page",
      "help_search",
      "package_vignettes",
      "package_news"
    )
  )

  expect_has_names(
    tests,
    c(
      "frameworks",
      "test_count",
      "snapshot_count",
      "helper_file_count",
      "fixture_dir_count",
      "more"
    )
  )
  expect_true(is.list(tests$frameworks) || is.character(tests$frameworks))
  expect_true("testthat" %in% unlist(tests$frameworks, use.names = FALSE))
  expect_non_negative_count(tests$test_count)
  expect_non_negative_count(tests$snapshot_count)
  expect_non_negative_count(tests$helper_file_count)
  expect_non_negative_count(tests$fixture_dir_count)
  expect_more_tool(tests$more, "tests_for")
})

test_that("package_overview exposes source object contracts and stable serve object", {
  result <- read_package_overview_result()
  source_objects <- result$source_objects

  expect_has_names(
    source_objects,
    c(
      "counts",
      "total_objects",
      "listed_objects",
      "objects",
      "objects_truncated",
      "parse_errors",
      "more"
    )
  )
  expect_has_names(
    source_objects$counts,
    c("functions", "r6_classes", "other_objects")
  )
  expect_non_negative_count(source_objects$counts$functions)
  expect_non_negative_count(source_objects$counts$r6_classes)
  expect_non_negative_count(source_objects$counts$other_objects)
  expect_non_negative_count(source_objects$total_objects)
  expect_non_negative_count(source_objects$listed_objects)
  expect_lte(source_objects$listed_objects, source_objects$total_objects)
  expect_lte(source_objects$listed_objects, 20L)
  expect_scalar_logical(source_objects$objects_truncated)

  expect_true(is.list(source_objects$objects))
  for (object in source_objects$objects) {
    expect_has_names(object, c("name", "kind", "location"))
    expect_scalar_character(object$name)
    expect_scalar_character(object$kind)
    expect_scalar_character(object$location)
    expect_true(object$kind %in% c("function", "r6_class", "other_object"))
  }

  expect_true(source_objects$total_objects >= source_objects$listed_objects)

  counts_are_available <- all(
    c("functions", "r6_classes", "other_objects") %in%
      names(source_objects$counts)
  )
  if (
    is.list(source_objects$objects) &&
      counts_are_available &&
      !isTRUE(source_objects$objects_truncated)
  ) {
    expected_counts <- table(factor(
      map_chr(source_objects$objects, "kind"),
      levels = c("function", "r6_class", "other_object")
    ))
    expect_equal(
      source_objects$counts$functions,
      unname(expected_counts[["function"]])
    )
    expect_equal(
      source_objects$counts$r6_classes,
      unname(expected_counts[["r6_class"]])
    )
    expect_equal(
      source_objects$counts$other_objects,
      unname(expected_counts[["other_object"]])
    )
  }
  expect_more_tools(
    source_objects$more,
    c("symbols", "definition", "r6_classes", "r6_class")
  )
})

test_that("package_overview serializes required array fields as JSON arrays", {
  withr::with_tempdir({
    write_description(suggests = "testthat")
    dir.create("R")
    dir.create("man")
    dir.create("vignettes")
    dir.create("tests/testthat/_snaps", recursive = TRUE)
    dir.create("tests/testthat/fixtures", recursive = TRUE)
    writeLines("export(fn)", "NAMESPACE")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines(
      "test_that('works', { expect_true(TRUE) })",
      "tests/testthat/test-fn.R"
    )
    writeLines("# snapshot", "tests/testthat/_snaps/fn.md")

    json <- rairai:::package_overview()

    for (field in c(
      "Depends",
      "Imports",
      "Suggests",
      "Enhances",
      "LinkingTo",
      "Remotes"
    )) {
      expect_json_array_member(json, field)
    }
    expect_json_array_member(json, "exports")
    expect_json_array_member(json, "layout")
    expect_json_array_member(json, "children")
    expect_json_array_member(json, "tools")
    expect_json_array_member(json, "frameworks")
    expect_json_array_member(json, "objects")
    expect_json_array_member(json, "parse_errors")
    expect_match(tool_result_text(json), '"Remotes":\\[\\]')
  })
})

test_that("package_overview returns required sections for a minimal package", {
  withr::with_tempdir({
    write_description(package = "minimal")

    result <- read_local_overview()

    expect_setequal(
      names(result),
      c(
        "description",
        "namespace",
        "files",
        "documentation",
        "tests",
        "source_objects"
      )
    )
    expect_true(result$files$has_package_files$DESCRIPTION)
    expect_false(result$files$has_package_files$NAMESPACE)
    expect_false(result$files$has_package_dirs$R)
    expect_false(result$files$has_package_dirs$tests)
    expect_false(result$files$has_package_dirs$`tests/testthat`)
    expect_equal(result$files$layout, list())
    expect_equal(result$files$summary$total_file_count, 1L)
    expect_equal(result$files$summary$top_level_dir_count, 0L)
    expect_equal(result$files$summary$top_level_file_count, 1L)
    expect_true(
      isTRUE(result$namespace$unavailable) || !is.null(result$namespace$error)
    )
    expect_equal(result$documentation$man_pages_count, 0L)
    expect_equal(result$documentation$vignettes_count, 0L)
    expect_equal(result$tests$frameworks, list())
    expect_equal(result$tests$test_count, 0L)
    expect_equal(result$tests$snapshot_count, 0L)
    expect_equal(result$tests$helper_file_count, 0L)
    expect_equal(result$tests$fixture_dir_count, 0L)
    expect_equal(result$source_objects$counts$functions, 0L)
    expect_equal(result$source_objects$counts$r6_classes, 0L)
    expect_equal(result$source_objects$counts$other_objects, 0L)
    expect_equal(result$source_objects$total_objects, 0L)
    expect_equal(result$source_objects$listed_objects, 0L)
    expect_false(result$source_objects$objects_truncated)
    expect_equal(result$source_objects$objects, list())
    expect_equal(result$source_objects$parse_errors, list())

    json <- rairai:::package_overview()
    expect_match(tool_result_text(json), '"layout":\\[\\]')
    expect_match(tool_result_text(json), '"objects":\\[\\]')
    expect_match(tool_result_text(json), '"parse_errors":\\[\\]')
  })
})

test_that("package_overview reports controlled source objects and test counts", {
  withr::with_tempdir({
    write_description(suggests = "testthat")
    dir.create("R")
    dir.create("tests/testthat/_snaps", recursive = TRUE)
    dir.create("tests/testthat/fixtures", recursive = TRUE)
    writeLines("importFrom(R6,R6Class)", "NAMESPACE")
    writeLines(
      c(
        "#' Arrow function title",
        "#' Arrow function details",
        "fn_arrow <- function(x) {",
        "  x",
        "}",
        "fn_equal = function(y) y",
        "Thing <- R6::R6Class('Thing')",
        "ImportedThing <- R6Class('ImportedThing')",
        "value <- 1"
      ),
      "R/objects.R"
    )
    writeLines(
      c(
        "test_that('one', { expect_true(TRUE) })",
        "testthat::test_that('two', { expect_equal(1, 1) })",
        "otherpkg::test_that('not counted', { stop('no') })",
        "# test_that('commented', { stop('no') })",
        'string <- "test_that( not a call"',
        "test_that('three', { expect_false(FALSE) })"
      ),
      "tests/testthat/test-objects.R"
    )
    writeLines("helper_value <- 1", "tests/testthat/helper-objects.R")
    writeLines("# snapshot", "tests/testthat/_snaps/objects.md")

    result <- read_local_overview()
    source_objects <- result$source_objects
    objects <- source_objects$objects
    object_kinds <- set_names(
      map_chr(objects, "kind"),
      map_chr(objects, "name")
    )

    expect_equal(result$tests$frameworks, list("testthat"))
    expect_equal(result$tests$test_count, 3L)
    expect_equal(result$tests$snapshot_count, 1L)
    expect_equal(result$tests$helper_file_count, 1L)
    expect_equal(result$tests$fixture_dir_count, 1L)
    expect_equal(source_objects$counts$functions, 2L)
    expect_equal(source_objects$counts$r6_classes, 2L)
    expect_equal(source_objects$counts$other_objects, 1L)
    expect_equal(source_objects$total_objects, 5L)
    expect_equal(source_objects$listed_objects, 5L)
    expect_false(source_objects$objects_truncated)
    expect_equal(object_kinds[["fn_arrow"]], "function")
    expect_equal(object_kinds[["fn_equal"]], "function")
    expect_equal(object_kinds[["Thing"]], "r6_class")
    expect_equal(object_kinds[["ImportedThing"]], "r6_class")
    expect_equal(object_kinds[["value"]], "other_object")
    fn_arrow <- objects[[which(names(object_kinds) == "fn_arrow")]]
    expect_match(fn_arrow$location, "^R/objects[.]R:3-5$")
    expect_equal(fn_arrow$leading_comment_lines, "1-2")
    expect_equal(source_objects$parse_errors, list())
  })
})

test_that("package_overview strips backticks from source object names", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    writeLines(
      c(
        "`weird-name` <- function() NULL",
        "`%foo%` <- function(x, y) x"
      ),
      "R/non-syntactic.R"
    )

    result <- read_local_overview()
    object_names <- map_chr(result$source_objects$objects, "name")

    expect_setequal(object_names, c("weird-name", "%foo%"))
    expect_equal(result$source_objects$counts$functions, 2L)
  })
})

test_that("package_overview previews source objects while preserving full counts", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    writeLines(
      sprintf("fn_%02d <- function() NULL", 1:25),
      "R/many.R"
    )

    result <- read_local_overview()

    expect_equal(result$source_objects$counts$functions, 25L)
    expect_equal(result$source_objects$total_objects, 25L)
    expect_equal(result$source_objects$listed_objects, 20L)
    expect_true(result$source_objects$objects_truncated)
    expect_length(result$source_objects$objects, 20L)
  })
})

test_that("package_overview source objects use flat source scope", {
  withr::with_tempdir({
    write_description()
    dir.create("R/nested", recursive = TRUE)
    writeLines("upper <- function() NULL", "R/upper.R")
    writeLines("lower <- function() NULL", "R/lower.r")
    writeLines("nested <- function() NULL", "R/nested/nested.R")

    result <- read_local_overview()
    object_names <- map_chr(result$source_objects$objects, "name")

    expect_setequal(object_names, c("upper", "lower"))
    expect_equal(result$source_objects$total_objects, 2L)
  })
})

test_that("package_overview file summary count matches unfiltered non-git layout surface", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    dir.create(".git")
    dir.create("renv")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("kept", ".git/config")
    writeLines("kept", "renv/file.txt")

    result <- read_local_overview()

    layout_paths <- map_chr(result$files$layout, "path")
    expect_equal(result$files$summary$top_level_file_count, 1L)
    expect_equal(result$files$summary$top_level_dir_count, 3L)
    expect_equal(result$files$summary$total_file_count, 4L)
    expect_setequal(layout_paths, c(".git", "R", "renv"))
  })
})

test_that("package_overview ignores leading double-star directory patterns", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    write_description()
    dir.create("R")
    dir.create(".quarto")
    dir.create("nested/.quarto", recursive = TRUE)
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("ignored", ".quarto/root.txt")
    writeLines("ignored", "nested/.quarto/nested.txt")
    writeLines("kept", "nested/kept.txt")
    writeLines("**/.quarto/", ".gitignore")
    gert::git_init(".")

    result <- read_local_overview()
    layout_paths <- map_chr(result$files$layout, "path")
    nested_node <- result$files$layout[[which(layout_paths == "nested")]]
    nested_children <- map_chr(nested_node$children, "path")

    expect_equal(result$files$summary$top_level_file_count, 2L)
    expect_equal(result$files$summary$top_level_dir_count, 2L)
    expect_equal(result$files$summary$total_file_count, 4L)
    expect_setequal(layout_paths, c("R", "nested"))
    expect_false(".quarto" %in% layout_paths)
    expect_false("nested/.quarto" %in% nested_children)
    expect_equal(nested_node$recursive_file_count, 1L)
  })
})

test_that("package_overview applies gitignore to file layout and counts", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    write_description(suggests = "testthat")
    dir.create("R")
    dir.create("tests/testthat", recursive = TRUE)
    dir.create("ignored-dir")
    dir.create("nested/ignored-nested", recursive = TRUE)
    dir.create("analysis_files")
    dir.create("docs")
    dir.create("vignettes")
    writeLines("export(fn)", "NAMESPACE")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines(
      "test_that('works', { expect_true(TRUE) })",
      "tests/testthat/test-fn.R"
    )
    writeLines("ignored", "ignored-dir/file.txt")
    writeLines("ignored", "nested/ignored-nested/file.txt")
    writeLines("kept", "nested/kept.txt")
    writeLines("ignored", "analysis_files/artifact.txt")
    writeLines("ignored", "docs/index.html")
    writeLines("ignored", "report.html")
    writeLines("ignored", "vignettes/intro.qmd")
    writeLines(
      c(
        "ignored-dir/",
        "nested/ignored-nested/",
        "*_files/",
        "*.html",
        "/docs/",
        "/vignettes/",
        "NAMESPACE"
      ),
      ".gitignore"
    )
    gert::git_init(".")

    result <- read_local_overview()
    layout_paths <- map_chr(result$files$layout, "path")
    nested_node <- result$files$layout[[which(layout_paths == "nested")]]
    nested_children <- map_chr(nested_node$children, "path")

    expect_equal(result$files$summary$top_level_file_count, 2L)
    expect_equal(result$files$summary$top_level_dir_count, 3L)
    expect_equal(result$files$summary$total_file_count, 5L)
    expect_setequal(layout_paths, c("R", "nested", "tests"))
    expect_false("ignored-dir" %in% layout_paths)
    expect_false("analysis_files" %in% layout_paths)
    expect_false("docs" %in% layout_paths)
    expect_false("vignettes" %in% layout_paths)
    expect_false("nested/ignored-nested" %in% nested_children)
    expect_equal(nested_node$recursive_file_count, 1L)
    expect_true(result$files$has_package_files$NAMESPACE)
    expect_true(result$files$has_package_dirs$vignettes)
  })
})

test_that("package_overview applies repo-root gitignore from package subdirectory", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    dir.create("pkg")
    writeLines(c("pkg/*.html", "pkg/generated/"), ".gitignore")
    gert::git_init(".")

    withr::with_dir("pkg", {
      write_description()
      dir.create("R")
      dir.create("generated")
      writeLines("fn <- function() NULL", "R/fn.R")
      writeLines("ignored", "report.html")
      writeLines("ignored", "generated/file.txt")
      writeLines("kept", "kept.txt")

      result <- read_local_overview()
      layout_paths <- map_chr(result$files$layout, "path")

      expect_equal(result$files$summary$top_level_file_count, 2L)
      expect_equal(result$files$summary$top_level_dir_count, 1L)
      expect_equal(result$files$summary$total_file_count, 3L)
      expect_setequal(layout_paths, "R")
      expect_false("generated" %in% layout_paths)
      expect_false(".git" %in% layout_paths)
    })
  })
})

test_that("package_overview ignores .gitignore when not in a git repo", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    write_description()
    dir.create("R")
    dir.create("docs")
    dir.create("foo/renv", recursive = TRUE)
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("kept", "docs/index.html")
    writeLines("kept", "foo/kept.txt")
    writeLines("kept", "foo/renv/file.txt")
    writeLines("docs/", ".gitignore")

    result <- read_local_overview()
    layout_paths <- map_chr(result$files$layout, "path")
    foo_node <- result$files$layout[[which(layout_paths == "foo")]]
    foo_children <- map_chr(foo_node$children, "path")

    expect_equal(result$files$summary$top_level_file_count, 2L)
    expect_equal(result$files$summary$top_level_dir_count, 3L)
    expect_equal(result$files$summary$total_file_count, 6L)
    expect_setequal(layout_paths, c("R", "docs", "foo"))
    expect_equal(foo_children, "foo/renv")
    expect_equal(foo_node$recursive_file_count, 2L)
  })
})

test_that("package_overview exposes source parse errors", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    writeLines("ok <- function() NULL", "R/ok.R")
    writeLines("broken <- function(", "R/broken.R")

    result <- read_local_overview()

    expect_equal(result$source_objects$counts$functions, 1L)
    expect_length(result$source_objects$parse_errors, 1L)
    expect_equal(result$source_objects$parse_errors[[1L]]$path, "R/broken.R")
    expect_scalar_character(result$source_objects$parse_errors[[1L]]$error)
  })
})

test_that("package_overview detects bare R6Class imported after other symbols", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    writeLines("importFrom(R6, something, R6Class, another)", "NAMESPACE")
    writeLines(
      c(
        "Thing <- R6Class('Thing')",
        "BacktickedThing <- `R6Class`('BacktickedThing')",
        "NamespacedThing <- R6::`R6Class`('NamespacedThing')"
      ),
      "R/classes.R"
    )

    result <- read_local_overview()
    objects <- result$source_objects$objects
    object_kinds <- set_names(
      map_chr(objects, "kind"),
      map_chr(objects, "name")
    )

    expect_equal(object_kinds[["Thing"]], "r6_class")
    expect_equal(object_kinds[["BacktickedThing"]], "r6_class")
    expect_equal(object_kinds[["NamespacedThing"]], "r6_class")
    expect_equal(result$source_objects$counts$r6_classes, 3L)
  })
})

test_that("package_overview does not classify fake bare R6Class calls as R6 classes", {
  withr::with_tempdir({
    write_description()
    dir.create("R")
    writeLines("export(fake)", "NAMESPACE")
    writeLines(
      c(
        "R6Class <- function(...) list(...) ",
        "fake <- R6Class('Fake')",
        "real <- R6::R6Class('Real')"
      ),
      "R/classes.R"
    )

    result <- read_local_overview()
    objects <- result$source_objects$objects
    object_kinds <- set_names(
      map_chr(objects, "kind"),
      map_chr(objects, "name")
    )

    expect_equal(object_kinds[["R6Class"]], "function")
    expect_equal(object_kinds[["fake"]], "other_object")
    expect_equal(object_kinds[["real"]], "r6_class")
    expect_equal(result$source_objects$counts$r6_classes, 1L)
  })
})

test_that("package_overview returns structured error without DESCRIPTION", {
  withr::with_tempdir({
    result <- tool_result_json(
      rairai:::package_overview(),
      simplifyVector = FALSE
    )

    expect_equal(result$error$code, "description_not_found")
  })
})

test_that("package_overview returns structured error for malformed DESCRIPTION", {
  withr::with_tempdir({
    writeLines("Package fixture", "DESCRIPTION")

    result <- tool_result_json(
      rairai:::package_overview(),
      simplifyVector = FALSE
    )

    expect_equal(result$error$code, "description_malformed")
    expect_equal(result$error$path, "DESCRIPTION")
    expect_match(result$error$detail, "malformed")
  })
})

test_that("package_overview tool is in the served toolset", {
  tools <- rairai:::serve_tools(NULL)
  tool_names <- mcp_tool_names(tools)

  expect_true("package_overview" %in% tool_names)
})
