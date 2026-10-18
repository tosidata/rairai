local_containment_fixture <- function(.env = parent.frame()) {
  parent <- withr::local_tempdir(.local_envir = .env)
  root <- file.path(parent, "pkg")
  outside <- file.path(parent, "outside")
  fs::dir_create(c(
    file.path(root, "R", "nested"),
    file.path(root, "tests", "testthat", "nested"),
    outside
  ))
  writeLines(
    c(
      "Package: containmentFixture",
      "Title: Source Containment Fixture",
      "Version: 0.0.0.9000",
      "Description: Static source containment fixture.",
      "License: MIT",
      "Suggests: testthat"
    ),
    file.path(root, "DESCRIPTION")
  )
  writeLines("export(target)", file.path(root, "NAMESPACE"))
  writeLines(
    c(
      "# Target docs",
      "target <- function(x = 1) x",
      "Widget <- R6::R6Class('Widget', public = list(",
      "  run = function() target()",
      "))",
      "caller <- function(obj) obj$run()"
    ),
    file.path(root, "R", "core.R")
  )
  writeLines(
    c(
      "nested_only <- function() target()",
      "Nested <- R6::R6Class('Nested')",
      "obj$run()"
    ),
    file.path(root, "R", "nested", "ignored.R")
  )
  for (path in c(
    "tests/testthat/test-core.R",
    "tests/testthat/nested/test-nested.r",
    "tests/test-fallback.R"
  )) {
    writeLines(
      c(
        "# Test docs",
        "test_that('target works', {",
        "  target()",
        "  obj$run()",
        "})"
      ),
      file.path(root, path)
    )
  }
  list(
    root = normalizePath(root, winslash = "/", mustWork = TRUE),
    outside = normalizePath(outside, winslash = "/", mustWork = TRUE)
  )
}

containment_link <- function(target, path) {
  testthat::skip_if_not(
    suppressWarnings(file.symlink(target, path)),
    "Cannot create a symlink"
  )
}

containment_call <- function(service, call, package_root = NULL) {
  tool_result_json(
    rairai:::root_bound_tool_call(service, call, package_root = package_root),
    simplifyVector = FALSE
  )
}

local_containment_reads <- function(outside, .env = parent.frame()) {
  reads <- new.env(parent = emptyenv())
  reads$paths <- character()
  original <- base::readLines
  # Observe actual reads, not just returned facts. These consumers have no
  # shared read wrapper; restore this test-only base binding at scope exit.
  testthat::local_mocked_bindings(
    readLines = function(con = stdin(), ...) {
      if (is.character(con) && length(con) == 1L && file.exists(con)) {
        path <- normalizePath(con, winslash = "/", mustWork = TRUE)
        if (fs::path_has_parent(path, outside)) {
          reads$paths <- c(reads$paths, path)
        }
      }
      original(con, ...)
    },
    .package = "base",
    .env = .env
  )
  reads
}

test_that("unfiltered source tools do not read an outside-linked R directory", {
  fixture <- local_containment_fixture()
  source <- file.path(fixture$root, "R")
  escaped <- file.path(fixture$outside, "R")
  fs::file_move(source, escaped)
  containment_link(escaped, source)
  rairai:::reset_internal_cache()
  withr::defer(rairai:::reset_internal_cache())
  reads <- local_containment_reads(fixture$outside)
  service <- rairai:::PackageToolService$new(root_path = fixture$root)

  for (request in c("cold", "warm")) {
    symbols <- containment_call(service, \(s) s$symbols())
    expect_equal(symbols$package_root, fixture$root, info = request)
    expect_equal(symbols$count, 0L, info = request)
    expect_length(symbols$symbols, 0L)

    definition <- containment_call(service, \(s) s$definition("target"))
    references <- containment_call(service, \(s) s$references("target"))
    expect_equal(definition$status, "none", info = request)
    expect_equal(references$status, "none", info = request)

    classes <- containment_call(service, \(s) s$r6_classes())
    class <- containment_call(service, \(s) s$r6_class("Widget"))
    method <- containment_call(service, \(s) s$r6_method("Widget", "run"))
    expect_equal(classes$count, 0L, info = request)
    expect_equal(class$error$code, "class_not_found", info = request)
    expect_equal(method$error$code, "class_not_found", info = request)
    expect_equal(method$package_root, fixture$root, info = request)
    expect_null(method$source)

    r6_refs <- containment_call(service, function(s) {
      s$r6_method_references("Widget", "run")
    })
    expect_equal(r6_refs$target_resolution$class$status, "missing")
    expect_setequal(
      map_chr(r6_refs$references, "location"),
      c("tests/testthat/test-core.R:4", "tests/testthat/nested/test-nested.r:4")
    )
    overview <- containment_call(service, \(s) s$package_overview())
    expect_equal(overview$source_objects$total_objects, 0L, info = request)
    expect_length(overview$source_objects$objects, 0L)
    expect_equal(overview$tests$test_count, 2L, info = request)
    expect_equal(overview$package_root, fixture$root, info = request)
  }

  filtered <- containment_call(service, \(s) s$symbols(path = "R/core.R"))
  expect_equal(filtered$error$code, "path_outside_root")
  expect_equal(filtered$package_root, fixture$root)
  expect_length(reads$paths, 0L)
})

for (scope in c("testthat", "tests", "fallback")) {
  test_that(paste("discovered tests cannot escape through linked", scope), {
    fixture <- local_containment_fixture()
    if (identical(scope, "fallback")) {
      fs::dir_delete(file.path(fixture$root, "tests", "testthat"))
    }
    tests <- file.path(fixture$root, "tests")
    if (identical(scope, "testthat")) {
      tests <- file.path(tests, "testthat")
    }
    escaped <- file.path(fixture$outside, "linked-tests")
    fs::file_move(tests, escaped)
    containment_link(escaped, tests)
    reads <- local_containment_reads(fixture$outside)
    service <- rairai:::PackageToolService$new(root_path = fixture$root)

    references <- containment_call(service, \(s) s$references("target"))
    expect_equal(references$status, "unique")
    expect_equal(
      map_chr(references$references, "location"),
      "R/core.R:4:20-4:28"
    )
    r6_refs <- containment_call(service, function(s) {
      s$r6_method_references("Widget", "run")
    })
    expect_equal(r6_refs$target_resolution$class$status, "unique")
    expect_equal(map_chr(r6_refs$references, "location"), "R/core.R:6")
    related <- containment_call(service, \(s) s$tests_for("target"))
    expect_equal(related$count, 0L)
    expect_length(related$matches, 0L)
    refactor <- containment_call(service, \(s) s$refactor_context("target"))
    expect_equal(refactor$related_test_count, 0L)
    expect_length(refactor$related_tests, 0L)
    expect_equal(refactor$test_text_hit_count, 0L)
    overview <- containment_call(service, \(s) s$package_overview())
    expect_equal(overview$tests$test_count, 0L)
    expect_equal(overview$source_objects$total_objects, 3L)
    expect_equal(related$package_root, fixture$root)
    expect_equal(refactor$package_root, fixture$root)
    expect_equal(overview$package_root, fixture$root)
    expect_length(reads$paths, 0L)
  })
}

for (linked in c(FALSE, TRUE)) {
  test_that(
    paste(
      "ordinary source facts preserve scopes and aliases; linked =",
      linked
    ),
    {
      fixture <- local_containment_fixture()
      if (linked) {
        for (path in c("R", "tests/testthat")) {
          alias <- file.path(fixture$root, path)
          target <- file.path(fixture$root, paste0(basename(path), "-storage"))
          fs::file_move(alias, target)
          containment_link(target, alias)
        }
      }
      service <- rairai:::PackageToolService$new(root_path = fixture$root)
      symbols <- containment_call(service, \(s) s$symbols(kind = "Function"))
      expect_equal(map_chr(symbols$symbols, "name"), c("target", "caller"))
      expect_equal(
        map_chr(symbols$symbols, "location"),
        c("R/core.R:2:1-2:28", "R/core.R:6:1-6:34")
      )
      expect_equal(symbols$symbols[[1L]]$leading_comment_lines, "1")
      expect_equal(symbols$package_root, fixture$root)
      references <- containment_call(service, \(s) s$references("target"))
      expect_equal(
        map_chr(references$references, "location"),
        c("R/core.R:4:20-4:28", "tests/testthat/test-core.R:3:3-3:11")
      )
      classes <- containment_call(service, \(s) s$r6_classes())
      expect_equal(map_chr(classes$classes, "name"), "Widget")
      expect_equal(classes$classes[[1L]]$location, "R/core.R:3-5")
      method <- containment_call(service, \(s) s$r6_method("Widget", "run"))
      expect_equal(method$source, "run = function() target()")
      expect_equal(method$location, "R/core.R:4")
      r6_refs <- containment_call(service, function(s) {
        s$r6_method_references("Widget", "run")
      })
      expect_setequal(
        map_chr(r6_refs$references, "location"),
        c(
          "R/core.R:6",
          "tests/testthat/test-core.R:4",
          "tests/testthat/nested/test-nested.r:4"
        )
      )
      related <- containment_call(service, \(s) s$tests_for("target"))
      expect_setequal(
        map_chr(related$matches, "location"),
        c(
          "tests/testthat/test-core.R:2-5",
          "tests/testthat/nested/test-nested.r:2-5"
        )
      )
      expect_equal(
        map_chr(related$matches, "leading_comment_lines"),
        c("1", "1")
      )
      overview <- containment_call(service, \(s) s$package_overview())
      expect_equal(overview$tests$test_count, 2L)
      expect_equal(
        map_chr(overview$source_objects$objects, "location"),
        c("R/core.R:2", "R/core.R:3-5", "R/core.R:6")
      )
      expect_equal(
        overview$source_objects$objects[[1L]]$leading_comment_lines,
        "1"
      )
    }
  )
}

test_that("test fallback preserves each consumer's recursion scope", {
  fixture <- local_containment_fixture()
  fs::dir_delete(file.path(fixture$root, "tests", "testthat"))
  fs::dir_create(file.path(fixture$root, "tests", "nested"))
  fs::file_copy(
    file.path(fixture$root, "tests", "test-fallback.R"),
    file.path(fixture$root, "tests", "nested", "test-deep.r")
  )
  service <- rairai:::PackageToolService$new(root_path = fixture$root)
  references <- containment_call(service, \(s) s$references("target"))
  expect_equal(map_chr(references$references, "location"), "R/core.R:4:20-4:28")
  related <- containment_call(service, \(s) s$tests_for("target"))
  expect_setequal(
    map_chr(related$matches, "location"),
    c("tests/test-fallback.R:2-5", "tests/nested/test-deep.r:2-5")
  )
  r6_refs <- containment_call(service, function(s) {
    s$r6_method_references("Widget", "run")
  })
  expect_setequal(
    map_chr(r6_refs$references, "location"),
    c("R/core.R:6", "tests/test-fallback.R:4", "tests/nested/test-deep.r:4")
  )
  overview <- containment_call(service, \(s) s$package_overview())
  expect_equal(overview$tests$test_count, 0L)
})

test_that("startup and per-call root aliases retain canonical provenance", {
  fixture <- local_containment_fixture()
  alias <- file.path(dirname(fixture$root), "pkg-alias")
  containment_link(fixture$root, alias)
  startup_root <- rairai:::normalize_serve_path(alias)
  expect_equal(startup_root, fixture$root)
  startup <- rairai:::PackageToolService$new(root_path = startup_root)
  from_startup <- containment_call(startup, \(s) s$symbols(query = "target"))
  expect_equal(from_startup$package_root, fixture$root)
  expect_equal(from_startup$symbols[[1L]]$location, "R/core.R:2:1-2:28")

  default <- rairai:::PackageToolService$new(root_path = fixture$outside)
  selected <- containment_call(default, \(s) s$symbols(query = "target"), alias)
  expect_equal(selected, from_startup)
  after <- containment_call(default, \(s) s$symbols())
  expect_equal(after$package_root, fixture$outside)
  expect_equal(after$count, 0L)
  expect_equal(default$root_path, fixture$outside)
})

for (transition in c("retarget", "replacement")) {
  test_that(paste("warm source and test caches reject directory", transition), {
    fixture <- local_containment_fixture()
    rairai:::reset_internal_cache()
    withr::defer(rairai:::reset_internal_cache())
    paths <- c("R", "tests/testthat")
    for (path in paths) {
      original <- file.path(fixture$root, path)
      copy <- file.path(fixture$outside, basename(path))
      fs::dir_copy(original, copy)
      if (identical(transition, "retarget")) {
        stored <- file.path(fixture$root, paste0(basename(path), "-storage"))
        fs::file_move(original, stored)
        containment_link(stored, original)
      }
    }
    # Identical size/mtime must not authorize cache reuse after a path escapes.
    for (path in c("R/core.R", "tests/testthat/test-core.R")) {
      original <- file.path(fixture$root, path)
      copy <- file.path(
        fixture$outside,
        basename(dirname(path)),
        basename(path)
      )
      stamp <- as.POSIXct("2026-01-01", tz = "UTC")
      Sys.setFileTime(original, stamp)
      Sys.setFileTime(copy, stamp)
      expect_equal(file.info(original)$size, file.info(copy)$size)
      expect_equal(file.info(original)$mtime, file.info(copy)$mtime)
    }
    service <- rairai:::PackageToolService$new(root_path = fixture$root)
    cold <- containment_call(service, \(s) s$references("target"))
    warm <- containment_call(service, \(s) s$references("target"))
    expect_equal(cold$count, 2L)
    expect_equal(warm, cold)
    reads <- local_containment_reads(fixture$outside)

    # Changes happen only between completed requests, never inside a read.
    for (path in rev(paths)) {
      alias <- file.path(fixture$root, path)
      if (identical(transition, "retarget")) {
        expect_equal(unlink(alias), 0L)
      } else {
        fs::file_move(
          alias,
          file.path(fixture$root, paste0(basename(path), "-storage"))
        )
      }
      containment_link(file.path(fixture$outside, basename(path)), alias)
      if (identical(path, "tests/testthat")) {
        references <- containment_call(service, \(s) s$references("target"))
        expect_equal(
          map_chr(references$references, "location"),
          "R/core.R:4:20-4:28"
        )
        related <- containment_call(service, \(s) s$refactor_context("target"))
        expect_equal(related$related_test_count, 0L)
        expect_equal(related$test_text_hit_count, 0L)
      }
    }
    for (current in list(
      service,
      rairai:::PackageToolService$new(root_path = fixture$root)
    )) {
      symbols <- containment_call(current, \(s) s$symbols())
      expect_equal(symbols$count, 0L)
      expect_equal(symbols$package_root, fixture$root)
      method <- containment_call(current, \(s) s$r6_method("Widget", "run"))
      expect_equal(method$error$code, "class_not_found")
      expect_null(method$source)
      overview <- containment_call(current, \(s) s$package_overview())
      expect_equal(overview$source_objects$total_objects, 0L)
      expect_equal(overview$tests$test_count, 0L)
    }
    expect_length(reads$paths, 0L)

    for (path in paths) {
      alias <- file.path(fixture$root, path)
      expect_equal(unlink(alias), 0L)
      containment_link(
        file.path(fixture$root, paste0(basename(path), "-storage")),
        alias
      )
    }
    restored <- containment_call(service, \(s) s$references("target"))
    expect_equal(restored, cold)
  })
}

for (replaced in c("root", "ancestor")) {
  test_that(paste("bound source authority survives replacement of", replaced), {
    fixture <- local_containment_fixture()
    ancestor <- file.path(dirname(fixture$root), "bound")
    fs::dir_create(ancestor)
    root <- file.path(ancestor, "pkg")
    fs::file_move(fixture$root, root)
    root <- normalizePath(root, winslash = "/", mustWork = TRUE)
    escaped <- file.path(fixture$outside, "pkg")
    fs::dir_copy(root, escaped)
    for (path in c("R/core.R", "tests/testthat/test-core.R")) {
      stamp <- as.POSIXct("2026-01-01", tz = "UTC")
      Sys.setFileTime(file.path(root, path), stamp)
      Sys.setFileTime(file.path(escaped, path), stamp)
      expect_equal(
        file.info(file.path(root, path))$size,
        file.info(file.path(escaped, path))$size
      )
      expect_equal(
        file.info(file.path(root, path))$mtime,
        file.info(file.path(escaped, path))$mtime
      )
    }
    rairai:::reset_internal_cache()
    withr::defer(rairai:::reset_internal_cache())
    service <- rairai:::PackageToolService$new(root_path = root)
    before <- containment_call(service, \(s) s$references("target"))
    expect_equal(before$count, 2L)
    expect_equal(containment_call(service, \(s) s$references("target")), before)
    class <- rairai:::r6_syntax_parse_file(root, "R/core.R")$classes[[1L]]
    relative_root <- as.character(fs::path_rel(root))
    expect_equal(
      rairai:::contained_source_path("R/core.R", relative_root),
      file.path(root, "R/core.R")
    )

    # Replace only between completed requests; retain the same bound service.
    original <- if (identical(replaced, "root")) root else ancestor
    target <- if (identical(replaced, "root")) escaped else fixture$outside
    stored <- file.path(dirname(fixture$outside), "saved")
    fs::file_move(original, stored)
    containment_link(target, original)
    reads <- local_containment_reads(fixture$outside)
    for (request in c("warm", "cold")) {
      if (identical(request, "cold")) {
        rairai:::reset_internal_cache()
      }
      symbols <- containment_call(service, \(s) s$symbols())
      expect_equal(symbols$count, 0L, info = request)
      expect_equal(symbols$package_root, root)
      expect_equal(service$root_path, root)
      expect_equal(service$source_index$root_path, root)
      references <- containment_call(service, \(s) s$references("target"))
      expect_equal(references$status, "none")
      classes <- containment_call(service, \(s) s$r6_classes())
      expect_equal(classes$count, 0L)
      method <- containment_call(service, \(s) s$r6_method("Widget", "run"))
      expect_equal(method$error$code, "class_not_found")
      expect_null(method$source)
      related <- containment_call(service, \(s) s$tests_for("target"))
      expect_equal(related$count, 0L)
      r6_refs <- containment_call(service, function(s) {
        s$r6_method_references("Widget", "run")
      })
      expect_equal(r6_refs$count, 0L)
      expect_equal(r6_refs$target_resolution$class$status, "missing")
      expect_null(rairai:::contained_source_path("R/core.R", relative_root))
      saved <- rairai:::r6_method_match_record(class, "run", root)
      expect_null(saved$source)
      expect_equal(saved$error$code, "method_source_unavailable")
      comment <- rairai:::compact_location_fields(
        list(path = "R/core.R", line = 2L, end_line = 2L),
        include_leading_comment = TRUE,
        source_root = root
      )
      expect_null(comment$leading_comment_lines)
    }
    expect_length(reads$paths, 0L)

    expect_equal(unlink(original), 0L)
    fs::file_move(stored, original)
    expect_equal(containment_call(service, \(s) s$references("target")), before)
  })
}

test_that("in-root retargeting refreshes facts with equal size and mtime", {
  fixture <- local_containment_fixture()
  rairai:::reset_internal_cache()
  withr::defer(rairai:::reset_internal_cache())
  alias <- file.path(fixture$root, "R")
  first <- file.path(fixture$root, "first-source")
  second <- file.path(fixture$root, "second-source")
  fs::file_move(alias, first)
  fs::dir_copy(first, second)
  containment_link(first, alias)
  first_file <- file.path(first, "core.R")
  second_file <- file.path(second, "core.R")
  writeLines(
    str_replace(readLines(first_file), fixed("x = 1"), "x = 2"),
    second_file
  )
  stamp <- as.POSIXct("2026-01-01", tz = "UTC")
  Sys.setFileTime(first_file, stamp)
  Sys.setFileTime(second_file, stamp)
  expect_equal(file.info(first_file)$size, file.info(second_file)$size)
  expect_equal(file.info(first_file)$mtime, file.info(second_file)$mtime)
  service <- rairai:::PackageToolService$new(root_path = fixture$root)
  before <- containment_call(service, \(s) s$function_formals("target"))
  expect_equal(before$signatures[[1L]]$label, "target(x = 1)")
  expect_equal(
    containment_call(service, \(s) s$function_formals("target")),
    before
  )

  expect_equal(unlink(alias), 0L)
  containment_link(second, alias)
  after <- containment_call(service, \(s) s$function_formals("target"))
  expect_equal(after$signatures[[1L]]$label, "target(x = 2)")
  expect_equal(after$resolved$location, "R/core.R:2:1-2:28")
  expect_equal(after$package_root, fixture$root)
})

test_that("saved records cannot reread escaped source or comments", {
  fixture <- local_containment_fixture()
  parsed <- rairai:::r6_syntax_parse_file(fixture$root, "R/core.R")
  class <- parsed$classes[[1L]]
  record <- list(path = "R/core.R", line = 2L, end_line = 2L)
  before <- rairai:::compact_location_fields(
    record,
    include_leading_comment = TRUE,
    source_root = fixture$root
  )
  expect_equal(before$leading_comment_lines, "1")
  expect_equal(
    rairai:::r6_method_match_record(class, "run", fixture$root)$source,
    "run = function() target()"
  )

  source <- file.path(fixture$root, "R", "core.R")
  escaped <- file.path(fixture$outside, "core.R")
  fs::file_move(source, escaped)
  containment_link(escaped, source)
  reads <- local_containment_reads(fixture$outside)
  # A saved record is reused after a completed index call, not a simulated race.
  for (path in c("R/core.R", escaped)) {
    record$path <- path
    after <- rairai:::compact_location_fields(
      record,
      include_leading_comment = TRUE,
      source_root = fixture$root
    )
    expect_null(after$leading_comment_lines)
    expect_length(
      rairai:::read_source_lines(rairai:::source_path_for_record(
        path,
        fixture$root
      )),
      0L
    )
  }
  method <- rairai:::r6_method_match_record(class, "run", fixture$root)
  expect_null(method$source)
  expect_equal(method$error$code, "method_source_unavailable")
  reparsed <- rairai:::r6_syntax_parse_file(fixture$root, "R/core.R")
  expect_length(reparsed$lines, 0L)
  expect_null(reparsed$root)
  expect_length(reparsed$classes, 0L)
  expect_length(reparsed$member_references, 0L)
  expect_length(reads$paths, 0L)
})
