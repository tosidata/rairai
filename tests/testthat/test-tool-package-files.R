run_from_package_root <- function(expr) {
  pkg <- Sys.getenv("TESTTHAT_PKG", unset = "rairai")
  withr::with_dir(find.package(pkg), force(expr))
}

read_local_package_files <- function(...) {
  tool_result_json(
    rairai:::read_package_files(...),
    simplifyVector = FALSE
  )
}

package_file_paths <- function(result) {
  map_chr(result$files, "path")
}

test_that("read_package_files defaults to R scope", {
  response <- run_from_package_root(rairai:::read_package_files())
  expect_tool_value(response)
  result <- tool_result_json(response, simplifyVector = FALSE)

  expect_equal(result$scope, "R")
  expect_true(result$count >= 1)
  expect_true(all(map_lgl(
    result$files,
    function(file) str_starts(file$path, "R/")
  )))
})

test_that("read_package_files treats empty include values as defaults", {
  result <- run_from_package_root(
    tool_result_json(
      rairai:::read_package_files(include = c("")),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$scope, "R")
  expect_null(result$include)
})

test_that("read_package_files all scope covers the package file surface", {
  withr::with_tempdir({
    dir.create("R")
    dir.create("tests/testthat", recursive = TRUE)
    dir.create("man")
    dir.create("vignettes")
    dir.create("inst/extdata", recursive = TRUE)
    dir.create("dev")
    writeLines("Package: fixture", "DESCRIPTION")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("test_that('works', {})", "tests/testthat/test-fn.R")
    writeLines("\\name{fn}", "man/fn.Rd")
    writeLines("---\ntitle: Intro\n---", "vignettes/intro.qmd")
    writeLines("data", "inst/extdata/data.txt")
    writeLines("notes", "dev/notes.txt")
    writeLines("top-level", "README.md")

    result <- read_local_package_files(scope = "all")

    expect_equal(result$scope, "all")
    expect_setequal(
      package_file_paths(result),
      c(
        "DESCRIPTION",
        "README.md",
        "R/fn.R",
        "dev/notes.txt",
        "inst/extdata/data.txt",
        "man/fn.Rd",
        "tests/testthat/test-fn.R",
        "vignettes/intro.qmd"
      )
    )
    expect_equal(result$count, 8L)
  })
})

test_that("read_package_files applies gitignore filters in a git repo", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    dir.create("R")
    dir.create("generated")
    dir.create("nested/ignored", recursive = TRUE)
    writeLines("Package: fixture", "DESCRIPTION")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("ignored", "generated/file.txt")
    writeLines("ignored", "nested/ignored/file.txt")
    writeLines("ignored", "report.html")
    writeLines("kept", "nested/kept.txt")
    writeLines(c("generated/", "nested/ignored/", "*.html"), ".gitignore")
    gert::git_init(".")

    result <- read_local_package_files(scope = "all")

    expect_setequal(
      package_file_paths(result),
      c(".gitignore", "DESCRIPTION", "R/fn.R", "nested/kept.txt")
    )
    expect_equal(result$count, 4L)
  })
})

test_that("package_file_git_repo_root returns the checkout root", {
  skip_if_not_installed("gert")

  package_root <- run_from_package_root(
    normalizePath(".", winslash = "/", mustWork = TRUE)
  )
  skip_if(
    !file.exists(file.path(package_root, ".git")),
    "package root is not a git checkout"
  )

  expect_equal(
    rairai:::package_file_git_repo_root(package_root),
    package_root
  )
})

test_that("read_package_files applies repo-root gitignore from package subdirectory", {
  skip_if_not_installed("gert")

  withr::with_tempdir({
    dir.create("pkg")
    writeLines(c("pkg/*.html", "pkg/generated/"), ".gitignore")
    gert::git_init(".")

    withr::with_dir("pkg", {
      dir.create("R")
      dir.create("generated")
      writeLines("Package: fixture", "DESCRIPTION")
      writeLines("fn <- function() NULL", "R/fn.R")
      writeLines("ignored", "generated/file.txt")
      writeLines("ignored", "report.html")
      writeLines("kept", "kept.txt")

      result <- read_local_package_files(scope = "all")

      expect_setequal(
        package_file_paths(result),
        c("DESCRIPTION", "R/fn.R", "kept.txt")
      )
      expect_equal(result$count, 3L)
    })
  })
})

test_that("read_package_files keeps focused scopes focused", {
  withr::with_tempdir({
    dir.create("R")
    dir.create("man")
    dir.create("vignettes")
    dir.create("docs")
    writeLines("Package: fixture", "DESCRIPTION")
    writeLines("fn <- function() NULL", "R/fn.R")
    writeLines("helper <- function() NULL", "R/helper.R")
    writeLines("\\name{fn}", "man/fn.Rd")
    writeLines("---\ntitle: Intro\n---", "vignettes/intro.qmd")
    writeLines("site", "docs/index.html")
    writeLines("top-level", "README.md")

    default_result <- read_local_package_files()
    docs_result <- read_local_package_files(scope = "docs")

    expect_equal(default_result$scope, "R")
    expect_setequal(
      package_file_paths(default_result),
      c("R/fn.R", "R/helper.R")
    )
    expect_equal(docs_result$scope, "docs")
    expect_setequal(
      package_file_paths(docs_result),
      c("man/fn.Rd", "vignettes/intro.qmd")
    )
  })
})

test_that("read_package_files focused scopes ignore top-level file collisions", {
  withr::with_tempdir({
    writeLines("not a directory", "R")
    writeLines("not a directory", "tests")
    writeLines("not a directory", "man")
    writeLines("not a directory", "vignettes")

    r_result <- read_local_package_files(scope = "R")
    tests_result <- read_local_package_files(scope = "tests")
    docs_result <- read_local_package_files(scope = "docs")

    expect_equal(package_file_paths(r_result), character())
    expect_equal(package_file_paths(tests_result), character())
    expect_equal(package_file_paths(docs_result), character())
    expect_equal(r_result$count, 0L)
    expect_equal(tests_result$count, 0L)
    expect_equal(docs_result$count, 0L)
  })
})

test_that("read_package_files can include metadata fields", {
  result <- run_from_package_root(
    tool_result_json(
      rairai:::read_package_files(include = c("lines", "size", "modified")),
      simplifyVector = FALSE
    )
  )

  first_file <- result$files[[1]]

  expect_true(all(c("lines", "size", "modified") %in% names(first_file)))
})

test_that("read_package_files safely skips line counts for binary files", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(c("one", "two"), "R/text.R")
    writeBin(as.raw(c(0x41, 0x00, 0x42)), "R/blob.bin")

    result <- read_local_package_files(include = "lines")
    paths <- package_file_paths(result)
    files <- set_names(result$files, paths)

    expect_equal(files[["R/text.R"]]$lines, 2L)
    expect_false("lines" %in% names(files[["R/blob.bin"]]))
    expect_null(result$errors)
  })
})

test_that("package_file_count_lines preserves readLines line-count semantics", {
  withr::with_tempdir({
    cases <- list(
      empty = raw(),
      trailing_lf = charToRaw("one\ntwo\n"),
      no_trailing_lf = charToRaw("one\ntwo"),
      crlf = charToRaw("one\r\ntwo\r\n"),
      cr_only = charToRaw("one\rtwo\r"),
      mixed = charToRaw("one\r\ntwo\rthree\nfour")
    )

    for (name in names(cases)) {
      path <- paste0(name, ".txt")
      writeBin(cases[[name]], path)
      expect_equal(
        rairai:::package_file_count_lines(path, raw_count_threshold = 1L),
        length(readLines(path, warn = FALSE)),
        info = name
      )
    }
  })
})

test_that("package_file_count_raw_lines handles CRLF across chunk boundaries", {
  withr::with_tempdir({
    path <- "boundary.txt"
    writeBin(charToRaw("one\r\ntwo\r\nthree"), path)

    expect_equal(
      rairai:::package_file_count_raw_lines(path, chunk_size = 1L),
      length(readLines(path, warn = FALSE))
    )
    expect_equal(
      rairai:::package_file_count_raw_lines(path, chunk_size = 2L),
      length(readLines(path, warn = FALSE))
    )
  })
})

test_that("read_package_files reports line count read errors per file", {
  withr::with_tempdir({
    dir.create("R")
    writeLines(c("one", "two"), "R/text.R")
    writeLines("bad", "R/bad.R")

    testthat::local_mocked_bindings(
      package_file_read_lines = function(path) {
        if (identical(fs::path_file(path), "bad.R")) {
          stop("cannot read fixture")
        }
        readLines(path, warn = FALSE)
      },
      .package = "rairai"
    )

    result <- read_local_package_files(include = "lines")
    files <- set_names(result$files, package_file_paths(result))

    expect_equal(files[["R/text.R"]]$lines, 2L)
    expect_false("lines" %in% names(files[["R/bad.R"]]))
    expect_equal(result$errors[[1]]$path, "R/bad.R")
    expect_equal(result$errors[[1]]$field, "lines")
    expect_equal(result$errors[[1]]$code, "file_read_error")
    expect_match(result$errors[[1]]$detail, "cannot read fixture")
  })
})

test_that("read_package_files reports binary probe read errors per file", {
  withr::with_tempdir({
    dir.create("R")
    writeLines("bad", "R/bad.R")

    testthat::local_mocked_bindings(
      package_file_is_binary = function(path, n = 8192L) {
        stop("cannot probe fixture")
      },
      .package = "rairai"
    )

    result <- read_local_package_files(include = "lines")
    files <- set_names(result$files, package_file_paths(result))

    expect_false("lines" %in% names(files[["R/bad.R"]]))
    expect_equal(result$errors[[1]]$path, "R/bad.R")
    expect_equal(result$errors[[1]]$code, "file_read_error")
    expect_match(result$errors[[1]]$detail, "cannot probe fixture")
  })
})

test_that("read_package_files returns structured errors for invalid input", {
  expect_tool_error(run_from_package_root(
    rairai:::read_package_files(scope = "nope")
  ))
  invalid_scope <- tool_result_json(
    run_from_package_root(rairai:::read_package_files(scope = "nope")),
    simplifyVector = FALSE,
    error = TRUE
  )

  expect_equal(as.character(unlist(invalid_scope$error$code)), "invalid_scope")

  expect_tool_error(run_from_package_root(
    rairai:::read_package_files(include = c("nope"))
  ))
  invalid_include <- tool_result_json(
    run_from_package_root(rairai:::read_package_files(include = c("nope"))),
    simplifyVector = FALSE,
    error = TRUE
  )

  expect_equal(
    as.character(unlist(invalid_include$error$code)),
    "invalid_include"
  )
})
