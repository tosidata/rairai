run_from_package_root <- function(expr) {
  pkg <- Sys.getenv("TESTTHAT_PKG", unset = "rairai")
  withr::with_dir(find.package(pkg), force(expr))
}

read_dependencies_json <- function(depth = 1L) {
  run_from_package_root(
    tool_result_json(
      rairai:::read_package_dependencies(depth = depth),
      simplifyVector = FALSE
    )
  )
}

test_that("read_package_dependencies treats client-filled depth as default", {
  default_result <- read_dependencies_json()
  null_result <- read_dependencies_json(depth = NULL)
  zero_result <- read_dependencies_json(depth = 0L)

  expect_equal(null_result, default_result)
  expect_equal(zero_result, default_result)
})

test_that("read_package_dependencies returns concise direct dependency groups", {
  result <- read_dependencies_json()

  expect_named(
    result,
    c("dependencies", "remotes", "config_needs")
  )
  expect_named(
    result$dependencies,
    c("depends", "imports", "suggests", "enhances", "linkingto")
  )

  import_entries <- unlist(result$dependencies$imports, use.names = FALSE)

  expect_equal(result$dependencies$depends, list("R (>= 4.3.0)"))
  expect_contains(import_entries, "jsonlite")
  expect_contains(import_entries, "purrr")
  expect_contains(import_entries, "treesitter")
  expect_contains(import_entries, "treesitter.r")
  expect_match(
    import_entries,
    "^mcplite(?: \\([^()]+\\))?$",
    all = FALSE
  )
  expect_no_match(import_entries, "^ellmer(?: \\([^()]+\\))?$")
  expect_contains(result$dependencies$suggests, "testthat (>= 3.0.0)")
  expect_contains(result$dependencies$suggests, "withr")
  expect_equal(result$dependencies$enhances, list())
  expect_equal(result$dependencies$linkingto, list())
})

test_that("read_package_dependencies reports absent remotes and config needs", {
  result <- read_dependencies_json()

  expect_equal(result$remotes, list())
  expect_equal(result$config_needs, list())
})

test_that("read_package_dependencies parses remotes and config needs", {
  withr::with_tempdir({
    writeLines(
      c(
        "Package: fixture",
        "Remotes: owner/repo@main",
        "Config/Needs/website: pkgdown"
      ),
      "DESCRIPTION"
    )

    result <- tool_result_json(
      rairai:::read_package_dependencies(),
      simplifyVector = FALSE
    )

    expect_equal(
      result$remotes,
      list(list(remote = "owner/repo@main", package = "repo"))
    )
    expect_equal(result$config_needs, list(website = list("pkgdown")))
  })
})

test_that("read_package_dependencies reports missing DESCRIPTION", {
  withr::with_tempdir({
    result <- tool_result_json(
      rairai:::read_package_dependencies(),
      simplifyVector = FALSE
    )

    expect_equal(result$error$code, "description_not_found")
    expect_equal(result$error$message, "DESCRIPTION file not found.")
  })
})

test_that("read_package_dependencies reports malformed DESCRIPTION", {
  withr::with_tempdir({
    writeLines("Package fixture", "DESCRIPTION")

    result <- tool_result_json(
      rairai:::read_package_dependencies(),
      simplifyVector = FALSE
    )

    expect_equal(result$error$code, "description_malformed")
    expect_equal(result$error$path, "DESCRIPTION")
    expect_match(result$error$detail, "malformed")
  })
})

test_that("read_package_dependencies skips metadata without resolvable dependencies", {
  withr::with_tempdir({
    writeLines(
      c(
        "Package: fixture",
        "Depends: R (>= 4.2.0)"
      ),
      "DESCRIPTION"
    )

    testthat::local_mocked_bindings(
      dependency_metadata = function() {
        stop("metadata should not be read")
      },
      .package = "rairai"
    )

    result <- tool_result_json(
      rairai:::read_package_dependencies(depth = 2L),
      simplifyVector = FALSE
    )

    expect_equal(result$dependencies$depends, list("R (>= 4.2.0)"))
    expect_named(
      result$indirect_dependencies,
      c("depends", "imports", "suggests", "enhances", "linkingto")
    )
    expect_null(result$dependency_metadata_error)
  })
})

test_that("read_package_dependencies keeps direct data without metadata", {
  withr::with_tempdir({
    writeLines(
      c(
        "Package: fixture",
        "Imports: cli",
        "Suggests: testthat",
        "Remotes: owner/repo",
        "Config/Needs/website: pkgdown"
      ),
      "DESCRIPTION"
    )

    testthat::local_mocked_bindings(
      dependency_metadata = function() {
        stop("metadata failed")
      },
      .package = "rairai"
    )

    result <- tool_result_json(
      rairai:::read_package_dependencies(depth = 2L),
      simplifyVector = FALSE
    )

    expect_equal(result$dependencies$imports, list("cli"))
    expect_equal(result$remotes[[1]]$remote, "owner/repo")
    expect_equal(result$config_needs$website, list("pkgdown"))
    expect_named(
      result$indirect_dependencies,
      c("depends", "imports", "suggests", "enhances", "linkingto")
    )
    expect_true(all(lengths(result$indirect_dependencies) == 0L))
    expect_equal(
      result$dependency_metadata_error$code,
      "dependency_metadata_unavailable"
    )
    expect_match(result$dependency_metadata_error$detail, "metadata failed")
  })
})
