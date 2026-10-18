cran_json <- function(expr, error = FALSE) {
  tool_result_json(expr, simplifyVector = FALSE, error = error)
}

mock_cran_search_result <- function(total = 2L) {
  result <- data.frame(
    score = c(12.5, 7.25),
    package = c("glue", "epoxy"),
    version = I(list(numeric_version("1.8.0"), numeric_version("1.0.0"))),
    title = c("Interpreted String Literals", "String Interpolation"),
    description = c(
      "An implementation of interpreted string literals.",
      "String interpolation with additional helpers."
    ),
    date = as.POSIXct(
      c("2024-09-30 22:30:01", "2024-01-01 12:00:00"),
      tz = "UTC"
    ),
    downloads_last_month = c(12345L, 2345L),
    revdeps = c(100L, 10L),
    license = c("MIT + file LICENSE", "MIT"),
    url = c(
      "https://glue.tidyverse.org, https://github.com/tidyverse/glue",
      "https://example.com/epoxy"
    ),
    bugreports = c(
      "https://github.com/tidyverse/glue/issues",
      NA_character_
    )
  )
  attr(result, "metadata") <- list(
    query = "string interpolation",
    format = "long",
    from = 1L,
    size = nrow(result),
    server = "https://search.r-pkg.org",
    total = total,
    timed_out = FALSE
  )
  result
}

mock_cran_package <- function() {
  structure(
    list(
      Package = "glue",
      Title = "Interpreted String Literals",
      Version = "1.8.0",
      Description = "An implementation of interpreted string literals.",
      License = "MIT + file LICENSE",
      URL = "https://glue.tidyverse.org, https://github.com/tidyverse/glue",
      BugReports = "https://github.com/tidyverse/glue/issues",
      `Date/Publication` = "2024-09-30 22:30:01 UTC",
      `Authors@R` = "person('Jim', 'Hester', role = 'aut')",
      Author = "Jim Hester",
      Maintainer = "Jim Hester <jim@example.com>",
      Depends = list(R = ">= 3.6"),
      Imports = list(methods = "*", cli = ">= 3.6.0"),
      Suggests = list(testthat = ">= 3.0.0", waldo = ">=\n0.5.3"),
      Enhances = list(),
      LinkingTo = list()
    ),
    class = "cran_package"
  )
}

test_that("cran_search returns compact ranked candidates", {
  testthat::local_mocked_bindings(
    pkg_search = function(...) mock_cran_search_result(),
    .package = "pkgsearch"
  )

  result <- cran_json(rairai:::cran_search(" string interpolation "))

  expect_named(
    result,
    c(
      "query",
      "n_results",
      "source",
      "total",
      "returned",
      "timed_out",
      "results",
      "more"
    )
  )
  expect_equal(result$query, "string interpolation")
  expect_equal(result$n_results, 20L)
  expect_equal(result$source$backend, "pkgsearch")
  expect_equal(result$returned, 2L)
  expect_equal(result$results[[1]]$package, "glue")
  expect_equal(result$results[[1]]$rank, 1L)
  expect_equal(result$results[[1]]$version, "1.8.0")
  expect_equal(
    result$results[[1]]$description,
    "An implementation of interpreted string literals."
  )
  expect_equal(
    result$results[[1]]$url,
    "https://glue.tidyverse.org, https://github.com/tidyverse/glue"
  )
  expect_equal(result$more$tool, "cran_package")
})

test_that("cran_search normalizes client-filled n_results", {
  sizes <- list()
  testthat::local_mocked_bindings(
    pkg_search = function(query, format, size) {
      sizes[[length(sizes) + 1L]] <<- size
      mock_cran_search_result(total = 0L)[0, ]
    },
    .package = "pkgsearch"
  )

  cran_json(rairai:::cran_search("glue", n_results = NULL))
  cran_json(rairai:::cran_search("glue", n_results = 0L))
  cran_json(rairai:::cran_search("glue", n_results = 100L))

  expect_equal(sizes, list(20L, 20L, 50L))
})

test_that("CRAN ingress squishes text and keeps coercive limit defaults", {
  observed <- list()
  testthat::local_mocked_bindings(
    pkg_search = function(query, format, size) {
      observed[[length(observed) + 1L]] <<- list(query = query, size = size)
      mock_cran_search_result(total = 0L)[0, ]
    },
    .package = "pkgsearch"
  )
  limits <- list(NULL, integer(), NA_integer_, "bad", list("2", 9), 0, -1, 100)
  for (limit in limits) {
    cran_json(rairai:::cran_search(c("  linear  model  ", "unused"), limit))
  }
  expect_equal(map_chr(observed, "query"), rep("linear model", 8L))
  expect_equal(
    map_int(observed, "size"),
    c(20L, 20L, 20L, 20L, 2L, 20L, 20L, 50L)
  )
  for (query in list(NULL, character(), NA_character_, "  ")) {
    result <- cran_json(rairai:::cran_search(query), error = TRUE)
    expect_equal(result$error$code, "missing_query")
  }
})

test_that("cran_search reports missing query and backend failures", {
  expect_tool_error(rairai:::cran_search(""))
  expect_equal(
    cran_json(rairai:::cran_search(""), error = TRUE)$error$code,
    "missing_query"
  )

  testthat::local_mocked_bindings(
    pkg_search = function(...) stop("search unavailable"),
    .package = "pkgsearch"
  )

  expect_tool_error(rairai:::cran_search("glue"))
  result <- cran_json(rairai:::cran_search("glue"), error = TRUE)
  expect_equal(result$error$code, "cran_search_failed")
  expect_match(result$error$detail, "search unavailable")
})

test_that("cran_package returns package_overview-like description fields", {
  testthat::local_mocked_bindings(
    cran_package = function(package_name) {
      expect_equal(package_name, "glue")
      mock_cran_package()
    },
    .package = "pkgsearch"
  )
  testthat::local_mocked_bindings(
    cran_package_installed = function(package_name) {
      expect_equal(package_name, "glue")
      TRUE
    },
    .package = "rairai"
  )

  result <- cran_json(rairai:::cran_package("glue"))
  fields <- result$description$fields

  expect_named(result, "description")
  expect_named(result$description, c("fields", "more"))
  expect_equal(fields$Package, "glue")
  expect_equal(fields$Title, "Interpreted String Literals")
  expect_equal(
    fields$URL,
    "https://glue.tidyverse.org, https://github.com/tidyverse/glue"
  )
  expect_equal(fields$Depends, list("R (>= 3.6)"))
  expect_equal(fields$Imports, list("methods", "cli (>= 3.6.0)"))
  expect_equal(
    fields$Suggests,
    list(
      "testthat (>= 3.0.0)",
      "waldo (>= 0.5.3)"
    )
  )
  expect_equal(fields$Enhances, list())
  expect_equal(fields$LinkingTo, list())
  expect_true(result$description$more$installed)
  expect_equal(
    result$description$more$tools,
    list(
      "package_docs",
      "help_search",
      "help_page",
      "package_news",
      "package_vignettes"
    )
  )
})

test_that("cran_package does not suggest installed docs for uninstalled packages", {
  testthat::local_mocked_bindings(
    cran_package = function(package_name) mock_cran_package(),
    .package = "pkgsearch"
  )
  testthat::local_mocked_bindings(
    cran_package_installed = function(package_name) FALSE,
    .package = "rairai"
  )

  result <- cran_json(rairai:::cran_package("glue"))

  expect_false(result$description$more$installed)
  expect_null(result$description$more$tools)
  expect_match(result$description$more$reason, "not installed", fixed = TRUE)
})

test_that("cran_package reports missing name and lookup failures", {
  expect_tool_error(rairai:::cran_package(""))
  expect_equal(
    cran_json(rairai:::cran_package(""), error = TRUE)$error$code,
    "missing_package_name"
  )

  testthat::local_mocked_bindings(
    cran_package = function(...) stop("not found"),
    .package = "pkgsearch"
  )

  expect_tool_error(rairai:::cran_package("notapackage"))
  result <- cran_json(rairai:::cran_package("notapackage"), error = TRUE)
  expect_equal(result$error$code, "package_not_found")
  expect_match(result$error$detail, "not found")
})
