run_from_tests_root <- function(expr) {
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")),
    "Not in source tree"
  )
  withr::with_dir(root, force(expr))
}

expect_source_or_installed_package <- function(source_value) {
  testthat::expect_match(source_value, "^(local|installed)$")
}

read_help_page <- function(...) {
  tool_result_json(
    rairai:::help_page(...),
    simplifyVector = FALSE
  )
}

read_help_search <- function(...) {
  tool_result_json(
    rairai:::help_search(...),
    simplifyVector = FALSE
  )
}

expect_help_search_match_is_compact <- function(match) {
  expect_true(all(
    c(
      "rank",
      "topic",
      "package",
      "title",
      "source",
      "score",
      "matched",
      "next_tools"
    ) %in%
      names(match)
  ))
  expect_false("text" %in% names(match))
  expect_type(match$topic, "character")
  expect_type(match$package, "character")
  expect_type(match$title, "character")
  expect_source_or_installed_package(match$source)
  expect_true(is.numeric(match$score) || is.integer(match$score))
  expect_gt(length(match$matched), 0L)
  for (matched in match$matched) {
    expect_true(all(c("field", "type") %in% names(matched)))
  }
  expect_equal(match$next_tools[[1L]]$tool, "help_page")
  expect_equal(match$next_tools[[1L]]$arguments$topic, match$topic)
  expect_equal(match$next_tools[[1L]]$arguments$package, match$package)
}

# help_page tests

test_that("help_page returns text and metadata for in-source topic", {
  result <- run_from_tests_root(read_help_page("serve"))

  expect_equal(result$topic, "serve")
  expect_equal(result$package, "rairai")
  expect_equal(result$source, "local")
  expect_equal(result$title, "Serve the MCP tools")
  expect_true(nchar(result$text) > 0)
  expect_true(str_detect(result$text, regex("serve", ignore_case = TRUE)))
  expect_true(is.list(result$sections) || is.character(result$sections))
  expect_true("Description" %in% unlist(result$sections, use.names = FALSE))
  expect_equal(result$requested_sections, list())
  expect_setequal(
    unlist(result$matched_sections, use.names = FALSE),
    unlist(result$sections, use.names = FALSE)
  )
  expect_equal(result$missing_sections, list())
  expect_false(result$truncated)
})

test_that("help_page returns text and metadata for installed package topic", {
  result <- read_help_page("lm", package = "stats")

  expect_equal(result$topic, "lm")
  expect_equal(result$package, "stats")
  expect_equal(result$source, "installed")
  expect_type(result$title, "character")
  expect_true(nchar(result$text) > 0)
  expect_true(length(result$sections) > 0)
  expect_equal(result$requested_sections, list())
  expect_setequal(
    unlist(result$matched_sections, use.names = FALSE),
    unlist(result$sections, use.names = FALSE)
  )
  expect_equal(result$missing_sections, list())
  expect_false(result$truncated)
})

test_that("help_page falls back to installed package when topic not in source", {
  result <- run_from_tests_root(read_help_page("lm"))

  expect_equal(result$topic, "lm")
  expect_equal(result$package, "stats")
  expect_equal(result$source, "installed")
  expect_true(nchar(result$text) > 0)
})

test_that("help_page sections filters to requested sections only", {
  result <- read_help_page(
    "lm",
    package = "stats",
    sections = list("Description")
  )

  expect_equal(result$requested_sections, list("Description"))
  expect_equal(result$matched_sections, list("Description"))
  expect_equal(result$missing_sections, list())
  expect_true(str_detect(result$text, fixed("Description:")))
  expect_false(str_detect(result$text, fixed("Examples:")))
})

test_that("help_page sections filtering is case insensitive", {
  lower_result <- read_help_page(
    "lm",
    package = "stats",
    sections = list("description")
  )
  upper_result <- read_help_page(
    "lm",
    package = "stats",
    sections = list("Description")
  )

  expect_equal(lower_result$text, upper_result$text)
  expect_equal(lower_result$requested_sections, list("description"))
  expect_equal(lower_result$matched_sections, list("Description"))
  expect_true(str_detect(lower_result$text, fixed("Description:")))
})

test_that("help_page reports missing requested sections", {
  result <- read_help_page(
    "lm",
    package = "stats",
    sections = list("Description", "NonexistentSection123")
  )

  expect_equal(
    result$requested_sections,
    list("Description", "NonexistentSection123")
  )
  expect_equal(result$matched_sections, list("Description"))
  expect_equal(result$missing_sections, list("NonexistentSection123"))
  expect_true(str_detect(result$text, fixed("Description:")))
  expect_true("Description" %in% unlist(result$sections, use.names = FALSE))
})

test_that("help_page returns empty text when requested sections are all missing", {
  result <- read_help_page(
    "lm",
    package = "stats",
    sections = list("NonexistentSection123")
  )

  expect_equal(result$requested_sections, list("NonexistentSection123"))
  expect_equal(result$matched_sections, list())
  expect_equal(result$missing_sections, list("NonexistentSection123"))
  expect_equal(result$text, "")
})

test_that("help_page max_chars truncates text", {
  result <- read_help_page("lm", package = "stats", max_chars = 50L)

  expect_lte(nchar(result$text), 50L)
  expect_true(isTRUE(result$truncated))
})

test_that("help_page does not truncate when max_chars exceeds text length", {
  result <- run_from_tests_root(read_help_page("serve", max_chars = 1e6L))

  expect_false(result$truncated)
})

test_that("help_page returns error for missing topic", {
  result <- read_help_page("")

  expect_equal(as.character(result$error$code), "missing_topic")
})

test_that("help_page returns error for unknown topic", {
  result <- read_help_page("zzz_nonexistent_topic_xyz_abc")

  expect_equal(as.character(result$error$code), "not_found")
})

test_that("help_page returns error for unknown requested package", {
  package <- "zzz_nonexistent_pkg_xyz_abc"
  result <- read_help_page("lm", package = package)

  expect_package_not_found_guidance(result$error, package)
  expect_equal(result$package, package)
})

test_that("help_page reports ambiguous unscoped installed topics", {
  result <- read_help_page("set_names")

  expect_equal(as.character(result$error$code), "ambiguous_topic")
  candidate_packages <- map_chr(result$error$candidates, "package")
  expect_true(all(c("purrr", "rlang") %in% candidate_packages))

  for (candidate in result$error$candidates) {
    expect_true(all(
      c("topic", "package", "title", "source") %in% names(candidate)
    ))
    expect_false("text" %in% names(candidate))
    expect_false("sections" %in% names(candidate))
    expect_equal(candidate$source, "installed")
  }
})

test_that("help_page treats NULL and empty package as unspecified", {
  null_result <- run_from_tests_root(read_help_page("serve", package = NULL))
  empty_result <- run_from_tests_root(read_help_page("serve", package = ""))

  expect_source_or_installed_package(null_result$source)
  expect_source_or_installed_package(empty_result$source)
  expect_equal(null_result$package, "rairai")
  expect_equal(empty_result$package, "rairai")
})

# help_search tests

test_that("help search defaults to mixed lookup while docs default to local", {
  root <- withr::local_tempdir()
  writeLines("Package: DocFixture", file.path(root, "DESCRIPTION"))
  dir.create(file.path(root, "man"))
  writeLines(
    c(
      "\\name{alpha}",
      "\\alias{alpha}",
      "\\title{Alpha topic}",
      "\\description{Local alpha documentation.}"
    ),
    file.path(root, "man", "alpha.Rd")
  )
  testthat::local_mocked_bindings(
    installed_help_search_topics = function(query) {
      list(list(
        topic = "alpha",
        package = "installedfixture",
        title = "Alpha topic",
        description = "Installed alpha documentation.",
        aliases = "alpha",
        source = "installed"
      ))
    },
    .package = "rairai"
  )

  mixed <- read_help_search("alpha", root_path = root)
  expect_equal(mixed$search_scope, "local_plus_installed")
  expect_equal(map_chr(mixed$matches, "source"), c("local", "installed"))
  for (package in list(NULL, "", "  ", character())) {
    expect_equal(read_help_search("alpha", package, root_path = root), mixed)
    docs <- tool_result_json(
      rairai:::package_docs(package, root_path = root),
      simplifyVector = FALSE
    )
    expect_equal(docs$source, "local")
    expect_equal(docs$package, "DocFixture")
    expect_equal(map_chr(docs$help$topics, "topic"), "alpha")
  }

  scoped <- read_help_search("alpha", "DOCFIXTURE", root_path = root)
  expect_equal(scoped$package, "DOCFIXTURE")
  expect_equal(scoped$search_scope, "local_package")
  expect_equal(scoped$matches, mixed$matches[1L])
  expect_equal(
    read_help_page("alpha", "docfixture", root_path = root),
    read_help_page("alpha", root_path = root)
  )

  unlink(file.path(root, "man"), recursive = TRUE)
  unavailable <- read_help_search("alpha", root_path = root)
  expect_equal(unavailable$search_scope, "installed")
  expect_equal(unavailable$matches[[1L]]$source, "installed")
  expect_equal(unavailable$limitations[[1L]]$code, "local_rd_missing")
})

test_that("help_search returns ranked scoped matches for a valid query", {
  result <- run_from_tests_root(read_help_search("serve", package = "rairai"))

  expect_equal(result$query, "serve")
  expect_equal(result$package, "rairai")
  expect_equal(result$search_scope, "local_package")
  expect_gte(result$total_count, 1L)
  expect_gte(result$count, 1L)
  expect_lte(result$count, result$limit)
  expect_equal(result$offset, 0L)
  expect_equal(result$limit, 20L)

  first <- result$matches[[1L]]
  expect_equal(first$rank, 1L)
  expect_equal(first$topic, "serve")
  expect_equal(first$package, "rairai")
  expect_equal(first$source, "local")
  expect_help_search_match_is_compact(first)
})

test_that("help_search ranks exact topic matches above weaker matches", {
  result <- read_help_search("lm", package = "stats", limit = 5L)

  expect_equal(result$package, "stats")
  expect_equal(result$search_scope, "package")
  expect_gt(result$total_count, 0L)
  expect_equal(result$matches[[1L]]$topic, "lm")
  expect_equal(result$matches[[1L]]$rank, 1L)

  matched <- result$matches[[1L]]$matched
  matched_fields <- map_chr(matched, "field")
  matched_types <- map_chr(matched, "type")
  expect_true(any(matched_fields == "topic" & matched_types == "exact"))
})

test_that("help_search scoring preserves ranking tiers", {
  topic <- function(
    topic,
    aliases = character(),
    title = "",
    description = "",
    source = "installed"
  ) {
    list(
      topic = topic,
      package = "pkg",
      title = title,
      description = description,
      aliases = aliases,
      source = source
    )
  }

  matches <- list(
    exact_topic = rairai:::help_search_match(topic("alpha"), "alpha"),
    exact_alias = rairai:::help_search_match(
      topic("beta", aliases = "alpha"),
      "alpha"
    ),
    topic_prefix = rairai:::help_search_match(topic("alphabet"), "alpha"),
    alias_prefix = rairai:::help_search_match(
      topic("gamma", aliases = "alphabet"),
      "alpha"
    ),
    title_words = rairai:::help_search_match(
      topic("delta", title = "Alpha topic"),
      "alpha"
    ),
    title_substring = rairai:::help_search_match(
      topic("epsilon", title = "Alphabetical topic"),
      "alpha"
    ),
    description_words = rairai:::help_search_match(
      topic("zeta", description = "Alpha topic"),
      "alpha"
    ),
    local_title_words = rairai:::help_search_match(
      topic("eta", title = "Alpha topic", source = "local"),
      "alpha"
    )
  )

  scores <- map_int(matches, "score")
  expect_gt(scores[["exact_topic"]], scores[["exact_alias"]])
  expect_gt(scores[["exact_alias"]], scores[["topic_prefix"]])
  expect_gt(scores[["topic_prefix"]], scores[["alias_prefix"]])
  expect_gt(scores[["alias_prefix"]], scores[["title_words"]])
  expect_gt(scores[["title_words"]], scores[["title_substring"]])
  expect_gt(scores[["title_substring"]], scores[["description_words"]])
  expect_gt(scores[["local_title_words"]], scores[["title_words"]])
})

test_that("help_search result metadata explains matches without long text", {
  result <- run_from_tests_root(read_help_search(
    "MCP tools",
    package = "rairai"
  ))

  expect_gt(result$count, 0L)
  for (match in result$matches) {
    expect_help_search_match_is_compact(match)
  }
})

test_that("help_search returns empty matches for unknown scoped query", {
  result <- run_from_tests_root(
    read_help_search("zzz_nonexistent_query_xyz_abc", package = "rairai")
  )

  expect_equal(result$query, "zzz_nonexistent_query_xyz_abc")
  expect_equal(result$package, "rairai")
  expect_equal(result$search_scope, "local_package")
  expect_equal(result$total_count, 0L)
  expect_equal(result$count, 0L)
  expect_equal(result$offset, 0L)
  expect_equal(result$limit, 20L)
  expect_false(result$has_more)
  expect_equal(result$matches, list())
  expect_true("next_tools" %in% names(result))

  next_tool_names <- map_chr(result$next_tools, "tool")
  expect_equal(next_tool_names, c("package_docs", "symbols"))
  expect_equal(result$next_tools[[2L]]$arguments$query, result$query)
  expect_false(result$next_tools[[2L]]$arguments$exact)
})

test_that("help_search reports missing local generated Rd docs", {
  result <- withr::with_tempdir({
    writeLines(
      c(
        "Package: nodocs",
        "Title: Test Package Without Docs",
        "Version: 0.0.1",
        "Description: A test package without generated Rd docs.",
        "License: MIT"
      ),
      "DESCRIPTION"
    )
    read_help_search("anything", package = "nodocs")
  })

  expect_equal(result$package, "nodocs")
  expect_equal(result$search_scope, "local_package")
  expect_equal(result$total_count, 0L)
  expect_equal(result$matches, list())
  expect_equal(length(result$limitations), 1L)
  expect_equal(result$limitations[[1L]]$code, "local_rd_missing")
  expect_equal(result$limitations[[1L]]$source, "local")
  expect_match(result$limitations[[1L]]$message, "man", fixed = TRUE)
})

test_that("help_search returns error for empty query", {
  expect_tool_error(rairai:::help_search(""))
  result <- tool_result_json(
    rairai:::help_search(""),
    simplifyVector = FALSE,
    error = TRUE
  )

  expect_equal(as.character(result$error$code), "missing_query")
})

test_that("help_search returns error for NULL query", {
  result <- read_help_search(NULL)

  expect_equal(as.character(result$error$code), "missing_query")
})

test_that("help_search returns error for missing requested package", {
  package <- "zzz_nonexistent_pkg_xyz_abc"
  result <- read_help_search("lm", package = package)

  expect_package_not_found_guidance(result$error, package)
})

test_that("help_search deduplicates results by topic and package", {
  result <- read_help_search("lm", package = "stats")

  keys <- map_chr(
    result$matches,
    function(m) paste(m$topic, m$package, sep = ":::")
  )
  expect_equal(length(keys), length(unique(keys)))
})

test_that("help_search limits and offsets matches", {
  first_page <- read_help_search("lm", package = "stats", limit = 1L)
  second_page <- read_help_search(
    "lm",
    package = "stats",
    limit = 1L,
    offset = 1L
  )

  expect_equal(first_page$limit, 1L)
  expect_equal(first_page$offset, 0L)
  expect_equal(first_page$count, 1L)
  expect_equal(length(first_page$matches), 1L)
  expect_equal(first_page$has_more, first_page$total_count > 1L)

  expect_equal(second_page$limit, 1L)
  expect_equal(second_page$offset, 1L)
  expect_equal(second_page$count, as.integer(second_page$total_count > 1L))

  if (second_page$total_count > 1L) {
    first_key <- paste(
      first_page$matches[[1]]$topic,
      first_page$matches[[1]]$package,
      sep = ":::"
    )
    second_key <- paste(
      second_page$matches[[1]]$topic,
      second_page$matches[[1]]$package,
      sep = ":::"
    )
    expect_false(identical(first_key, second_key))
    expect_equal(second_page$matches[[1L]]$rank, 2L)
  }
})

test_that("help_search normalizes invalid limit and offset", {
  result <- read_help_search("lm", package = "stats", limit = 0L, offset = -1L)

  expect_equal(result$limit, 20L)
  expect_equal(result$offset, 0L)
})

test_that("help ingress trims text and keeps coercive limit defaults", {
  root <- withr::local_tempdir()
  limits <- list(NULL, integer(), NA_integer_, "bad", list("2", 9), 0, -1, 100)
  results <- map(limits, function(limit) {
    read_help_search(
      c("  linear  model  ", "unused"),
      package = " stats ",
      limit = limit,
      offset = -1,
      root_path = root
    )
  })
  expect_equal(map_chr(results, "query"), rep("linear  model", 8L))
  expect_equal(
    map_int(results, "limit"),
    c(20L, 20L, 20L, 20L, 2L, 20L, 20L, 100L)
  )
  expect_equal(map_int(results, "offset"), rep(0L, 8L))
  untruncated <- read_help_page("lm", package = "stats", root_path = root)
  for (max_chars in list(NULL, integer(), NA_integer_, "bad", 0, -1)) {
    expect_equal(
      read_help_page(
        "lm",
        package = "stats",
        max_chars = max_chars,
        root_path = root
      ),
      untruncated
    )
  }
})
