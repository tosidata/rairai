run_from_tests_root <- function(expr) {
  withr::with_dir(
    testthat::test_path("..", ".."),
    force(expr)
  )
}

test_that("tests_for returns matches for a known symbol", {
  result <- run_from_tests_root(
    tool_result_json(
      rairai:::tests_for("read_package_files"),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$name, "read_package_files")
  expect_gte(result$count, 1L)
  expect_true(all(map_lgl(
    result$matches,
    function(m) str_starts(m$location, "tests/")
  )))
  expect_true(all(map_lgl(
    result$matches,
    function(m) str_detect(m$location, ":\\d+(?:-\\d+)?$")
  )))
  expect_true(all(map_lgl(
    result$matches,
    function(m) is.integer(m$hit_count) && m$hit_count >= 1L
  )))
  expect_true(all(map_lgl(
    result$matches,
    function(m) length(m$hits) == m$hit_count
  )))
})

test_that("tests_for returns empty result when no tests directory exists", {
  withr::with_tempdir({
    result <- tool_result_json(
      rairai:::tests_for("some_func_name"),
      simplifyVector = FALSE
    )

    expect_equal(result$name, "some_func_name")
    expect_equal(result$count, 0L)
    expect_equal(result$matches, list())
  })
})

test_that("tests_for returns nearest test_that description when available", {
  result <- run_from_tests_root(
    tool_result_json(
      rairai:::tests_for("read_package_files"),
      simplifyVector = FALSE
    )
  )

  expect_gte(result$count, 1L)
  expect_true(any(map_lgl(
    result$matches,
    function(m) !is.null(m$desc) && nzchar(m$desc)
  )))
})

test_that("tests_for keeps exact locations separate from leading comments", {
  withr::with_tempdir({
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c(
        "#' test docs are just comments here",
        "# regular comment",
        "testthat::test_that('foo works', {",
        "  testthat::expect_true(foo())",
        "})"
      ),
      "tests/testthat/test-foo.R"
    )

    result <- tool_result_json(
      rairai:::tests_for("foo"),
      simplifyVector = FALSE
    )

    locations <- map_chr(result$matches, "location")
    expect_true("tests/testthat/test-foo.R:3-5" %in% locations)
    expect_false(any(str_detect(locations, ":1-")))
    expect_true(any(map_lgl(
      result$matches,
      function(m) !is.null(m$desc) && identical(m$desc, "foo works")
    )))

    match <- result$matches[[match("tests/testthat/test-foo.R:3-5", locations)]]
    expect_equal(match$leading_comment_lines, "1-2")
    expect_equal(match$hit_count, 2L)
    expect_equal(
      unlist(match$hits, use.names = FALSE),
      c("tests/testthat/test-foo.R:3", "tests/testthat/test-foo.R:4")
    )
  })
})

test_that("tests_for uses Tree-sitter ranges for multiline test blocks", {
  withr::with_tempdir({
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c(
        "# unrelated earlier comment",
        "",
        "# block context",
        "# still context",
        "testthat::test_that(",
        "  'qualified multiline uses qux',",
        "  {",
        "    # parentheses in comments do not close blocks: )",
        "    testthat::expect_true(qux(list(a = '(')))",
        "    testthat::expect_equal(function(x) { qux(x) }(1), 1)",
        "  }",
        ")",
        "",
        "test_that('bare call uses qux', {",
        "  testthat::expect_true(qux())",
        "})"
      ),
      "tests/testthat/test-qux.R"
    )

    result <- tool_result_json(
      rairai:::tests_for("qux"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 2L)
    expect_equal(
      map_chr(result$matches, "location"),
      c("tests/testthat/test-qux.R:5-12", "tests/testthat/test-qux.R:14-16")
    )
    expect_equal(result$matches[[1L]]$leading_comment_lines, "3-4")
    expect_false("leading_comment_lines" %in% names(result$matches[[2L]]))
    expect_equal(
      map_chr(result$matches, "desc"),
      c(
        "qualified multiline uses qux",
        "bare call uses qux"
      )
    )
    expect_equal(map_int(result$matches, "hit_count"), c(3L, 2L))
  })
})

test_that("tests_for keeps text-hit semantics for comments and strings", {
  withr::with_tempdir({
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c(
        "# mention_only appears in a leading comment",
        "test_that('string mention_only remains a text hit', {",
        "  testthat::expect_true(TRUE)",
        "})"
      ),
      "tests/testthat/test-text-hit.R"
    )

    result <- tool_result_json(
      rairai:::tests_for("mention_only"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 2L)
    expect_equal(
      map_chr(result$matches, "location"),
      c(
        "tests/testthat/test-text-hit.R:1",
        "tests/testthat/test-text-hit.R:2-4"
      )
    )
    expect_false("leading_comment_lines" %in% names(result$matches[[1L]]))
    expect_equal(result$matches[[2L]]$leading_comment_lines, "1")
    expect_equal(
      unlist(result$matches[[1L]]$hits, use.names = FALSE),
      "tests/testthat/test-text-hit.R:1"
    )
    expect_equal(
      unlist(result$matches[[2L]]$hits, use.names = FALSE),
      "tests/testthat/test-text-hit.R:2"
    )
  })
})

test_that("tests_for groups repeated hits in the same test block", {
  withr::with_tempdir({
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c(
        "testthat::test_that('groups hits', {",
        "  testthat::expect_equal(bar(), 1)",
        "  testthat::expect_true(bar() > 0)",
        "})"
      ),
      "tests/testthat/test-bar.R"
    )

    result <- tool_result_json(
      rairai:::tests_for("bar"),
      simplifyVector = FALSE
    )

    expect_equal(result$count, 1L)
    expect_equal(result$matches[[1L]]$location, "tests/testthat/test-bar.R:1-4")
    expect_equal(result$matches[[1L]]$desc, "groups hits")
    expect_equal(result$matches[[1L]]$hit_count, 2L)
    expect_equal(
      unlist(result$matches[[1L]]$hits, use.names = FALSE),
      c("tests/testthat/test-bar.R:2", "tests/testthat/test-bar.R:3")
    )
  })
})

test_that("tests_for keeps matches outside test blocks line-specific", {
  withr::with_tempdir({
    dir.create("tests/testthat", recursive = TRUE)
    writeLines(
      c(
        "testthat::test_that('uses baz', {",
        "  testthat::expect_true(baz())",
        "})",
        "",
        "# outside helper context",
        "baz <- function() TRUE"
      ),
      "tests/testthat/test-baz.R"
    )

    result <- tool_result_json(
      rairai:::tests_for("baz"),
      simplifyVector = FALSE
    )

    locations <- map_chr(result$matches, "location")
    expect_equal(
      locations,
      c("tests/testthat/test-baz.R:1-3", "tests/testthat/test-baz.R:6")
    )
    expect_false("leading_comment_lines" %in% names(result$matches[[1L]]))
    expect_equal(result$matches[[2L]]$leading_comment_lines, "5")
    expect_equal(map_int(result$matches, "hit_count"), c(2L, 1L))
    expect_equal(
      unlist(result$matches[[2L]]$hits, use.names = FALSE),
      "tests/testthat/test-baz.R:6"
    )
  })
})

test_that("tests_for returns error for empty name", {
  result <- run_from_tests_root(
    tool_result_json(
      rairai:::tests_for(""),
      simplifyVector = FALSE
    )
  )

  expect_equal(as.character(unlist(result$error$code)), "missing_name")
})

test_that("tests_for is the registered public test-discovery tool", {
  tools <- rairai:::serve_tools(NULL)
  tool_names <- mcp_tool_names(tools)

  expect_true("tests_for" %in% tool_names)
  expect_false("find_test_for_symbol" %in% tool_names)
})
