test_that("format_location emits compact source locations", {
  expect_equal(rairai:::format_location("R/foo.R"), "R/foo.R")
  expect_equal(rairai:::format_location("R/foo.R", line = 42), "R/foo.R:42")
  expect_equal(
    rairai:::format_location("R/foo.R", line = 42, end_line = 57),
    "R/foo.R:42-57"
  )
  expect_equal(
    rairai:::format_location("R/foo.R", line = 42, column = 3),
    "R/foo.R:42:3"
  )
  expect_equal(
    rairai:::format_location(
      "R/foo.R",
      line = 42,
      column = 3,
      end_line = 57,
      end_column = 8
    ),
    "R/foo.R:42:3-57:8"
  )
  expect_equal(
    rairai:::format_location(
      "R/foo.R",
      line = 42,
      column = 3,
      end_line = 42,
      end_column = 8
    ),
    "R/foo.R:42:3-42:8"
  )
})

test_that("compact_location_fields removes redundant source fields", {
  record <- rairai:::compact_location_fields(list(
    name = "foo",
    path = "R/foo.R",
    line = 1L,
    column = 2L,
    end_line = 3L,
    end_column = 4L
  ))

  expect_equal(record$name, "foo")
  expect_equal(record$location, "R/foo.R:1:2-3:4")
  expect_null(record$path)
  expect_null(record$line)
  expect_null(record$column)
  expect_null(record$end_line)
  expect_null(record$end_column)
})

test_that("format_same_file_line_range emits pathless compact ranges", {
  expect_equal(rairai:::format_same_file_line_range(4L), "4")
  expect_equal(rairai:::format_same_file_line_range(1L, 4L), "1-4")
  expect_null(rairai:::format_same_file_line_range(NULL))
})

test_that("leading_comment_block_range finds contiguous ordinary and roxygen comments", {
  lines <- c(
    "#' Title",
    "# regular comment",
    "foo <- function() NULL"
  )

  expect_equal(
    rairai:::leading_comment_block_range(lines, 3L),
    list(start_line = 1L, end_line = 2L)
  )
})

test_that("leading_comment_block_range stops at blank lines and code", {
  blank_boundary <- c(
    "# unrelated",
    "",
    "# related",
    "foo <- function() NULL"
  )
  code_boundary <- c(
    "# unrelated",
    "old <- function() NULL",
    "# related",
    "foo <- function() NULL"
  )

  expect_equal(
    rairai:::leading_comment_block_range(blank_boundary, 4L),
    list(start_line = 3L, end_line = 3L)
  )
  expect_equal(
    rairai:::leading_comment_block_range(code_boundary, 4L),
    list(start_line = 3L, end_line = 3L)
  )
})

test_that("leading_comment_block_range omits absent and first-line context", {
  expect_null(rairai:::leading_comment_block_range(
    "foo <- function() NULL",
    1L
  ))
  expect_null(rairai:::leading_comment_block_range(
    "foo <- function() NULL",
    0L
  ))
  expect_null(rairai:::leading_comment_block_range(
    "foo <- function() NULL",
    2L
  ))
})

test_that("compact_location_fields adds optional leading comment lines", {
  lines <- c(
    "#' Title",
    "# details",
    "foo <- function() {",
    "  NULL",
    "}"
  )
  record <- rairai:::compact_location_fields(
    list(path = "R/foo.R", line = 3L, end_line = 5L),
    include_columns = FALSE,
    include_leading_comment = TRUE,
    source_lines = lines
  )

  expect_equal(record$location, "R/foo.R:3-5")
  expect_equal(record$leading_comment_lines, "1-2")
})

test_that("compact_location_fields omits absent leading comment lines", {
  record <- rairai:::compact_location_fields(
    list(path = "R/foo.R", line = 1L, end_line = 1L),
    include_columns = FALSE,
    include_leading_comment = TRUE,
    source_lines = "foo <- function() NULL"
  )

  expect_equal(record$location, "R/foo.R:1")
  expect_false("leading_comment_lines" %in% names(record))
})
