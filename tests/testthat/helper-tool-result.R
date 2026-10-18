mcp_tool_definition <- function(tool, protocol_version = "2025-11-25") {
  getFromNamespace("tool_as_mcp_definition", "mcplite")(
    tool,
    protocol_version = protocol_version
  )
}

mcp_tool_names <- function(tools) {
  map_chr(tools, \(tool) mcp_tool_definition(tool)$name)
}

capture_tool_result <- function(result) {
  tryCatch(
    list(value = force(result), error = NULL),
    error = function(cnd) {
      list(value = NULL, error = cnd)
    }
  )
}

tool_result_text <- function(result, error = FALSE) {
  # `error` is a compatibility adapter for older test calls; thrown tool errors
  # are now detected by catching `result` instead of selecting result slots.
  captured <- capture_tool_result(result)

  if (!is.null(captured$error)) {
    return(conditionMessage(captured$error))
  }

  captured$value
}

tool_result_json <- function(result, ..., error = FALSE) {
  jsonlite::fromJSON(tool_result_text(result, error = error), ...)
}

expect_tool_value <- function(result) {
  captured <- capture_tool_result(result)

  expect_null(captured$error)
  expect_type(captured$value, "character")
  invisible(captured$value)
}

expect_tool_error <- function(result) {
  captured <- capture_tool_result(result)

  expect_s3_class(captured$error, "error")
  expect_type(conditionMessage(captured$error), "character")
  invisible(captured$error)
}

expect_package_not_found_guidance <- function(error, package) {
  expect_equal(error$code, "package_not_found")
  expect_match(error$message, package, fixed = TRUE)
  expect_equal(
    error$note,
    "Install the package in the R environment running rairai, then retry."
  )
  expect_equal(
    error$install_hint,
    paste0("install.packages(\"", package, "\")")
  )
}
