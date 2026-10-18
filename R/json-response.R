#' Create a JSON response
#'
#' Absent, empty, and `NA` values each have distinct JSON-shaping meanings;
#' use them consistently.
#'
#' @param ... Fields to include in the JSON object.
#' @param auto_unbox Passed to [jsonlite::toJSON()].
#'
#' @returns A JSON object string.
#'
#' @examples
#' stopifnot(identical(
#'   as.character(rairai:::json_response(a = NULL)),
#'   '{"a":{}}'
#' ))
#' stopifnot(identical(
#'   as.character(rairai:::json_response(a = list())),
#'   '{"a":[]}'
#' ))
#' stopifnot(identical(
#'   as.character(rairai:::json_response(a = NA)),
#'   '{"a":null}'
#' ))
#' stopifnot(identical(
#'   as.character(rairai:::json_response(a = I(NA))),
#'   '{"a":[null]}'
#' ))
#'
#' @keywords internal
json_response <- function(..., auto_unbox = TRUE) {
  toJSON(rlang::list2(...), auto_unbox = auto_unbox)
}

#' Create a successful MCP tool response
#'
#' @inheritParams json_response
#' @param auto_unbox Passed to [jsonlite::toJSON()].
#'
#' @returns JSON text for successful MCP tool content.
#'
#' @keywords internal
tool_value_response <- function(..., auto_unbox = TRUE) {
  json_response(..., auto_unbox = auto_unbox)
}

#' Create an MCP tool error response
#'
#' @inheritParams json_response
#' @param auto_unbox Passed to [jsonlite::toJSON()].
#'
#' @returns Throws a tool-level error whose message is JSON text.
#'
#' @keywords internal
tool_error_response <- function(..., auto_unbox = TRUE) {
  stop(json_response(..., auto_unbox = auto_unbox), call. = FALSE)
}
