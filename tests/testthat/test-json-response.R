test_that("json_response serializes dynamic dots", {
  response <- json_response(foo = "bar", count = 1L)

  expect_equal(
    tool_result_json(response, simplifyVector = FALSE),
    list(foo = "bar", count = 1L)
  )
})

test_that("json_response supports splicing lists", {
  fields <- list(foo = "bar", count = 1L)
  response <- json_response(!!!fields)

  expect_equal(
    tool_result_json(response, simplifyVector = FALSE),
    fields
  )
})

test_that("json_response allows disabling auto-unboxing", {
  response <- json_response(foo = "bar", auto_unbox = FALSE)

  expect_equal(
    tool_result_json(response, simplifyVector = FALSE),
    list(foo = list("bar"))
  )
})

test_that("json_response distinguishes empty JSON shapes", {
  expect_identical(as.character(json_response(a = NULL)), '{"a":{}}')
  expect_identical(as.character(json_response(a = list())), '{"a":[]}')
  expect_identical(as.character(json_response(a = NA)), '{"a":null}')
  expect_identical(as.character(json_response(a = I(NA))), '{"a":[null]}')
})

test_that("tool response helpers return success JSON and throw error JSON", {
  success <- tool_value_response(foo = "bar", count = 1L)
  error <- expect_error(
    tool_error_response(error = list(code = "bad_input")),
    class = "error"
  )

  expect_tool_value(success)
  expect_equal(
    tool_result_json(success, simplifyVector = FALSE),
    list(foo = "bar", count = 1L)
  )
  expect_equal(
    jsonlite::fromJSON(conditionMessage(error), simplifyVector = FALSE),
    list(error = list(code = "bad_input"))
  )
})
