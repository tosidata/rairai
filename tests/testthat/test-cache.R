test_that("cached_value treats NULL as a cached value and supports refresh", {
  rairai:::reset_internal_cache()
  withr::defer(rairai:::reset_internal_cache())

  counter <- new.env(parent = emptyenv())
  counter$evaluations <- 0L
  key <- list(type = "null-test", value = 1L)

  first <- rairai:::cached_value(
    key = key,
    namespace = "test",
    expr = {
      counter$evaluations <- counter$evaluations + 1L
      NULL
    }
  )
  second <- rairai:::cached_value(
    key = key,
    namespace = "test",
    expr = {
      counter$evaluations <- counter$evaluations + 1L
      "unexpected"
    }
  )
  refreshed <- rairai:::cached_value(
    key = key,
    namespace = "test",
    refresh = TRUE,
    expr = {
      counter$evaluations <- counter$evaluations + 1L
      "refreshed"
    }
  )

  expect_null(first)
  expect_null(second)
  expect_equal(refreshed, "refreshed")
  expect_equal(counter$evaluations, 2L)
})
