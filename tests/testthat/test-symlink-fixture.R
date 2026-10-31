test_that("symlink fixture failures retain diagnostics without leaking warnings", {
  rlang::local_bindings(
    file.symlink = function(from, to) {
      warning("simulated Windows privilege failure")
      FALSE
    },
    .env = environment(create_test_symlink)
  )

  expect_no_warning(
    result <- tryCatch(create_test_symlink("target", "link"), skip = identity)
  )

  expect_s3_class(result, "skip")
  expect_match(
    conditionMessage(result),
    "simulated Windows privilege failure",
    fixed = TRUE
  )
})

test_that("symlink fixture failures without warnings still skip", {
  rlang::local_bindings(
    file.symlink = function(from, to) FALSE,
    .env = environment(create_test_symlink)
  )

  result <- tryCatch(create_test_symlink("target", "link"), skip = identity)

  expect_s3_class(result, "skip")
  expect_match(
    conditionMessage(result),
    "Cannot create a symlink",
    fixed = TRUE
  )
})

test_that("symlink fixture creation does not convert unexpected errors to skips", {
  error <- simpleError("unexpected fixture error")
  rlang::local_bindings(
    file.symlink = function(from, to) stop(error),
    .env = environment(create_test_symlink)
  )

  result <- tryCatch(
    create_test_symlink("target", "link"),
    error = identity,
    skip = identity
  )

  expect_identical(result, error)
})

test_that("successful symlink setup leaves behavior assertions and conditions active", {
  rlang::local_bindings(
    file.symlink = function(from, to) {
      expect_identical(from, "target")
      expect_identical(to, "link")
      TRUE
    },
    .env = environment(create_test_symlink)
  )

  result <- tryCatch(create_test_symlink("target", "link"), skip = identity)
  expect_true(result)
  expect_warning(
    {
      create_test_symlink("target", "link")
      warning("behavior warning")
    },
    "behavior warning"
  )
  expect_error(
    {
      create_test_symlink("target", "link")
      stop("behavior error")
    },
    "behavior error"
  )
})
