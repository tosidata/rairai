test_that("path_to_uri percent-encodes file paths", {
  root_path <- tempfile("rairai uri root ")
  path <- file.path("R", "space # percent % ää.R")

  uri <- path_to_uri(path, root_path = root_path)

  expect_true(str_starts(uri, fixed("file://")))
  expect_match(uri, "%20", fixed = TRUE)
  expect_match(uri, "%23", fixed = TRUE)
  expect_match(uri, "%25", fixed = TRUE)
  expect_match(uri, "%C3%A4%C3%A4", fixed = TRUE)
  expect_false(str_detect(uri, fixed(" ")))
  expect_false(str_detect(uri, fixed("#")))
})

test_that("uri_to_path decodes file URIs and restores relative paths", {
  root_path <- tempfile("rairai-uri-root-")
  path <- file.path("R", "space # percent % ää.R")
  absolute_path <- normalizePath(
    file.path(root_path, path),
    winslash = "/",
    mustWork = FALSE
  )

  uri <- path_to_uri(path, root_path = root_path)

  expect_equal(uri_to_path(uri, root_path = root_path), path)
  expect_equal(
    uri_to_path(uri, root_path = root_path, relative = FALSE),
    absolute_path
  )
})

test_that("path_to_uri leaves existing file URIs unchanged", {
  uri <- "file:///tmp/R/space%20%23%20percent%20%25%20%C3%A4%C3%A4.R"

  expect_equal(path_to_uri(uri, root_path = tempdir()), uri)
})

test_that("root-relative paths normalize to canonical in-root paths", {
  root_path <- normalizePath(
    withr::local_tempdir(),
    winslash = "/",
    mustWork = TRUE
  )
  dir.create(file.path(root_path, "R"))
  source_path <- file.path(root_path, "R", "example.R")
  file.create(source_path)

  result <- normalize_root_relative_path(
    file.path(root_path, "R", "..", "R", "example.R"),
    root_path = root_path
  )

  expect_equal(result, file.path("R", "example.R"))
})

test_that("root-relative paths reject paths outside the root", {
  parent <- withr::local_tempdir()
  root_path <- file.path(parent, "root")
  outside_path <- file.path(parent, "outside", "secret.R")
  dir.create(root_path)
  root_path <- normalizePath(root_path, winslash = "/", mustWork = TRUE)
  dir.create(dirname(outside_path))
  file.create(outside_path)
  input_path <- file.path(root_path, "..", "outside", "secret.R")

  result <- normalize_root_relative_path(
    input_path,
    root_path = root_path,
    argument = "context_path"
  )

  expect_equal(result$error$code, "path_outside_root")
  expect_equal(
    result$error$data,
    list(
      argument = "context_path",
      path = input_path,
      resolved_path = normalizePath(outside_path, winslash = "/"),
      root_path = normalizePath(root_path, winslash = "/")
    )
  )
})

test_that("root-relative paths retain authority when a root ancestor changes", {
  parent <- withr::local_tempdir()
  root_path <- file.path(parent, "bound", "pkg")
  outside <- file.path(parent, "outside")
  fs::dir_create(file.path(root_path, "R"))
  fs::dir_create(file.path(outside, "pkg", "R"))
  file.create(file.path(root_path, "R", "example.R"))
  file.create(file.path(outside, "pkg", "R", "example.R"))
  root_path <- normalizePath(root_path, winslash = "/", mustWork = TRUE)
  filters <- c(
    "R/example.R",
    file.path(root_path, "R/example.R"),
    path_to_uri("R/example.R", root_path = root_path)
  )
  for (path in filters) {
    expect_equal(normalize_root_relative_path(path, root_path), "R/example.R")
  }

  fs::dir_delete(dirname(root_path))
  skip_if_not(
    file.symlink(outside, dirname(root_path)),
    "Cannot create a symlink"
  )
  for (path in filters) {
    result <- normalize_root_relative_path(path, root_path)
    expect_equal(result$error$code, "path_outside_root")
    expect_equal(result$error$data$root_path, root_path)
    expect_equal(
      result$error$data$resolved_path,
      normalizePath(file.path(outside, "pkg", "R", "example.R"), winslash = "/")
    )
  }
})

test_that("root-relative paths reject symlink escapes", {
  parent <- withr::local_tempdir()
  root_path <- file.path(parent, "root")
  outside_path <- file.path(parent, "outside")
  link_path <- file.path(root_path, "linked-outside")
  dir.create(root_path)
  root_path <- normalizePath(root_path, winslash = "/", mustWork = TRUE)
  dir.create(outside_path)
  file.create(file.path(outside_path, "secret.R"))
  if (!isTRUE(suppressWarnings(file.symlink(outside_path, link_path)))) {
    skip("Host does not support symlinks.")
  }
  input_path <- file.path(link_path, "secret.R")

  result <- normalize_root_relative_path(input_path, root_path = root_path)

  expect_equal(result$error$code, "path_outside_root")
  expect_equal(
    result$error$data,
    list(
      argument = "path",
      path = input_path,
      resolved_path = normalizePath(
        file.path(outside_path, "secret.R"),
        winslash = "/"
      ),
      root_path = normalizePath(root_path, winslash = "/")
    )
  )
})
