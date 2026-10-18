run_from_tests_root <- function(expr) {
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")),
    "Not in source tree"
  )
  withr::with_dir(root, force(expr))
}

read_package_docs <- function(...) {
  tool_result_json(
    rairai:::package_docs(...),
    simplifyVector = FALSE
  )
}

expect_package_docs_topic_is_compact <- function(topic) {
  expect_true(all(c("topic", "title", "aliases") %in% names(topic)))
  expect_false("text" %in% names(topic))
  expect_false("description" %in% names(topic))
  expect_false("usage" %in% names(topic))
  expect_false("examples" %in% names(topic))
  expect_type(topic$topic, "character")
  expect_type(topic$title, "character")
  expect_true(is.list(topic$aliases) || is.character(topic$aliases))
  if ("sections" %in% names(topic)) {
    expect_true(is.list(topic$sections) || is.character(topic$sections))
  }
}

expect_next_tools_for_package <- function(next_tools, package) {
  tool_names <- map_chr(next_tools, "tool")
  expect_setequal(
    tool_names,
    c("help_page", "help_search", "package_vignettes", "package_news")
  )

  for (tool in next_tools) {
    expect_true(all(c("tool", "reason", "arguments") %in% names(tool)))
    expect_type(tool$reason, "character")
    expect_equal(tool$arguments$package, package)
  }

  search_tool <- next_tools[[which(tool_names == "help_search")[[1L]]]]
  expect_equal(search_tool$arguments$query, "<query>")

  page_tool <- next_tools[[which(tool_names == "help_page")[[1L]]]]
  expect_equal(page_tool$arguments$topic, "<topic>")
}

test_that("package_docs returns a local documentation map", {
  result <- run_from_tests_root(read_package_docs(limit = 10L))

  expect_equal(result$package, "rairai")
  expect_equal(result$source, "local")
  expect_true(all(
    c("package", "source", "help", "vignettes", "news", "next_tools") %in%
      names(result)
  ))

  expect_true(all(
    c(
      "total_count",
      "count",
      "offset",
      "limit",
      "has_more",
      "topics"
    ) %in%
      names(result$help)
  ))
  expect_gte(result$help$total_count, 1L)
  expect_equal(result$help$count, length(result$help$topics))
  expect_equal(result$help$offset, 0L)
  expect_equal(result$help$limit, 10L)
  expect_equal(
    result$help$has_more,
    result$help$total_count > result$help$count
  )

  topics <- result$help$topics
  topic_names <- map_chr(topics, "topic")
  expect_true("serve" %in% topic_names)
  expect_package_docs_topic_is_compact(topics[[which(topic_names == "serve")[[
    1L
  ]]]])

  expect_true(all(c("available", "count") %in% names(result$vignettes)))
  expect_true(is.logical(result$vignettes$available))
  expect_gte(result$vignettes$count, 0L)
  expect_true(all(c("available") %in% names(result$news)))

  expect_next_tools_for_package(result$next_tools, "rairai")
})

test_that("package_docs topics do not contain long help text", {
  local_result <- run_from_tests_root(read_package_docs())
  installed_result <- read_package_docs("stats", limit = 5L)

  for (topic in c(local_result$help$topics, installed_result$help$topics)) {
    expect_package_docs_topic_is_compact(topic)
  }
})

test_that("package_docs paginates help topics", {
  first_page <- read_package_docs("stats", limit = 1L)
  second_page <- read_package_docs("stats", limit = 1L, offset = 1L)

  expect_equal(first_page$package, "stats")
  expect_equal(first_page$source, "installed")
  expect_gt(first_page$help$total_count, 1L)
  expect_equal(first_page$help$limit, 1L)
  expect_equal(first_page$help$offset, 0L)
  expect_equal(first_page$help$count, 1L)
  expect_true(first_page$help$has_more)

  expect_equal(second_page$help$limit, 1L)
  expect_equal(second_page$help$offset, 1L)
  expect_equal(second_page$help$count, 1L)

  first_topic <- first_page$help$topics[[1L]]$topic
  second_topic <- second_page$help$topics[[1L]]$topic
  expect_false(identical(first_topic, second_topic))
})

test_that("package_docs returns installed package documentation maps", {
  result <- read_package_docs("stats", limit = 3L)

  expect_equal(result$package, "stats")
  expect_equal(result$source, "installed")
  expect_gt(result$help$total_count, 3L)
  expect_equal(result$help$count, 3L)
  expect_equal(length(result$help$topics), 3L)
  expect_true(result$help$has_more)

  for (topic in result$help$topics) {
    expect_package_docs_topic_is_compact(topic)
  }

  expect_true(all(c("available", "count") %in% names(result$vignettes)))
  expect_true(all(c("available") %in% names(result$news)))
  expect_next_tools_for_package(result$next_tools, "stats")
})

test_that("package_docs summarizes installed help article fallback docs", {
  testthat::local_mocked_bindings(
    package_docs_index = function(package = NULL, root_path = ".") {
      list(
        package = "mockpkg",
        source = "installed",
        topics = list()
      )
    },
    installed_package_vignettes = function(package, name) {
      list(
        package = package,
        source = "installed_help",
        document_type = "help_article",
        count = 1L,
        vignettes = list(list(
          name = "getting-started",
          title = "Getting Started",
          topic = "getting-started",
          aliases = I("start_here"),
          source = "installed_help",
          document_type = "help_article"
        ))
      )
    },
    installed_package_news = function(package, version, keyword) {
      list(error = list(code = "not_found", message = "No NEWS"))
    },
    .package = "rairai"
  )

  result <- read_package_docs("mockpkg")

  expect_true(result$vignettes$available)
  expect_equal(result$vignettes$count, 1L)
  expect_equal(result$vignettes$items[[1L]]$name, "getting-started")
  expect_equal(result$vignettes$items[[1L]]$title, "Getting Started")
  expect_equal(result$vignettes$items[[1L]]$document_type, "help_article")
})

test_that("package_docs reports missing local generated Rd docs", {
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
    read_package_docs()
  })

  expect_equal(result$package, "nodocs")
  expect_equal(result$source, "local")
  expect_equal(result$help$total_count, 0L)
  expect_equal(result$help$topics, list())
  expect_equal(length(result$help$limitations), 1L)
  expect_equal(result$help$limitations[[1L]]$code, "local_rd_missing")
  expect_equal(result$help$limitations[[1L]]$source, "local")
  expect_match(result$help$limitations[[1L]]$message, "man", fixed = TRUE)
})

test_that("documentation readers preserve missing and malformed DESCRIPTION", {
  root <- withr::local_tempdir()
  dir.create(file.path(root, "man"))
  writeLines(
    c("\\name{alpha}", "\\alias{alpha}", "\\title{Alpha topic}"),
    file.path(root, "man", "alpha.Rd")
  )
  writeLines(c("# 1.0.0", "- Local change."), file.path(root, "NEWS.md"))

  for (description in list(NULL, "not dcf", "Title: No package field")) {
    if (!is.null(description)) {
      writeLines(description, file.path(root, "DESCRIPTION"))
    }
    docs <- read_package_docs(root_path = root)
    expect_equal(docs$package, "local")
    expect_equal(docs$source, "local")
    expect_true(docs$news$available)
    if (identical(description, "Title: No package field")) {
      expect_equal(map_chr(docs$help$topics, "topic"), "alpha")
      expect_false("limitations" %in% names(docs$help))
    } else {
      expect_equal(docs$help$topics, list())
      expect_equal(docs$help$limitations[[1L]]$code, "local_rd_unavailable")
    }
  }
})

test_that("local Rd read errors stay limitations rather than package errors", {
  root <- withr::local_tempdir()
  writeLines("Package: DocFixture", file.path(root, "DESCRIPTION"))
  dir.create(file.path(root, "man"))
  testthat::local_mocked_bindings(
    Rd_db = function(...) stop("Unreadable Rd fixture"),
    .package = "tools"
  )
  testthat::local_mocked_bindings(
    installed_help_search_topics = function(query) list(),
    .package = "rairai"
  )

  docs <- read_package_docs("DOCFIXTURE", root_path = root)
  expect_equal(docs, read_package_docs(root_path = root))
  expect_equal(docs$source, "local")
  expect_equal(docs$package, "DocFixture")
  expect_equal(docs$help$topics, list())
  expect_equal(
    docs$help$limitations,
    list(list(
      code = "local_rd_unavailable",
      message = paste(
        "Local generated Rd documentation could not be read:",
        "Unreadable Rd fixture"
      ),
      source = "local"
    ))
  )
  search <- tool_result_json(
    rairai:::help_search("alpha", "docfixture", root_path = root),
    simplifyVector = FALSE
  )
  expect_equal(search$search_scope, "local_package")
  expect_equal(search$matches, list())
  expect_equal(search$limitations, docs$help$limitations)
  expect_equal(map_chr(search$next_tools, "tool"), c("package_docs", "symbols"))

  installed <- read_package_docs("otherfixture", root_path = root)
  expect_package_not_found_guidance(installed$error, "otherfixture")
})

test_that("package_docs returns structured package_not_found errors", {
  package <- "zzz_nonexistent_pkg_xyz_abc"
  result <- read_package_docs(package)

  expect_package_not_found_guidance(result$error, package)
})
