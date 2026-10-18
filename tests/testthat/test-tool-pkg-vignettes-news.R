run_from_pkg_root <- function(expr) {
  root <- testthat::test_path("..", "..")
  testthat::skip_if_not(
    file.exists(file.path(root, "DESCRIPTION")),
    "Not in source tree"
  )
  withr::with_dir(root, force(expr))
}

# package_vignettes tests

test_that("package_vignettes lists local vignettes", {
  skip_if(
    !dir.exists(file.path(testthat::test_path("..", ".."), "vignettes")),
    "Not in source tree"
  )
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$package, "rairai")
  expect_equal(result$source, "local")
  expect_gte(result$count, 1L)
  expect_true(length(result$vignettes) > 0)
  first <- result$vignettes[[1]]
  expect_true(!is.null(first$name))
  expect_true(!is.null(first$file))
})

test_that("package_vignettes retrieves local vignette content", {
  skip_if(
    !dir.exists(file.path(testthat::test_path("..", ".."), "vignettes")),
    "Not in source tree"
  )
  vigs <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(),
      simplifyVector = FALSE
    )
  )
  first_name <- vigs$vignettes[[1]]$name

  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(name = first_name),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$package, "rairai")
  expect_equal(result$source, "local")
  expect_equal(result$name, first_name)
  expect_equal(result$content_type, "source")
  expect_true(nchar(result$content) > 0)
})

test_that("package_vignettes returns error for unknown local vignette", {
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(name = "nonexistent_vignette_xyz"),
      simplifyVector = FALSE
    )
  )

  expect_equal(as.character(result$error$code), "not_found")
})

test_that("package_vignettes treats NULL and empty package as local", {
  null_result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(package = NULL),
      simplifyVector = FALSE
    )
  )
  empty_result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(package = ""),
      simplifyVector = FALSE
    )
  )

  expect_equal(null_result$package, "rairai")
  expect_equal(empty_result$package, "rairai")
  expect_equal(null_result$source, "local")
  expect_equal(empty_result$source, "local")
})

test_that("package_vignettes treats local package name as local source", {
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_vignettes(package = "rairai"),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$package, "rairai")
  expect_equal(result$source, "local")
})

test_that("package_vignettes lists installed package vignettes", {
  result <- tool_result_json(
    rairai:::package_vignettes(package = "stringr"),
    simplifyVector = FALSE
  )

  expect_equal(result$package, "stringr")
  expect_equal(result$source, "installed")
  expect_gte(result$count, 1L)
  first <- result$vignettes[[1]]
  expect_true(!is.null(first$name))
  expect_true(!is.null(first$title))
})

test_that("package_vignettes retrieves installed vignette content", {
  vigs <- tool_result_json(
    rairai:::package_vignettes(package = "stringr"),
    simplifyVector = FALSE
  )
  first_name <- vigs$vignettes[[1]]$name

  result <- tool_result_json(
    rairai:::package_vignettes(package = "stringr", name = first_name),
    simplifyVector = FALSE
  )

  expect_equal(result$package, "stringr")
  expect_equal(result$source, "installed")
  expect_equal(result$name, first_name)
  expect_true(result$content_type %in% c("source", "stripped_html"))
  expect_true(nchar(result$content) > 0)
})

test_that("package_vignettes returns error for unknown installed vignette", {
  result <- tool_result_json(
    rairai:::package_vignettes(package = "stringr", name = "nonexistent_xyz"),
    simplifyVector = FALSE
  )

  expect_equal(as.character(result$error$code), "not_found")
})

mock_help_articles <- function() {
  list(
    list(
      name = "getting-started",
      title = "Getting Started",
      topic = "getting-started",
      aliases = I(c("Getting Started", "start_here")),
      source = "installed_help",
      document_type = "help_article"
    ),
    list(
      name = "mockpkg-package",
      title = "mockpkg",
      topic = "mockpkg-package",
      aliases = I(c("mockpkg", "overview")),
      source = "installed_help",
      document_type = "help_article"
    )
  )
}

with_mock_help_article_fallback <- function(
  expr,
  articles = mock_help_articles(),
  vigs = NULL,
  package = "stringr"
) {
  testthat::skip_if_not_installed(package)
  testthat::local_mocked_bindings(
    installed_vignette_results = function(package) vigs,
    help_article_candidates = function(package) articles,
    find_installed_rd_for_topic = function(topic, package) list(rd = topic),
    render_rd_to_text = function(rd) paste("Rendered", rd),
    .package = "rairai"
  )

  force(expr)
}

mock_vignette_results <- function() {
  matrix(
    c("standard-vignette", "Standard Vignette"),
    nrow = 1L,
    dimnames = list(NULL, c("Item", "Title"))
  )
}

test_that("package_vignettes ignores fallback docs when installed vignettes exist", {
  result <- with_mock_help_article_fallback(
    tool_result_json(
      rairai:::package_vignettes(package = "stringr"),
      simplifyVector = FALSE
    ),
    articles = stop("fallback should not be consulted"),
    vigs = mock_vignette_results()
  )

  expect_equal(result$package, "stringr")
  expect_equal(result$source, "installed")
  expect_equal(result$count, 1L)
  expect_equal(result$vignettes[[1L]]$name, "standard-vignette")
})

test_that("package_vignettes lists installed help article fallback docs", {
  result <- with_mock_help_article_fallback(tool_result_json(
    rairai:::package_vignettes(package = "stringr"),
    simplifyVector = FALSE
  ))

  expect_equal(result$package, "stringr")
  expect_equal(result$source, "installed_help")
  expect_equal(result$document_type, "help_article")
  expect_match(result$note, "No standard vignettes found", fixed = TRUE)
  expect_equal(result$count, 2L)

  names <- map_chr(result$vignettes, "name")
  expect_equal(names, c("getting-started", "mockpkg-package"))
  getting_started_idx <- match("getting-started", names)
  expect_false(is.na(getting_started_idx))

  getting_started <- result$vignettes[[getting_started_idx]]
  expect_equal(getting_started$title, "Getting Started")
  expect_equal(getting_started$source, "installed_help")
  expect_equal(getting_started$document_type, "help_article")
  expect_equal(getting_started$topic, "getting-started")
})

test_that("package_vignettes retrieves installed help article fallback by title", {
  result <- with_mock_help_article_fallback(tool_result_json(
    rairai:::package_vignettes(package = "stringr", name = "Getting Started"),
    simplifyVector = FALSE
  ))

  expect_equal(result$package, "stringr")
  expect_equal(result$source, "installed_help")
  expect_equal(result$document_type, "help_article")
  expect_match(result$note, "No standard vignettes found", fixed = TRUE)
  expect_equal(result$name, "getting-started")
  expect_equal(result$title, "Getting Started")
  expect_equal(result$topic, "getting-started")
  expect_equal(result$content_type, "rendered_rd")
  expect_match(result$content, "Rendered getting-started", fixed = TRUE)
})

test_that("package_vignettes reports ambiguous help article fallback names", {
  articles <- list(
    list(
      name = "first-topic",
      title = "Getting Started",
      topic = "first-topic",
      aliases = I("guide"),
      source = "installed_help",
      document_type = "help_article"
    ),
    list(
      name = "second-topic",
      title = "Guide",
      topic = "second-topic",
      aliases = I("guide"),
      source = "installed_help",
      document_type = "help_article"
    )
  )

  result <- with_mock_help_article_fallback(
    tool_result_json(
      rairai:::package_vignettes(package = "stringr", name = "guide"),
      simplifyVector = FALSE
    ),
    articles = articles
  )

  expect_equal(result$error$code, "ambiguous_name")
  expect_match(result$error$message, "guide", fixed = TRUE)
  expect_equal(length(result$error$candidates), 2L)

  candidate_names <- map_chr(result$error$candidates, "name")
  expect_setequal(candidate_names, c("first-topic", "second-topic"))
  expect_true(all(map_lgl(
    result$error$candidates,
    \(candidate) {
      all(c("name", "title", "topic", "aliases") %in% names(candidate))
    }
  )))
})

test_that("package_vignettes returns not_found for unknown help article fallback", {
  result <- with_mock_help_article_fallback(tool_result_json(
    rairai:::package_vignettes(package = "stringr", name = "nonexistent_xyz"),
    simplifyVector = FALSE
  ))

  expect_equal(as.character(result$error$code), "not_found")
})

test_that("package_vignettes returns package_not_found for uninstalled package", {
  result <- tool_result_json(
    rairai:::package_vignettes(package = "nonexistent_pkg_xyz"),
    simplifyVector = FALSE
  )

  expect_equal(as.character(result$error$code), "package_not_found")
})

# package_news tests

test_that("package_news returns a local NEWS overview", {
  skip_if(
    !file.exists(file.path(testthat::test_path("..", ".."), "NEWS.md")),
    "Not in source tree"
  )
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$package, "rairai")
  expect_equal(result$source, "NEWS.md")
  expect_equal(result$mode, "overview")
  expect_true(!is.null(result$latest$heading))
  expect_true(!is.null(result$latest$location))
  expect_true(!is.null(result$latest$entries))
  expect_lte(result$latest$returned_count, 20L)
  expect_equal(result$count, length(result$versions))
  expect_equal(result$versions[[1]]$version, "development version")
  expect_true("0.2.3" %in% map_chr(result$versions, "version"))
  first_version <- result$versions[[1]]
  expect_equal(first_version$heading, result$latest$heading)
  expect_equal(first_version$location, result$latest$location)
})

test_that("package_news keeps NEWS section ranges without roxygen expansion", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "#' Not roxygen for NEWS",
        "",
        "# localpkg 1.0.0",
        "",
        "- Initial release."
      ),
      "NEWS.md"
    )

    result <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )

    expect_equal(result$latest$location, "NEWS.md:3-5")
    expect_equal(result$versions[[1]]$location, "NEWS.md:3-5")
  })
})

test_that("package_news assigns nested categories to complete releases", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# localpkg (development version)",
        "",
        "## New features",
        "",
        "- Added alpha support.",
        "",
        "## Fixes 2.0",
        "",
        "- Fixed beta handling.",
        "",
        "# localpkg 1.0.0",
        "",
        "## New features",
        "",
        "- Added old alpha support.",
        "",
        "## Fixes 2.0",
        "",
        "- Fixed old gamma handling."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(overview$count, 2L)
    expect_equal(
      map_chr(overview$versions, "version"),
      c(
        "development version",
        "1.0.0"
      )
    )
    expect_equal(overview$latest$location, "NEWS.md:1-10")
    expect_equal(overview$latest$entry_count, 2L)
    expect_true(any(str_detect(
      overview$latest$entries,
      fixed("alpha support")
    )))
    expect_true(any(str_detect(
      overview$latest$entries,
      fixed("beta handling")
    )))

    keyword <- tool_result_json(
      rairai:::package_news(keyword = "alpha"),
      simplifyVector = FALSE
    )
    expect_equal(keyword$count, 2L)
    expect_length(keyword$matches, 2L)
    expect_named(
      keyword$matches[[1]],
      c("heading", "version", "count", "matches")
    )
    expect_type(keyword$matches[[1]]$matches, "list")
    expect_equal(map_chr(keyword$matches, "heading"), rep("New features", 2L))
    expect_equal(
      map_chr(keyword$matches, "version"),
      c(
        "development version",
        "1.0.0"
      )
    )

    dotted_category <- tool_result_json(
      rairai:::package_news(keyword = "beta"),
      simplifyVector = FALSE
    )
    expect_equal(dotted_category$matches[[1]]$heading, "Fixes 2.0")
    expect_equal(dotted_category$matches[[1]]$version, "development version")

    category_version <- tool_result_json(
      rairai:::package_news(version = "Fixes 2.0"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(category_version$found))

    release <- tool_result_json(
      rairai:::package_news(version = "1.0.0"),
      simplifyVector = FALSE
    )
    expect_equal(release$location, "NEWS.md:11-19")
    expect_true(str_detect(release$content, fixed("## New features")))
    expect_true(str_detect(release$content, fixed("## Fixes 2.0")))
    expect_true(str_detect(release$content, fixed("old gamma handling")))
    expect_false(str_detect(release$content, fixed("beta handling")))

    selected_keyword <- tool_result_json(
      rairai:::package_news(
        version = "development version",
        keyword = "beta"
      ),
      simplifyVector = FALSE
    )
    expect_true(selected_keyword$found)
    expect_equal(selected_keyword$count, 1L)

    other_release_keyword <- tool_result_json(
      rairai:::package_news(version = "1.0.0", keyword = "beta"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(other_release_keyword$found))
    expect_equal(other_release_keyword$count, 0L)
  })
})

test_that("package_news keeps nested version-like headings inside releases", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# localpkg 2.0.0",
        "",
        "## 2.1.0",
        "- Documented nested behavior.",
        "# localpkg 1.0.0",
        "",
        "- Previous release."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(overview$count, 2L)
    expect_equal(map_chr(overview$versions, "version"), c("2.0.0", "1.0.0"))
    expect_equal(overview$latest$location, "NEWS.md:1-4")
    expect_equal(overview$latest$entry_count, 1L)

    keyword <- tool_result_json(
      rairai:::package_news(keyword = "nested behavior"),
      simplifyVector = FALSE
    )
    expect_equal(keyword$matches[[1]]$heading, "2.1.0")
    expect_equal(keyword$matches[[1]]$version, "2.0.0")

    nested_version <- tool_result_json(
      rairai:::package_news(version = "2.1.0"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(nested_version$found))
  })
})

test_that("package_news requires structural release ancestry", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# Changes",
        "## API",
        "### 2.0",
        "",
        "- Updated API details."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(overview$count, 1L)
    expect_null(overview$versions[[1]]$version)
    expect_equal(overview$latest$location, "NEWS.md:1-5")
    expect_equal(overview$latest$entry_count, 1L)

    keyword <- tool_result_json(
      rairai:::package_news(keyword = "API details"),
      simplifyVector = FALSE
    )
    expect_equal(keyword$matches[[1]]$heading, "2.0")
    expect_null(keyword$matches[[1]]$version)

    nested_version <- tool_result_json(
      rairai:::package_news(version = "2.0"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(nested_version$found))
  })
})

test_that("package_news recognizes versions below a document title", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# Changes",
        "## 2.0.0",
        "",
        "- Current release.",
        "## 1.0.0",
        "",
        "- Previous release."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(map_chr(overview$versions, "version"), c("2.0.0", "1.0.0"))
    expect_equal(overview$latest$location, "NEWS.md:2-4")

    release <- tool_result_json(
      rairai:::package_news(version = "1.0.0"),
      simplifyVector = FALSE
    )
    expect_true(release$found)
    expect_match(release$content, "Previous release", fixed = TRUE)
  })
})

test_that("package_news stops release ownership at generic siblings", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# localpkg 2.0.0",
        "- Release change.",
        "# Migration notes",
        "- General migration guidance.",
        "# localpkg 1.0.0",
        "- Previous release.",
        "# General notes",
        "- General future guidance."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(map_chr(overview$versions, "version"), c("2.0.0", "1.0.0"))
    expect_equal(overview$latest$location, "NEWS.md:1-2")
    expect_equal(overview$latest$entry_count, 1L)
    expect_match(overview$latest$entries[[1]], "Release change", fixed = TRUE)
    expect_false(any(str_detect(
      overview$latest$entries,
      fixed("migration guidance")
    )))

    current <- tool_result_json(
      rairai:::package_news(version = "2.0.0"),
      simplifyVector = FALSE
    )
    expect_equal(current$location, "NEWS.md:1-2")
    expect_false(str_detect(current$content, fixed("Migration notes")))
    expect_false(str_detect(current$content, fixed("migration guidance")))

    previous <- tool_result_json(
      rairai:::package_news(version = "1.0.0"),
      simplifyVector = FALSE
    )
    expect_equal(previous$location, "NEWS.md:5-6")
    expect_false(str_detect(previous$content, fixed("General notes")))

    gap_keyword <- tool_result_json(
      rairai:::package_news(keyword = "migration guidance"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(gap_keyword$found))
    expect_equal(gap_keyword$count, 0L)

    current_gap_keyword <- tool_result_json(
      rairai:::package_news(
        version = "2.0.0",
        keyword = "migration guidance"
      ),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(current_gap_keyword$found))
    expect_equal(current_gap_keyword$count, 0L)

    release_keyword <- tool_result_json(
      rairai:::package_news(keyword = "Previous release"),
      simplifyVector = FALSE
    )
    expect_true(release_keyword$found)
    expect_equal(release_keyword$matches[[1]]$version, "1.0.0")
  })
})

test_that("NEWS version headings are recognized strictly", {
  expect_equal(
    rairai:::news_heading_version(
      "localpkg (development version)",
      "localpkg"
    ),
    "development version"
  )
  expect_equal(
    rairai:::news_heading_version("Version 1.2.3", "localpkg"),
    "1.2.3"
  )
  expect_equal(
    rairai:::news_heading_version("v1.2.3", "localpkg"),
    "1.2.3"
  )
  expect_equal(
    rairai:::news_heading_version("localpkg 1.2.3", "localpkg"),
    "1.2.3"
  )
  expect_null(rairai:::news_heading_version("Fixes 2.0", "localpkg"))
  expect_null(rairai:::news_heading_version("otherpkg 1.2.3", "localpkg"))
  expect_null(rairai:::news_heading_version("Release notes", "localpkg"))
})

test_that("package_news does not treat dotted categories as releases", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# Changes",
        "",
        "## Fixes 2.0",
        "",
        "- Fixed category handling.",
        "",
        "## API 3.0",
        "",
        "- Updated category details."
      ),
      "NEWS.md"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(overview$count, 1L)
    expect_null(overview$versions[[1]]$version)
    expect_equal(overview$latest$location, "NEWS.md:1-9")
    expect_equal(overview$latest$entry_count, 2L)

    keyword <- tool_result_json(
      rairai:::package_news(keyword = "category handling"),
      simplifyVector = FALSE
    )
    expect_equal(keyword$count, 1L)
    expect_equal(keyword$matches[[1]]$heading, "Fixes 2.0")
    expect_null(keyword$matches[[1]]$version)

    category_version <- tool_result_json(
      rairai:::package_news(version = "Fixes 2.0"),
      simplifyVector = FALSE
    )
    expect_false(isTRUE(category_version$found))
  })
})

test_that("package_news preserves flat unheaded NEWS fallback", {
  withr::with_tempdir({
    writeLines(
      c(
        "Initial release notes",
        "",
        "- Added flat NEWS support."
      ),
      "NEWS"
    )

    overview <- tool_result_json(
      rairai:::package_news(),
      simplifyVector = FALSE
    )
    expect_equal(overview$count, 1L)
    expect_equal(overview$latest$location, "NEWS:1-3")
    expect_equal(overview$latest$entry_count, 1L)
    expect_match(overview$latest$entries[[1]], "flat NEWS support")

    keyword <- tool_result_json(
      rairai:::package_news(keyword = "flat NEWS"),
      simplifyVector = FALSE
    )
    expect_true(keyword$found)
    expect_equal(keyword$count, 1L)
  })
})

test_that("package_news filters local NEWS by keyword", {
  skip_if(
    !file.exists(file.path(testthat::test_path("..", ".."), "NEWS.md")),
    "Not in source tree"
  )
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_news(keyword = "help_page"),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$package, "rairai")
  expect_equal(result$mode, "keyword")
  expect_true(result$found)
  expect_gte(result$count, 1L)
  entries <- unlist(result$matches, recursive = TRUE, use.names = FALSE)
  expect_true(any(str_detect(entries, fixed("help_page"))))
})

test_that("package_news returns found=FALSE for unmatched keyword", {
  skip_if(
    !file.exists(file.path(testthat::test_path("..", ".."), "NEWS.md")),
    "Not in source tree"
  )
  result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_news(keyword = "zzznonexistent_keyword_xyz"),
      simplifyVector = FALSE
    )
  )

  expect_equal(result$mode, "keyword")
  expect_false(isTRUE(result$found))
  expect_equal(result$count, 0L)
  expect_equal(result$matches, list())
})

test_that("package_news treats NULL and empty package as local", {
  skip_if(
    !file.exists(file.path(testthat::test_path("..", ".."), "NEWS.md")),
    "Not in source tree"
  )
  null_result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_news(package = NULL),
      simplifyVector = FALSE
    )
  )
  empty_result <- run_from_pkg_root(
    tool_result_json(
      rairai:::package_news(package = ""),
      simplifyVector = FALSE
    )
  )

  expect_equal(null_result$package, "rairai")
  expect_equal(empty_result$package, "rairai")
})

test_that("package_news preserves release structure for installed packages", {
  package_source <- withr::local_tempdir()
  package_library <- withr::local_tempdir()
  writeLines(
    c(
      "Package: newspkgfixture",
      "Type: Package",
      "Title: Installed NEWS Fixture",
      "Version: 0.0.1",
      paste0(
        "Authors@R: person(\"Test\", \"Fixture\", ",
        "email = \"test@example.com\", role = c(\"aut\", \"cre\"))"
      ),
      "Description: Provides deterministic installed NEWS test content.",
      "License: MIT",
      "Encoding: UTF-8"
    ),
    file.path(package_source, "DESCRIPTION")
  )
  writeLines(character(), file.path(package_source, "NAMESPACE"))
  writeLines(
    c(
      "# Changes",
      "## 2.0.0",
      "",
      "### API",
      "",
      "#### 2.1",
      "",
      "- Updated installed API details.",
      "",
      "### Fixes",
      "",
      "- Fixed installed behavior.",
      "",
      "## Migration notes",
      "",
      "- General installed migration guidance.",
      "",
      "## 1.0.0",
      "",
      "- Previous installed release.",
      "",
      "## General notes",
      "",
      "- General installed future guidance."
    ),
    file.path(package_source, "NEWS.md")
  )

  install_output <- system2(
    file.path(R.home("bin"), "R"),
    c(
      "CMD",
      "INSTALL",
      "--no-byte-compile",
      paste0("--library=", shQuote(package_library)),
      shQuote(package_source)
    ),
    stdout = TRUE,
    stderr = TRUE
  )
  install_status <- attr(install_output, "status")
  if (is.null(install_status)) {
    install_status <- 0L
  }
  expect_equal(
    install_status,
    0L,
    info = paste(install_output, collapse = "\n")
  )
  withr::local_libpaths(package_library, action = "prefix")

  overview <- tool_result_json(
    rairai:::package_news(package = "newspkgfixture"),
    simplifyVector = FALSE
  )
  expect_equal(overview$package, "newspkgfixture")
  expect_equal(overview$mode, "overview")
  expect_equal(map_chr(overview$versions, "version"), c("2.0.0", "1.0.0"))
  expect_equal(map_chr(overview$versions, "heading"), c("2.0.0", "1.0.0"))
  expect_equal(overview$latest$location, "NEWS.md:2-13")
  expect_equal(overview$latest$entry_count, 2L)

  release <- tool_result_json(
    rairai:::package_news(package = "newspkgfixture", version = "2.0.0"),
    simplifyVector = FALSE
  )
  expect_true(release$found)
  expect_match(release$content, "### API", fixed = TRUE)
  expect_match(release$content, "#### 2.1", fixed = TRUE)
  expect_match(release$content, "Updated installed API details", fixed = TRUE)
  expect_match(release$content, "Fixed installed behavior", fixed = TRUE)
  expect_false(str_detect(release$content, fixed("Migration notes")))
  expect_false(str_detect(
    release$content,
    fixed("installed migration guidance")
  ))
  expect_false(str_detect(release$content, fixed("Previous installed release")))

  keyword <- tool_result_json(
    rairai:::package_news(
      package = "newspkgfixture",
      keyword = "API details"
    ),
    simplifyVector = FALSE
  )
  expect_equal(keyword$matches[[1]]$heading, "2.1")
  expect_equal(keyword$matches[[1]]$version, "2.0.0")

  gap_keyword <- tool_result_json(
    rairai:::package_news(
      package = "newspkgfixture",
      keyword = "migration guidance"
    ),
    simplifyVector = FALSE
  )
  expect_false(isTRUE(gap_keyword$found))
  expect_equal(gap_keyword$count, 0L)

  previous <- tool_result_json(
    rairai:::package_news(package = "newspkgfixture", version = "1.0.0"),
    simplifyVector = FALSE
  )
  expect_equal(previous$location, "NEWS.md:18-21")
  expect_false(str_detect(previous$content, fixed("General notes")))

  category_version <- tool_result_json(
    rairai:::package_news(package = "newspkgfixture", version = "2.1"),
    simplifyVector = FALSE
  )
  expect_false(isTRUE(category_version$found))
})

test_that("package_news filters keyword within version", {
  withr::with_tempdir({
    writeLines("Package: localpkg", "DESCRIPTION")
    writeLines(
      c(
        "# localpkg 1.0.0",
        "",
        "- Added help_search support.",
        "- Other change."
      ),
      "NEWS.md"
    )

    result <- tool_result_json(
      rairai:::package_news(version = "1.0.0", keyword = "help_search"),
      simplifyVector = FALSE
    )

    expect_equal(result$mode, "version_keyword")
    expect_equal(result$version, "1.0.0")
    expect_true(result$found)
    expect_gte(result$count, 1L)
  })
})

test_that("package_news returns found=FALSE for unmatched version", {
  result <- tool_result_json(
    rairai:::package_news(package = "stringr", version = "99.99.99"),
    simplifyVector = FALSE
  )

  expect_equal(result$mode, "version")
  expect_false(isTRUE(result$found))
  expect_equal(result$content, "")
  expect_true(!is.null(result$available_versions))
})

test_that("package_news returns error for invalid keyword regex", {
  result <- tool_result_json(
    rairai:::package_news(keyword = "(invalid"),
    simplifyVector = FALSE
  )

  expect_equal(as.character(result$error$code), "invalid_keyword_regex")
})

test_that("local NEWS keeps precedence without bypassing read failures", {
  root <- withr::local_tempdir()
  writeLines("Package: NewsFixture", file.path(root, "DESCRIPTION"))
  markdown <- file.path(root, "NEWS.md")
  plain <- file.path(root, "NEWS")
  writeLines(c("# 2.0.0", "- Markdown change."), markdown)
  writeLines(c("# 1.0.0", "- Plain change."), plain)
  writeLines("- ChangeLog only.", file.path(root, "ChangeLog"))

  result <- tool_result_json(
    rairai:::package_news(root_path = root),
    simplifyVector = FALSE
  )
  expect_equal(result$source, "NEWS.md")
  expect_equal(result$latest$version, "2.0.0")
  expect_equal(
    tool_result_json(
      rairai:::package_news(
        "NEWSFIXTURE",
        version = " ",
        keyword = " ",
        root_path = root
      ),
      simplifyVector = FALSE
    ),
    result
  )

  unlink(markdown)
  expect_equal(
    tool_result_json(rairai:::package_news(root_path = root))$source,
    "NEWS"
  )
  dir.create(markdown)
  # A directory is an existing but unreadable first candidate, even as root.
  expect_error(
    suppressWarnings(rairai:::package_news(root_path = root)),
    class = "simpleError"
  )
  unlink(markdown, recursive = TRUE)
  unlink(plain)
  missing <- tool_result_json(rairai:::package_news(root_path = root))
  expect_equal(missing$error$code, "not_found")
  expect_equal(missing$error$message, "No NEWS.md or NEWS file found.")
})

test_that("installed NEWS supports ChangeLog without bypassing read failures", {
  root <- withr::local_tempdir()
  lib <- withr::local_tempdir()
  package <- file.path(lib, "newsfixture")
  dir.create(package)
  writeLines(
    c("Package: newsfixture", "Version: 1.0.0"),
    file.path(package, "DESCRIPTION")
  )
  withr::local_libpaths(lib)
  candidates <- file.path(package, c("NEWS.md", "NEWS", "ChangeLog"))
  for (i in seq_along(candidates)) {
    writeLines(c("# 1.0.0", paste("- Candidate", i)), candidates[[i]])
  }

  for (i in seq_along(candidates)) {
    result <- tool_result_json(
      rairai:::package_news("newsfixture", root_path = root),
      simplifyVector = FALSE
    )
    expect_equal(result$package, "newsfixture")
    expect_equal(result$source, basename(candidates[[i]]))
    expect_equal(result$latest$entries, paste("- Candidate", i))
    unlink(candidates[[i]])
    if (i == 1L) {
      dir.create(candidates[[i]])
      expect_error(
        suppressWarnings(rairai:::package_news(
          "newsfixture",
          root_path = root
        )),
        class = "simpleError"
      )
      unlink(candidates[[i]], recursive = TRUE)
    }
  }
  missing <- tool_result_json(
    rairai:::package_news("newsfixture", root_path = root)
  )
  expect_equal(missing$error$code, "not_found")
  expect_equal(
    missing$error$message,
    "No NEWS file found for package: newsfixture"
  )
})

test_that("package_news returns error for uninstalled package", {
  result <- tool_result_json(
    rairai:::package_news(package = "nonexistent_pkg_xyz"),
    simplifyVector = FALSE
  )

  expect_equal(as.character(result$error$code), "package_not_found")
})
