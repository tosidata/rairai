cran_search <- function(query, n_results = NULL) {
  query <- normalize_cran_query(query)
  if (is.null(query)) {
    return(tool_error_response(
      query = "",
      error = list(
        code = "missing_query",
        message = "Query must be a non-empty string."
      )
    ))
  }

  n_results <- min(normalize_limit_value(n_results, default = 20L), 50L)

  search_result <- tryCatch(
    pkgsearch::pkg_search(query, format = "long", size = n_results),
    error = function(cnd) {
      list(
        error = list(
          code = "cran_search_failed",
          message = "CRAN package search failed.",
          detail = conditionMessage(cnd)
        )
      )
    }
  )

  if (!is.null(search_result$error)) {
    return(tool_error_response(query = query, error = search_result$error))
  }

  metadata <- attr(search_result, "metadata") %||% list()
  results <- cran_search_results(search_result)
  total <- metadata$total %||% length(results)

  tool_value_response(
    query = query,
    n_results = n_results,
    source = list(
      backend = "pkgsearch",
      server = metadata$server %||% NA_character_
    ),
    total = total,
    returned = length(results),
    timed_out = metadata$timed_out %||% NA,
    results = I(results),
    more = list(
      tool = "cran_package",
      reason = paste(
        "Use with an exact package name for DESCRIPTION-like CRAN metadata",
        "and dependency fields."
      )
    )
  )
}

cran_package <- function(package_name) {
  package_name <- normalize_cran_query(package_name)
  if (is.null(package_name)) {
    return(tool_error_response(
      package_name = "",
      error = list(
        code = "missing_package_name",
        message = "Package name must be a non-empty string."
      )
    ))
  }

  package_result <- tryCatch(
    pkgsearch::cran_package(package_name),
    error = function(cnd) {
      list(
        error = list(
          code = cran_package_error_code(cnd),
          message = "CRAN package metadata lookup failed.",
          detail = conditionMessage(cnd)
        )
      )
    }
  )

  if (!is.null(package_result$error)) {
    return(tool_error_response(
      package_name = package_name,
      error = package_result$error
    ))
  }

  tool_value_response(description = cran_package_description(package_result))
}

normalize_cran_query <- function(query) {
  if (length(query) == 0L || is.na(query[[1L]])) {
    return(NULL)
  }

  query <- as.character(query[[1L]]) |>
    str_squish()

  if (!nzchar(query)) {
    return(NULL)
  }

  query
}

cran_search_results <- function(search_result) {
  if (nrow(search_result) == 0L) {
    return(list())
  }

  list(
    rank = seq_len(nrow(search_result)),
    score = search_result$score,
    package = search_result$package,
    version = format(search_result$version),
    title = search_result$title,
    description = search_result$description,
    date = format(search_result$date, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    downloads_last_month = search_result$downloads_last_month,
    revdeps = search_result$revdeps,
    license = search_result$license,
    url = search_result$url,
    bugreports = search_result$bugreports
  ) |>
    transpose()
}

cran_package_description <- function(package) {
  fields <- cran_pkg_desc_fields(package)

  list(
    fields = fields,
    more = cran_package_more(fields$Package)
  )
}

cran_package_more <- function(package_name) {
  if (cran_package_installed(package_name)) {
    return(list(
      installed = TRUE,
      tools = I(c(
        "package_docs",
        "help_search",
        "help_page",
        "package_news",
        "package_vignettes"
      )),
      reason = paste(
        "This package is installed. Use documentation tools for help topics,",
        "API details, release notes, and vignettes."
      )
    ))
  }

  list(
    installed = FALSE,
    reason = paste(
      "This package is not installed. Install it before using",
      "installed-package documentation tools such as package_docs,",
      "help_search, help_page, package_news, or package_vignettes."
    )
  )
}

cran_package_installed <- function(package_name) {
  if (is.null(package_name) || is.na(package_name)) {
    return(FALSE)
  }

  requireNamespace(package_name, quietly = TRUE)
}

cran_pkg_desc_fields <- function(package) {
  list(
    Package = package$Package,
    Title = package$Title,
    Version = package$Version,
    Description = package$Description,
    License = package$License,
    URL = package$URL,
    BugReports = package$BugReports,
    `Date/Publication` = package$`Date/Publication`,
    `Authors@R` = package$`Authors@R`,
    Author = package$Author,
    Maintainer = package$Maintainer,
    Depends = cran_package_dependencies(package$Depends),
    Imports = cran_package_dependencies(package$Imports),
    Suggests = cran_package_dependencies(package$Suggests),
    Enhances = cran_package_dependencies(package$Enhances),
    LinkingTo = cran_package_dependencies(package$LinkingTo)
  )
}

cran_package_dependencies <- function(deps) {
  if (length(deps) == 0L) {
    return(list())
  }

  ifelse(
    deps == "*",
    names(deps),
    sprintf("%s (%s)", names(deps), str_squish(deps))
  ) |>
    I()
}

cran_package_error_code <- function(cnd) {
  message <- conditionMessage(cnd) |>
    str_to_lower()

  if (str_detect(message, "not found|missing|not exist|not\\s+found")) {
    return("package_not_found")
  }

  "cran_package_failed"
}

tool_cran_search <- function() {
  mcplite::tool(
    cran_search,
    name = "cran_search",
    description = paste(
      "Search CRAN packages by name, title, description, author, and",
      "maintainer. Use cran_package for exact package metadata."
    ),
    arguments = list(
      query = mcplite::type_string(
        "Search query."
      ),
      n_results = mcplite::type_integer(
        "Maximum number of packages. Default: 20; maximum: 50.",
        required = FALSE
      )
    )
  )
}

tool_cran_package <- function() {
  mcplite::tool(
    cran_package,
    name = "cran_package",
    description = "Look up CRAN metadata for one exact package name.",
    arguments = list(
      package_name = mcplite::type_string(
        "Exact package name."
      )
    )
  )
}
