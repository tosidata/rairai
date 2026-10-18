package_docs <- function(
  package = NULL,
  limit = 20L,
  offset = 0L,
  root_path = "."
) {
  package <- normalize_help_string(package)
  limit <- normalize_limit_value(limit, default = 20L)
  offset <- normalize_help_offset(offset)

  doc_index <- package_docs_index(package, root_path = root_path)
  if (!is.null(doc_index$error)) {
    return(tool_error_response(error = doc_index$error))
  }

  topics <- doc_index$topics
  total_count <- length(topics)
  if (offset >= total_count) {
    page <- list()
  } else {
    page <- topics[seq.int(offset + 1L, min(offset + limit, total_count))]
  }

  help <- list(
    total_count = total_count,
    count = length(page),
    offset = offset,
    limit = limit,
    has_more = offset + length(page) < total_count,
    topics = I(unname(map(page, package_docs_topic)))
  )
  if (length(doc_index$limitations %||% list()) > 0L) {
    help$limitations <- I(doc_index$limitations)
  }

  tool_value_response(
    package = doc_index$package,
    source = doc_index$source,
    help = help,
    vignettes = package_docs_vignettes_summary(
      doc_index$package,
      doc_index$source,
      root_path = root_path
    ),
    news = package_docs_news_summary(
      doc_index$package,
      doc_index$source,
      root_path = root_path
    ),
    next_tools = I(package_docs_next_tools(doc_index$package))
  )
}

package_docs_index <- function(package = NULL, root_path = ".") {
  local_name <- local_package_name(root_path = root_path)

  if (
    is.null(package) ||
      identical(str_to_lower(package), str_to_lower(local_name %||% ""))
  ) {
    local_index <- rd_index_source_result(
      package = local_name,
      root_path = root_path
    )
    return(list(
      package = local_name %||% "local",
      source = "local",
      topics = local_index$topics,
      limitations = local_index$limitations
    ))
  }

  rd_db <- tryCatch(
    tools::Rd_db(package = package),
    error = function(e) NULL
  )
  if (is.null(rd_db)) {
    return(list(error = package_not_found_error(package)))
  }

  list(
    package = package,
    source = "installed",
    topics = rd_index_from_db(rd_db, package = package, source = "installed")
  )
}

package_docs_topic <- function(topic) {
  out <- list(
    topic = topic$topic,
    title = topic$title,
    aliases = I(topic$aliases)
  )
  if (length(topic$sections %||% character()) > 0L) {
    out$sections <- I(topic$sections)
  }
  out
}

package_docs_vignettes_summary <- function(package, source, root_path = ".") {
  result <- if (identical(source, "local")) {
    local_package_vignettes(name = NULL, root_path = root_path)
  } else {
    installed_package_vignettes(package = package, name = NULL)
  }

  if (!is.null(result$error)) {
    return(list(
      available = FALSE,
      count = 0L,
      error = result$error
    ))
  }

  list(
    available = isTRUE((result$count %||% 0L) > 0L),
    count = result$count %||% 0L,
    items = I(result$vignettes %||% list())
  )
}

package_docs_news_summary <- function(package, source, root_path = ".") {
  result <- if (identical(source, "local")) {
    local_package_news(
      version = NULL,
      keyword = NULL,
      root_path = root_path
    )
  } else {
    installed_package_news(package = package, version = NULL, keyword = NULL)
  }

  if (!is.null(result$error)) {
    return(list(
      available = FALSE,
      error = result$error
    ))
  }

  list(
    available = TRUE,
    source = result$source,
    latest_version = result$latest$version,
    latest_heading = result$latest$heading,
    version_count = result$count %||% 0L
  )
}

package_docs_next_tools <- function(package) {
  list(
    list(
      tool = "help_page",
      reason = "Read one help topic from the documentation map.",
      arguments = list(topic = "<topic>", package = package)
    ),
    list(
      tool = "help_search",
      reason = "Search this package's help topics by name, alias, title, or concept.",
      arguments = list(query = "<query>", package = package)
    ),
    list(
      tool = "package_vignettes",
      reason = "Retrieve long-form package documentation.",
      arguments = list(package = package)
    ),
    list(
      tool = "package_news",
      reason = "Inspect release history or behavior changes.",
      arguments = list(package = package)
    )
  )
}

tool_package_docs <- function(service) {
  mcplite::tool(
    function(
      package = NULL,
      limit = 20L,
      offset = 0L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_docs(
            package = package,
            limit = limit,
            offset = offset
          )
        }
      )
    },
    name = "package_docs",
    description = paste(
      "Summarize package documentation, including help pages, aliases, titles,",
      "vignettes, and NEWS."
    ),
    arguments = list(
      package = mcplite::type_string(
        "Package to inspect. Leave empty for the local source package.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of help topics.",
        required = FALSE
      ),
      offset = mcplite::type_integer(
        "Number of help topics to skip.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
