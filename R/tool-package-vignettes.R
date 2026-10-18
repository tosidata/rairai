package_vignettes <- function(package = NULL, name = NULL, root_path = ".") {
  package <- normalize_help_string(package)
  name <- normalize_help_string(name)

  if (
    is.null(package) ||
      identical(
        str_to_lower(package),
        str_to_lower(local_package_name(root_path = root_path) %||% "")
      )
  ) {
    result <- local_package_vignettes(name, root_path = root_path)
  } else {
    result <- installed_package_vignettes(package, name)
  }

  if (!is.null(result$error)) {
    return(tool_error_response(!!!result))
  }

  tool_value_response(!!!result)
}

local_package_vignettes <- function(name, root_path = ".") {
  package <- local_package_name(root_path = root_path) %||% "local"
  vignette_dir <- fs::path(root_path, "vignettes")

  if (!fs::dir_exists(vignette_dir)) {
    if (!is.null(name)) {
      return(list(
        error = list(
          code = "not_found",
          message = paste0(
            "No vignettes directory found. Vignette not found: ",
            name
          )
        )
      ))
    }
    return(list(
      package = package,
      source = "local",
      count = 0L,
      vignettes = list()
    ))
  }

  vig_files <- fs::dir_ls(
    vignette_dir,
    regexp = "\\.(Rmd|rmd|qmd|Rnw|Rtex)$",
    type = "file"
  )

  vig_index <- map(vig_files, function(path) {
    filename <- fs::path_file(path)
    list(
      path = path,
      name = fs::path_ext_remove(filename),
      file = fs::path("vignettes", filename)
    )
  })

  if (!is.null(name)) {
    idx <- which(str_to_lower(map_chr(vig_index, "name")) == str_to_lower(name))
    if (length(idx) == 0) {
      return(list(
        error = list(
          code = "not_found",
          message = paste0("Vignette not found: ", name)
        )
      ))
    }
    vignette <- vig_index[[idx[1]]]
    content <- paste(readLines(vignette$path, warn = FALSE), collapse = "\n")
    return(list(
      package = package,
      source = "local",
      name = vignette$name,
      file = vignette$file,
      content_type = "source",
      content = content
    ))
  }

  if (length(vig_index) == 0) {
    return(list(
      package = package,
      source = "local",
      count = 0L,
      vignettes = list()
    ))
  }

  vig_list <- map(vig_index, function(vignette) {
    list(
      name = vignette$name,
      file = vignette$file,
      title = vignette_title_from_file(vignette$path)
    )
  })

  list(
    package = package,
    source = "local",
    count = length(vig_list),
    vignettes = vig_list
  )
}

installed_package_vignettes <- function(package, name) {
  pkg_path <- tryCatch(find.package(package), error = function(e) NULL)
  if (is.null(pkg_path)) {
    return(list(
      error = list(
        code = "package_not_found",
        message = paste0("Package not installed: ", package)
      )
    ))
  }

  vigs <- installed_vignette_results(package)

  if (is.null(vigs) || nrow(vigs) == 0) {
    fallback <- installed_help_articles(package, name)
    if (!is.null(fallback)) {
      return(fallback)
    }

    if (!is.null(name)) {
      return(list(
        error = list(
          code = "not_found",
          message = glue("No vignettes found for package '{package}'.")
        )
      ))
    }
    return(list(
      package = package,
      source = "installed",
      count = 0L,
      vignettes = list()
    ))
  }

  if (!is.null(name)) {
    idx <- which(str_to_lower(vigs[, "Item"]) == str_to_lower(name))
    if (length(idx) == 0) {
      return(list(
        error = list(
          code = "not_found",
          message = glue("Vignette '{name}' not found in package '{package}'.")
        )
      ))
    }
    row <- vigs[idx[1], ]
    item <- as.character(row["Item"])
    content <- read_vignette_content(package, item)
    result <- list(
      package = package,
      source = "installed",
      name = item,
      title = as.character(row["Title"])
    )
    if (!is.null(content)) {
      result$content_type <- content$content_type
      result$content <- content$content
    } else {
      result$error <- list(
        code = "content_unavailable",
        message = "Could not read vignette content."
      )
    }
    return(result)
  }

  vig_list <- map(seq_len(nrow(vigs)), function(i) {
    list(
      name = as.character(vigs[i, "Item"]),
      title = as.character(vigs[i, "Title"])
    )
  })

  list(
    package = package,
    source = "installed",
    count = nrow(vigs),
    vignettes = vig_list
  )
}

installed_vignette_results <- function(package) {
  tryCatch(
    utils::vignette(package = package)$results,
    error = function(e) NULL
  )
}

installed_help_articles <- function(package, name = NULL) {
  articles <- help_article_candidates(package)
  if (length(articles) == 0L) {
    return(NULL)
  }

  if (is.null(name)) {
    return(list(
      package = package,
      source = "installed_help",
      document_type = "help_article",
      note = "No standard vignettes found; returned article-like help topics.",
      count = length(articles),
      vignettes = articles
    ))
  }

  idx <- which(map_lgl(articles, help_article_matches_name, name = name))
  if (length(idx) == 0L) {
    return(NULL)
  }
  if (length(idx) > 1L) {
    return(list(
      error = list(
        code = "ambiguous_name",
        message = glue(
          "Help article name '{name}' matched multiple candidates ",
          "in package '{package}'."
        ),
        candidates = I(map(articles[idx], help_article_candidate_summary))
      )
    ))
  }

  article <- articles[[idx]]
  found <- find_installed_rd_for_topic(article$topic, package)
  text <- if (is.null(found$rd)) NULL else render_rd_to_text(found$rd)

  result <- list(
    package = package,
    source = "installed_help",
    document_type = "help_article",
    note = "No standard vignettes found; returned an article-like help topic.",
    name = article$name,
    title = article$title,
    topic = article$topic,
    aliases = I(article$aliases %||% character()),
    content_type = "rendered_rd"
  )

  if (!is.null(text)) {
    result$content <- text
  } else {
    result$error <- list(
      code = "content_unavailable",
      message = "Could not render help article content."
    )
  }

  result
}

help_article_candidates <- function(package) {
  rd_db <- tryCatch(
    tools::Rd_db(package = package),
    error = function(e) NULL
  )
  if (is.null(rd_db)) {
    return(list())
  }

  index <- rd_index_from_db(rd_db, package = package, source = "installed")
  articles <- index |>
    keep(\(topic) is_article_like_help_topic(topic, package)) |>
    map(help_topic_as_article)

  keys <- map_chr(articles, \(x) {
    str_to_lower(paste(x$topic, x$title, sep = "\r"))
  })
  articles[!duplicated(keys)]
}

is_article_like_help_topic <- function(topic, package) {
  allowed_titles <- c("getting started", "quickstart", "introduction")
  topic_values <- str_to_lower(c(
    topic$topic %||% character(),
    topic$title %||% character(),
    topic$aliases %||% character()
  ))

  any(topic_values %in% allowed_titles) ||
    any(topic_values %in% package_overview_help_names(package))
}

package_overview_help_names <- function(package) {
  package <- str_to_lower(package)
  c(package, paste0(package, "-package"))
}

help_topic_as_article <- function(topic) {
  list(
    name = topic$topic,
    title = topic$title,
    topic = topic$topic,
    aliases = I(topic$aliases %||% character()),
    source = "installed_help",
    document_type = "help_article"
  )
}

help_article_candidate_summary <- function(article) {
  list(
    name = article$name,
    title = article$title,
    topic = article$topic,
    aliases = I(article$aliases %||% character())
  )
}

help_article_matches_name <- function(article, name) {
  requested <- str_to_lower(name)
  values <- str_to_lower(c(
    article$name %||% character(),
    article$title %||% character(),
    article$topic %||% character(),
    article$aliases %||% character()
  ))

  requested %in% values
}

read_vignette_content <- function(package, name) {
  pkg_path <- tryCatch(find.package(package), error = function(e) NULL)
  if (is.null(pkg_path)) {
    return(NULL)
  }

  doc_dir <- fs::path(pkg_path, "doc")
  if (!fs::dir_exists(doc_dir)) {
    return(NULL)
  }

  html_file <- fs::path(doc_dir, paste0(name, ".html"))
  if (fs::file_exists(html_file)) {
    html <-
      rvest::read_html(html_file) |>
      rvest::html_text2()
    return(list(
      content = html,
      content_type = "stripped_html"
    ))
  }

  candidates <- fs::path(doc_dir, paste0(name, c(".Rmd", ".rmd", ".qmd")))
  src_file <- candidates[fs::file_exists(candidates)] |> head(1L)
  if (length(src_file) > 0L) {
    list(
      content = paste(readLines(src_file, warn = FALSE), collapse = "\n"),
      content_type = "source"
    )
  }
}

vignette_title_from_file <- function(path) {
  fm <- tryCatch(
    frontmatter::read_front_matter(path),
    error = function(e) NULL
  )

  if (!is.null(fm) && !is.null(fm$data$title)) {
    return(as.character(fm$data$title))
  }

  lines <- tryCatch(
    readLines(path, warn = FALSE, n = 30L),
    error = function(e) character()
  )

  titles <- str_match(lines, "VignetteIndexEntry\\{([^}]+)\\}")[, 2L]
  titles <- titles[!is.na(titles)]
  if (length(titles) > 0L) {
    titles[[1L]]
  }
}

tool_package_vignettes <- function(service) {
  mcplite::tool(
    function(package = NULL, name = NULL, package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_vignettes(package = package, name = name)
        }
      )
    },
    name = "package_vignettes",
    description = "List package vignettes or read one vignette.",
    arguments = list(
      package = mcplite::type_string(
        "Package to inspect. Leave empty for the local source package.",
        required = FALSE
      ),
      name = mcplite::type_string(
        "Vignette name to read. Leave empty to list vignettes.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
