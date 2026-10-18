package_news <- function(
  package = NULL,
  version = NULL,
  keyword = NULL,
  root_path = "."
) {
  package <- normalize_help_string(package)
  version <- normalize_help_string(version)
  keyword <- normalize_help_string(keyword)

  if (!is.null(keyword)) {
    valid <- tryCatch(
      {
        str_detect("", regex(keyword, ignore_case = TRUE))
        NULL
      },
      error = function(e) e
    )
    if (inherits(valid, "error")) {
      return(tool_error_response(
        error = list(
          code = "invalid_keyword_regex",
          message = paste0("Invalid regex pattern: ", conditionMessage(valid))
        )
      ))
    }
  }

  if (
    is.null(package) ||
      identical(
        str_to_lower(package),
        str_to_lower(local_package_name(root_path = root_path) %||% "")
      )
  ) {
    result <- local_package_news(
      version,
      keyword,
      root_path = root_path
    )
  } else {
    result <- installed_package_news(package, version, keyword)
  }

  if (!is.null(result$error)) {
    return(tool_error_response(!!!result))
  }

  tool_value_response(!!!result)
}

local_package_news <- function(version, keyword, root_path = ".") {
  read_package_news(
    candidates = fs::path(root_path, c("NEWS.md", "NEWS")),
    package = local_package_name(root_path = root_path) %||% "local",
    version = version,
    keyword = keyword,
    not_found_message = "No NEWS.md or NEWS file found."
  )
}

installed_package_news <- function(package, version, keyword) {
  pkg_path <- tryCatch(find.package(package), error = function(e) NULL)
  if (is.null(pkg_path)) {
    return(list(
      error = list(
        code = "package_not_found",
        message = paste0("Package not installed: ", package)
      )
    ))
  }

  read_package_news(
    candidates = file.path(pkg_path, c("NEWS.md", "NEWS", "ChangeLog")),
    package = package,
    version = version,
    keyword = keyword,
    not_found_message = paste0("No NEWS file found for package: ", package)
  )
}

read_package_news <- function(
  candidates,
  package,
  version,
  keyword,
  not_found_message
) {
  news_paths <- candidates[file.exists(candidates)]
  if (length(news_paths) == 0L) {
    return(list(error = list(code = "not_found", message = not_found_message)))
  }

  # Read the first existing candidate; a read failure must not try later files.
  news_path <- news_paths[[1L]]
  content <- paste(readLines(news_path, warn = FALSE), collapse = "\n")
  build_package_news_response(
    package = package,
    source = fs::path_file(news_path),
    content = content,
    version = version,
    keyword = keyword
  )
}

build_package_news_response <- function(
  package,
  source,
  content,
  version,
  keyword
) {
  sections <- parse_news_sections(content, package)
  versions <- package_news_versions(sections, source)

  if (!is.null(version)) {
    section <- find_news_section(sections, version)
    if (is.null(section)) {
      return(list(
        package = package,
        source = source,
        mode = "version",
        version = version,
        found = FALSE,
        available_versions = versions,
        content = ""
      ))
    }

    if (!is.null(keyword)) {
      matches <- news_section_keyword_matches(section, keyword)
      return(list(
        package = package,
        source = source,
        mode = "version_keyword",
        version = version,
        keyword = keyword,
        found = length(matches) > 0L,
        heading = section$heading,
        count = length(matches),
        matches = matches
      ))
    }

    return(list(
      package = package,
      source = source,
      mode = "version",
      version = version,
      found = TRUE,
      heading = section$heading,
      location = format_location(
        source,
        line = section$start_line,
        end_line = section$end_line
      ),
      content = section$content
    ))
  }

  if (!is.null(keyword)) {
    groups <- map(sections, function(section) {
      map(section$entry_groups, function(group) {
        matches <- news_section_keyword_matches(group, keyword)
        if (length(matches) == 0L) {
          return(NULL)
        }
        list(
          heading = group$heading %||% NA_character_,
          version = group$version %||% NA_character_,
          count = length(matches),
          matches = matches
        )
      })
    }) |>
      unlist(recursive = FALSE, use.names = FALSE) |>
      compact()

    return(list(
      package = package,
      source = source,
      mode = "keyword",
      keyword = keyword,
      found = length(groups) > 0L,
      count = sum(map_int(groups, "count")),
      matches = groups
    ))
  }

  latest <- sections[[1]] %||%
    list(
      heading = NULL,
      version = NULL,
      start_line = NA_integer_,
      end_line = NA_integer_,
      entries = character()
    )
  returned_entries <- latest$entries[seq_len(min(length(latest$entries), 20L))]

  list(
    package = package,
    source = source,
    mode = "overview",
    latest = list(
      heading = latest$heading %||% NA_character_,
      version = latest$version %||% NA_character_,
      location = format_location(
        source,
        line = latest$start_line,
        end_line = latest$end_line
      ),
      entry_count = length(latest$entries),
      returned_count = length(returned_entries),
      truncated = length(returned_entries) < length(latest$entries),
      entries = returned_entries
    ),
    count = length(versions),
    versions = versions
  )
}

parse_news_sections <- function(content, package) {
  lines <- split_lines(content)
  heading_match <- str_match(lines, "^(#+)\\s+(\\S.*)$")
  heading_idx <- which(!is.na(heading_match[, 1]))
  headings <- map(heading_idx, function(line) {
    heading <- str_trim(heading_match[line, 3])
    list(
      line = line,
      level = nchar(heading_match[line, 2]),
      heading = heading,
      version = news_heading_version(heading, package)
    )
  })

  release_candidates <- keep(headings, \(heading) !is.null(heading$version))
  top_level_headings <- if (length(headings) > 0L) {
    top_level <- min(map_int(headings, "level"))
    keep(headings, \(heading) heading$level == top_level)
  } else {
    list()
  }
  release_headings <- keep(
    top_level_headings,
    \(heading) !is.null(heading$version)
  )

  # Releases are outline roots, or direct children of one generic document
  # title. A deeper dotted heading cannot establish a release level by itself.
  if (
    length(release_headings) == 0L &&
      length(top_level_headings) == 1L &&
      is.null(top_level_headings[[1L]]$version)
  ) {
    release_level <- top_level_headings[[1L]]$level + 1L
    release_headings <- keep(
      release_candidates,
      \(heading) heading$level == release_level
    )
  }

  build_entry_groups <- function(start, end, heading, version, nested) {
    group_headings <- c(
      list(list(line = start, heading = heading)),
      nested
    )

    map(seq_along(group_headings), function(i) {
      group <- group_headings[[i]]
      body_start <- group$line + 1L
      body_end <- if (i < length(group_headings)) {
        group_headings[[i + 1L]]$line - 1L
      } else {
        end
      }
      body_lines <- if (body_start <= body_end) {
        lines[body_start:body_end]
      } else {
        character()
      }

      list(
        heading = group$heading,
        version = version,
        entries = parse_news_entries(body_lines)
      )
    })
  }

  if (length(release_headings) == 0L) {
    entry_groups <- build_entry_groups(
      start = 0L,
      end = length(lines),
      heading = NULL,
      version = NULL,
      nested = headings
    )
    entries <- map(entry_groups, "entries") |>
      unlist(use.names = FALSE) |>
      as.character()

    return(list(list(
      heading = NULL,
      version = NULL,
      start_line = 1L,
      end_line = length(lines),
      content = paste(lines, collapse = "\n"),
      entries = entries,
      entry_groups = entry_groups
    )))
  }

  map(seq_along(release_headings), function(i) {
    release <- release_headings[[i]]
    start <- release$line
    following_siblings <- keep(headings, function(heading) {
      heading$line > start && heading$level <= release$level
    })
    # A release owns only its Markdown descendants. Generic sibling headings
    # form unattributed gaps even when another recognized release follows later.
    end <- if (length(following_siblings) > 0L) {
      following_siblings[[1L]]$line - 1L
    } else {
      length(lines)
    }
    nested <- keep(headings, function(heading) {
      heading$line > start && heading$line <= end
    })
    entry_groups <- build_entry_groups(
      start = start,
      end = end,
      heading = release$heading,
      version = release$version,
      nested = nested
    )
    entries <- map(entry_groups, "entries") |>
      unlist(use.names = FALSE) |>
      as.character()

    list(
      heading = release$heading,
      version = release$version,
      level = release$level,
      start_line = start,
      end_line = end,
      content = paste(lines[start:end], collapse = "\n"),
      entries = entries,
      entry_groups = entry_groups
    )
  })
}

parse_news_entries <- function(lines) {
  if (length(lines) == 0L) {
    return(character())
  }

  bullet_start <- str_detect(lines, "^\\s*([-*+]|[0-9]+[.)])\\s+")
  if (!any(bullet_start)) {
    text <- str_trim(paste(lines, collapse = "\n"))
    if (!nzchar(text)) {
      return(character())
    }
    return(text)
  }

  starts <- which(bullet_start)
  map_chr(seq_along(starts), function(i) {
    start <- starts[[i]]
    end <- if (i < length(starts)) {
      starts[[i + 1L]] - 1L
    } else {
      length(lines)
    }
    paste(lines[start:end], collapse = "\n") |>
      str_trim()
  })
}

news_heading_version <- function(heading, package) {
  if (is.null(heading) || !nzchar(heading)) {
    return(NULL)
  }

  dev <- str_match(
    heading,
    paste0(
      "(?i)^(?:([A-Za-z][A-Za-z0-9.]*)\\s+)?",
      "\\((development version|dev version|development|dev)\\)$"
    )
  )
  dev_package_matches <- is.na(dev[, 2]) ||
    identical(str_to_lower(dev[, 2]), str_to_lower(package))
  if (!is.na(dev[, 3]) && dev_package_matches) {
    return(dev[, 3])
  }

  version_pattern <- "([0-9]+(?:\\.[0-9]+)+(?:[-.][A-Za-z0-9]+)*)"
  standalone <- str_match(
    heading,
    paste0("(?i)^(?:(?:version|v)\\s*)?", version_pattern, "$")
  )[, 2]
  if (!is.na(standalone)) {
    return(standalone)
  }

  package_version <- str_match(
    heading,
    paste0("^([A-Za-z][A-Za-z0-9.]*)\\s+[vV]?", version_pattern, "$")
  )
  if (
    !is.na(package_version[, 3]) &&
      identical(
        str_to_lower(package_version[, 2]),
        str_to_lower(package)
      )
  ) {
    return(package_version[, 3])
  }

  NULL
}

package_news_versions <- function(sections, source) {
  map(sections, function(section) {
    list(
      heading = section$heading %||% NA_character_,
      version = section$version %||% NA_character_,
      location = format_location(
        source,
        line = section$start_line,
        end_line = section$end_line
      )
    )
  })
}

find_news_section <- function(sections, version) {
  exact <- keep(sections, function(section) {
    identical(str_to_lower(section$version %||% ""), str_to_lower(version)) ||
      identical(str_to_lower(section$heading %||% ""), str_to_lower(version))
  })
  if (length(exact) > 0L) {
    return(exact[[1L]])
  }

  partial <- keep(sections, function(section) {
    str_detect(
      section$heading %||% "",
      fixed(version, ignore_case = TRUE)
    )
  })
  if (length(partial) > 0L) {
    return(partial[[1L]])
  }

  NULL
}

news_section_keyword_matches <- function(section, keyword) {
  keyword_regex <- regex(keyword, ignore_case = TRUE)
  entries <- section$entries
  if (length(entries) == 0L) {
    return(list())
  }

  matches <- entries[str_detect(entries, keyword_regex)]
  map(matches, function(entry) {
    list(entry = entry)
  })
}

tool_package_news <- function(service) {
  mcplite::tool(
    function(
      package = NULL,
      version = NULL,
      keyword = NULL,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_news(
            package = package,
            version = version,
            keyword = keyword
          )
        }
      )
    },
    name = "package_news",
    description = paste(
      "Read package NEWS entries for release history and behavior changes."
    ),
    arguments = list(
      package = mcplite::type_string(
        "Package to inspect. Leave empty for the local source package.",
        required = FALSE
      ),
      version = mcplite::type_string(
        "NEWS version or heading to read. Leave empty for an overview.",
        required = FALSE
      ),
      keyword = mcplite::type_string(
        "Keyword or regular expression to search.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
