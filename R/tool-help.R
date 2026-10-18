help_page <- function(
  topic,
  package = NULL,
  sections = NULL,
  max_chars = NULL,
  root_path = "."
) {
  topic <- normalize_help_string(topic)
  if (is.null(topic)) {
    return(tool_error_response(
      error = list(
        code = "missing_topic",
        message = "Topic must be a non-empty string."
      )
    ))
  }

  requested_package <- normalize_help_string(package)
  sections <- normalize_help_sections(sections)
  max_chars <- normalize_max_chars(max_chars)

  result <- find_rd_for_topic(
    topic,
    requested_package,
    root_path = root_path
  )

  if (!is.null(result$error)) {
    return(tool_error_response(
      topic = topic,
      package = requested_package,
      error = result$error
    ))
  }

  if (is.null(result$rd)) {
    return(tool_error_response(
      topic = topic,
      package = requested_package,
      error = list(
        code = "not_found",
        message = paste0("No help page found for: ", topic)
      )
    ))
  }

  text <- render_rd_to_text(result$rd)

  if (is.null(text)) {
    return(tool_error_response(
      topic = topic,
      package = result$package,
      source = result$source,
      error = list(
        code = "render_failed",
        message = paste0("Failed to render help page for: ", topic)
      )
    ))
  }

  available_sections <- extract_section_names(text)
  matched_sections <- available_sections
  missing_sections <- character()

  if (length(sections) > 0L) {
    selected <- extract_help_sections(text, sections, available_sections)
    text <- selected$text
    matched_sections <- selected$matched_sections
    missing_sections <- selected$missing_sections
  }

  truncated <- FALSE
  if (!is.null(max_chars) && nchar(text) > max_chars) {
    text <- substr(text, 1, max_chars)
    truncated <- TRUE
  }

  tool_value_response(
    topic = result$topic %||% topic,
    package = result$package,
    source = result$source,
    title = rd_title(result$rd),
    sections = I(available_sections),
    requested_sections = I(sections),
    matched_sections = I(matched_sections),
    missing_sections = I(missing_sections),
    text = text,
    truncated = truncated
  )
}

help_search <- function(
  query,
  package = NULL,
  limit = 20L,
  offset = 0L,
  root_path = "."
) {
  query <- normalize_help_string(query)
  if (is.null(query)) {
    return(tool_error_response(
      error = list(
        code = "missing_query",
        message = "Query must be a non-empty string."
      )
    ))
  }

  package <- normalize_help_string(package)
  limit <- normalize_limit_value(limit, default = 20L)
  offset <- normalize_help_offset(offset)

  indexed <- help_search_index(query, package, root_path = root_path)
  if (!is.null(indexed$error)) {
    return(tool_error_response(error = indexed$error))
  }

  matches <- indexed$topics |>
    map(help_search_match, query = query) |>
    compact()

  matches <- help_search_deduplicate(matches)
  matches <- matches[order(
    -map_int(matches, "score"),
    map_chr(matches, "source"),
    map_chr(matches, "package"),
    map_chr(matches, "topic")
  )]

  matches <- map2(matches, seq_along(matches), function(match, i) {
    match$rank <- i
    match
  })

  total_count <- length(matches)
  if (offset >= total_count) {
    page <- list()
  } else {
    page <- matches[seq.int(offset + 1L, min(offset + limit, total_count))]
  }

  response <- list(
    query = query,
    package = package,
    search_scope = indexed$scope,
    total_count = total_count,
    count = length(page),
    offset = offset,
    limit = limit,
    has_more = offset + length(page) < total_count,
    matches = I(unname(page))
  )
  if (length(indexed$limitations %||% list()) > 0L) {
    response$limitations <- I(indexed$limitations)
  }
  if (total_count == 0L && identical(indexed$scope, "local_package")) {
    response$next_tools <- I(help_search_no_match_tools(query))
  }

  tool_value_response(!!!response)
}

help_search_no_match_tools <- function(query) {
  list(
    list(
      tool = "package_docs",
      reason = paste(
        "List local help topics, vignettes, and NEWS when a generated Rd",
        "search has no matches."
      )
    ),
    list(
      tool = "symbols",
      arguments = list(query = query, exact = FALSE),
      reason = "Search source symbols when the query may refer to tool code."
    )
  )
}

help_search_index <- function(query, package = NULL, root_path = ".") {
  indexed <- package_docs_index(package, root_path = root_path)
  if (!is.null(indexed$error)) {
    return(indexed)
  }
  if (identical(indexed$source, "installed")) {
    return(list(topics = indexed$topics, scope = "package"))
  }

  topics <- indexed$topics
  scope <- "local_package"
  # Only unscoped help search adds installed matches to the local Rd index.
  if (is.null(package)) {
    scope <- if (length(topics) > 0L) "local_plus_installed" else "installed"
    topics <- c(topics, installed_help_search_topics(query))
  }

  list(
    topics = topics,
    scope = scope,
    limitations = indexed$limitations
  )
}

installed_help_search_topics <- function(query) {
  hs <- tryCatch(
    utils::help.search(pattern = query, ignore.case = TRUE),
    error = function(e) NULL
  )
  df <- hs$matches
  if (is.null(df) || nrow(df) == 0L) {
    return(list())
  }

  key <- paste(df[, "Name"], df[, "Package"], sep = ":::")
  df <- df[!duplicated(key), , drop = FALSE]

  map(seq_len(nrow(df)), function(i) {
    title <- as.character(df[i, "Title"])
    list(
      topic = as.character(df[i, "Name"]),
      package = as.character(df[i, "Package"]),
      title = title,
      description = compact_help_description(title),
      aliases = character(),
      source = "installed",
      sections = character()
    )
  })
}

help_search_match <- function(topic, query) {
  q <- str_to_lower(query)
  words <- help_query_words(query)
  matched <- list()
  score <- 0L

  add_match <- function(field, type, points) {
    matched[[length(matched) + 1L]] <<- list(field = field, type = type)
    score <<- max(score, points)
  }

  topic_name <- str_to_lower(topic$topic %||% "")
  aliases <- str_to_lower(topic$aliases %||% character())
  title <- str_to_lower(topic$title %||% "")
  description <- str_to_lower(topic$description %||% "")

  if (identical(topic_name, q)) {
    add_match("topic", "exact", 100L)
  }
  if (q %in% aliases) {
    add_match("alias", "exact", 95L)
  }
  if (str_starts(topic_name, fixed(q))) {
    add_match("topic", "prefix", 80L)
  }
  if (any(str_starts(aliases, fixed(q)))) {
    add_match("alias", "prefix", 75L)
  }
  if (help_words_match(title, words)) {
    add_match("title", "words", 60L)
  }
  if (str_detect(title, fixed(q))) {
    add_match("title", "substring", 50L)
  }
  if (help_words_match(description, words)) {
    add_match("description", "words", 40L)
  }
  if (str_detect(description, fixed(q))) {
    add_match("description", "substring", 30L)
  }

  if (length(matched) == 0L) {
    return(NULL)
  }

  if (identical(topic$source, "local")) {
    score <- score + 1L
  }

  list(
    rank = NA_integer_,
    topic = topic$topic,
    package = topic$package,
    title = topic$title,
    description = topic$description,
    source = topic$source,
    score = score,
    matched = I(matched),
    next_tools = I(list(list(
      tool = "help_page",
      arguments = list(topic = topic$topic, package = topic$package)
    )))
  )
}

help_search_deduplicate <- function(matches) {
  if (length(matches) == 0L) {
    return(matches)
  }

  keys <- map_chr(matches, \(x) paste(x$topic, x$package, sep = ":::"))
  matches[!duplicated(keys)]
}

help_query_words <- function(query) {
  words <- str_split_1(str_to_lower(query), "[^[:alnum:]_]+")
  words[nzchar(words)]
}

help_words_match <- function(text, words) {
  if (length(words) == 0L || !nzchar(text)) {
    return(FALSE)
  }
  all(map_lgl(words, \(word) {
    str_detect(text, paste0("\\b", str_escape(word), "\\b"))
  }))
}

find_rd_for_topic <- function(topic, package = NULL, root_path = ".") {
  local_name <- local_package_name(root_path = root_path)

  if (
    is.null(package) ||
      identical(str_to_lower(package), str_to_lower(local_name %||% ""))
  ) {
    local <- find_local_rd_for_topic(
      topic,
      local_name,
      root_path = root_path
    )
    if (!is.null(local$rd)) {
      return(local)
    }
    if (!is.null(package)) {
      return(list(rd = NULL, source = NULL, package = package))
    }
  }

  if (!is.null(package)) {
    return(find_installed_rd_for_topic(topic, package))
  }

  candidates <- installed_help_candidates(topic)
  if (length(candidates) > 1L) {
    return(list(
      error = list(
        code = "ambiguous_topic",
        message = paste0(
          "Multiple help topics named or aliased '",
          topic,
          "' were found."
        ),
        candidates = I(candidates)
      )
    ))
  }
  if (length(candidates) == 1L) {
    candidate <- candidates[[1L]]
    return(find_installed_rd_for_topic(topic, candidate$package))
  }

  list(rd = NULL, source = NULL, package = package)
}

find_local_rd_for_topic <- function(
  topic,
  package = NULL,
  root_path = "."
) {
  rd_db <- tryCatch(
    tools::Rd_db(dir = root_path),
    error = function(e) NULL
  )
  if (is.null(rd_db)) {
    return(list(rd = NULL, source = NULL, package = package))
  }

  found <- find_topic_in_rd_db(topic, rd_db)
  if (is.null(found$rd)) {
    return(list(rd = NULL, source = NULL, package = package))
  }

  list(
    rd = found$rd,
    topic = found$topic,
    source = "local",
    package = package %||% "local"
  )
}

find_installed_rd_for_topic <- function(topic, package) {
  rd_db <- tryCatch(
    tools::Rd_db(package = package),
    error = function(e) NULL
  )
  if (is.null(rd_db)) {
    return(list(
      rd = NULL,
      source = NULL,
      package = package,
      error = package_not_found_error(package)
    ))
  }

  found <- find_topic_in_rd_db(topic, rd_db)
  if (is.null(found$rd)) {
    return(list(rd = NULL, source = NULL, package = package))
  }

  list(
    rd = found$rd,
    topic = found$topic,
    source = "installed",
    package = package
  )
}

installed_help_candidates <- function(topic) {
  h <- tryCatch(
    utils::help(topic, help_type = "text", try.all.packages = TRUE),
    error = function(e) NULL
  )
  paths <- as.character(h %||% character())
  if (length(paths) == 0L) {
    return(list())
  }

  candidates <- map(paths, function(path) {
    pkg_name <- basename(dirname(dirname(path)))
    rd_db <- tryCatch(
      tools::Rd_db(package = pkg_name),
      error = function(e) NULL
    )
    if (is.null(rd_db)) {
      return(NULL)
    }
    found <- find_topic_in_rd_db(topic, rd_db)
    if (is.null(found$rd)) {
      return(NULL)
    }
    list(
      topic = found$topic,
      package = pkg_name,
      title = rd_title(found$rd),
      source = "installed"
    )
  }) |>
    compact()

  keys <- map_chr(candidates, \(x) paste(x$topic, x$package, sep = ":::"))
  candidates[!duplicated(keys)]
}

find_topic_in_rd_db <- function(topic, rd_db) {
  rd_name <- paste0(topic, ".Rd")
  if (rd_name %in% names(rd_db)) {
    return(list(
      rd = rd_db[[rd_name]],
      topic = rd_topic(rd_db[[rd_name]], rd_name)
    ))
  }

  for (rd_name in names(rd_db)) {
    rd <- rd_db[[rd_name]]
    aliases <- get_rd_aliases(rd)
    if (topic %in% aliases) {
      return(list(rd = rd, topic = rd_topic(rd, rd_name)))
    }
  }

  list(rd = NULL, topic = NULL)
}

rd_index_source_result <- function(package = NULL, root_path = ".") {
  if (!dir.exists(fs::path(root_path, "man"))) {
    return(list(
      topics = list(),
      limitations = list(local_rd_limitation(
        code = "local_rd_missing",
        message = "Local generated Rd documentation directory is missing: man."
      ))
    ))
  }

  rd_db <- tryCatch(
    tools::Rd_db(dir = root_path),
    error = function(e) e
  )
  if (inherits(rd_db, "error")) {
    return(list(
      topics = list(),
      limitations = list(local_rd_limitation(
        code = "local_rd_unavailable",
        message = paste0(
          "Local generated Rd documentation could not be read: ",
          conditionMessage(rd_db)
        )
      ))
    ))
  }

  list(
    topics = rd_index_from_db(
      rd_db,
      package = package %||% "local",
      source = "local"
    ),
    limitations = list()
  )
}

local_rd_limitation <- function(code, message) {
  list(
    code = code,
    message = message,
    source = "local"
  )
}

rd_index_from_db <- function(rd_db, package, source) {
  imap(rd_db, function(rd, rd_name) {
    list(
      topic = rd_topic(rd, rd_name),
      package = package,
      title = rd_title(rd),
      description = compact_help_description(rd_description(rd)),
      aliases = get_rd_aliases(rd),
      source = source,
      sections = rd_sections(rd)
    )
  }) |>
    unname() |>
    keep(\(x) !is.null(x$topic)) |>
    sort_by_topic_package()
}

sort_by_topic_package <- function(topics) {
  if (length(topics) == 0L) {
    return(topics)
  }
  topics[order(map_chr(topics, "package"), map_chr(topics, "topic"))]
}

rd_topic <- function(rd, rd_name = NULL) {
  name <- rd_tag_text(rd, "\\name")
  if (!is.null(name)) {
    return(name)
  }
  if (!is.null(rd_name)) {
    return(tools::file_path_sans_ext(basename(rd_name)))
  }
  NULL
}

rd_title <- function(rd) {
  rd_tag_text(rd, "\\title")
}

rd_description <- function(rd) {
  rd_tag_text(rd, "\\description")
}

compact_help_description <- function(description, max_chars = 240L) {
  if (is.null(description)) {
    return(NULL)
  }
  description <- str_squish(description)
  if (nchar(description) <= max_chars) {
    return(description)
  }
  paste0(substr(description, 1L, max_chars - 3L), "...")
}

rd_tag_text <- function(rd, tag) {
  idx <- rd_tag_indices(rd, tag)
  if (length(idx) == 0L) {
    return(NULL)
  }
  rd_text(rd[[idx[[1L]]]])
}

rd_tag_indices <- function(rd, tag) {
  tags <- map_chr(rd, function(x) attr(x, "Rd_tag") %||% "")
  which(tags == tag)
}

rd_text <- function(x) {
  text <- paste(unlist(x, use.names = FALSE), collapse = "") |>
    str_squish()
  if (!nzchar(text)) {
    return(NULL)
  }
  text
}

rd_sections <- function(rd) {
  section_map <- c(
    "\\description" = "Description",
    "\\usage" = "Usage",
    "\\arguments" = "Arguments",
    "\\details" = "Details",
    "\\value" = "Value",
    "\\references" = "References",
    "\\seealso" = "See Also",
    "\\examples" = "Examples",
    "\\author" = "Author",
    "\\note" = "Note"
  )

  tags <- map_chr(rd, function(x) attr(x, "Rd_tag") %||% "")
  sections <- unname(section_map[intersect(tags, names(section_map))])

  custom <- map_chr(which(tags == "\\section"), function(i) {
    rd_text(rd[[i]][[1L]]) %||% NA_character_
  })
  custom <- custom[!is.na(custom) & nzchar(custom)]

  unique(c(sections, custom))
}

get_rd_aliases <- function(rd) {
  alias_idx <- rd_tag_indices(rd, "\\alias")

  map_chr(alias_idx, function(i) {
    rd_text(rd[[i]]) %||% ""
  }) |>
    keep(nzchar) |>
    unique()
}

render_rd_to_text <- function(rd_obj) {
  tryCatch(
    {
      tmp <- tempfile(fileext = ".txt")
      on.exit(unlink(tmp), add = TRUE)
      tools::Rd2txt(
        rd_obj,
        out = tmp,
        stages = c("build", "install", "render"),
        options = list(underline_titles = FALSE)
      )
      paste(readLines(tmp, warn = FALSE), collapse = "\n")
    },
    error = function(e) NULL
  )
}

extract_section_names <- function(text) {
  lines <- split_lines(text)
  heading_lines <- lines[str_detect(lines, "^[A-Z][A-Za-z ()]+:$")]
  str_remove(heading_lines, ":$")
}

extract_help_sections <- function(text, sections, available_sections = NULL) {
  lines <- split_lines(text)

  section_heading_idx <- which(
    str_detect(lines, "^[A-Z][A-Za-z ()]+:$")
  )

  if (length(section_heading_idx) == 0L) {
    return(list(
      text = text,
      matched_sections = character(),
      missing_sections = sections
    ))
  }

  heading_names <- str_remove(lines[section_heading_idx], ":$")
  req <- str_to_lower(sections)
  matched_sections <- heading_names[str_to_lower(heading_names) %in% req]
  missing_sections <- sections[
    !str_to_lower(sections) %in% str_to_lower(matched_sections)
  ]

  selected_lines <- character(0)
  for (i in seq_along(section_heading_idx)) {
    if (str_to_lower(heading_names[[i]]) %in% req) {
      start <- section_heading_idx[[i]]
      end <- if (i < length(section_heading_idx)) {
        section_heading_idx[[i + 1L]] - 1L
      } else {
        length(lines)
      }
      selected_lines <- c(selected_lines, lines[start:end])
    }
  }

  if (length(selected_lines) == 0L) {
    selected_text <- ""
  } else {
    selected_text <- paste(selected_lines, collapse = "\n")
  }

  list(
    text = selected_text,
    matched_sections = matched_sections,
    missing_sections = missing_sections
  )
}

local_package_name <- function(root_path = ".") {
  desc <- read_package_description(root_path = root_path)$description
  if (!"Package" %in% colnames(desc)) {
    return(NULL)
  }
  as.character(desc[1, "Package"])
}

normalize_help_string <- function(x) {
  if (length(x) == 0L) {
    return(NULL)
  }
  x <- str_trim(as.character(x[[1]]))
  if (!nzchar(x)) {
    return(NULL)
  }
  x
}

normalize_help_sections <- function(sections) {
  if (length(sections) == 0L) {
    return(character(0))
  }
  sections <- str_trim(as.character(sections))
  sections[!is.na(sections) & nzchar(sections)]
}

normalize_max_chars <- function(max_chars) {
  if (length(max_chars) == 0L) {
    return(NULL)
  }
  max_chars <- suppressWarnings(as.integer(max_chars[[1]]))
  if (is.na(max_chars) || max_chars <= 0L) {
    return(NULL)
  }
  max_chars
}

normalize_help_offset <- function(offset) {
  if (length(offset) == 0L) {
    return(0L)
  }
  offset <- suppressWarnings(as.integer(offset[[1]]))
  if (is.na(offset) || offset < 0L) {
    return(0L)
  }
  offset
}

tool_help_page <- function(service) {
  mcplite::tool(
    function(
      topic,
      package = NULL,
      sections = NULL,
      max_chars = NULL,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$help_page(
            topic = topic,
            package = package,
            sections = sections,
            max_chars = max_chars
          )
        }
      )
    },
    name = "help_page",
    description = "Read one R help page as plain text.",
    arguments = list(
      topic = mcplite::type_string(
        "Help topic or alias."
      ),
      package = mcplite::type_string(
        "Package containing the topic.",
        required = FALSE
      ),
      sections = mcplite::type_array(
        items = mcplite::type_string(),
        description = paste(
          "Help sections to include, such as Usage, Arguments, Details, Value,",
          "or Examples."
        ),
        required = FALSE
      ),
      max_chars = mcplite::type_integer(
        "Maximum number of characters to return.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}

tool_help_search <- function(service) {
  mcplite::tool(
    function(
      query,
      package = NULL,
      limit = 20L,
      offset = 0L,
      package_root = NULL
    ) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$help_search(
            query = query,
            package = package,
            limit = limit,
            offset = offset
          )
        }
      )
    },
    name = "help_search",
    description = "Search R help topics by name, alias, title, or concept.",
    arguments = list(
      query = mcplite::type_string(
        "Search query."
      ),
      package = mcplite::type_string(
        "Package to search within.",
        required = FALSE
      ),
      limit = mcplite::type_integer(
        "Maximum number of matches.",
        required = FALSE
      ),
      offset = mcplite::type_integer(
        "Number of matches to skip.",
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
