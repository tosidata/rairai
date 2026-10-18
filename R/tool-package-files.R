read_package_files <- function(
  scope = "R",
  include = character(),
  root_path = "."
) {
  include <- as.character(include)
  include <- include[!is.na(include) & nzchar(include)]

  scope_paths <- list(
    R = "R",
    tests = "tests",
    docs = c("man", "vignettes"),
    all = "."
  )

  if (!scope %in% names(scope_paths)) {
    return(tool_error_response(
      error = list(
        code = "invalid_scope",
        message = paste0("Unknown scope value: ", scope),
        valid = names(scope_paths)
      )
    ))
  }

  valid_include <- c("lines", "size", "modified")
  invalid_include <- setdiff(include, valid_include)
  if (length(invalid_include) > 0) {
    return(tool_error_response(
      error = list(
        code = "invalid_include",
        message = paste0(
          "Unknown include value(s): ",
          paste(invalid_include, collapse = ", ")
        ),
        valid = valid_include
      )
    ))
  }

  file_index <- package_file_index(root_path)
  files <- package_file_index_scope_files(file_index, scope_paths[[scope]])

  if (length(files) == 0) {
    return(tool_value_response(
      scope = scope,
      files = list(),
      count = 0
    ))
  }

  file_records <- map(
    files,
    package_file_record,
    include = include,
    root_path = root_path
  )
  files_data <- map(file_records, "data")
  line_errors <- file_records |>
    map("error") |>
    compact()

  response <- list(
    scope = scope,
    files = files_data,
    count = length(files_data)
  )

  if (length(include) > 0) {
    response$include <- include
  }
  if (length(line_errors) > 0) {
    response$errors <- line_errors
  }

  tool_value_response(!!!response)
}

package_file_record <- function(path, include, root_path = ".") {
  data <- list(path = path)
  error <- NULL

  if (length(include) == 0) {
    return(list(data = data, error = error))
  }

  absolute_path <- fs::path(root_path, path)
  info <- file.info(absolute_path)

  if ("lines" %in% include) {
    line_count <- package_file_line_count(absolute_path, display_path = path)
    if (!is.null(line_count$lines)) {
      data$lines <- line_count$lines
    }
    error <- line_count$error
  }

  if ("size" %in% include) {
    data$size <- unname(as.integer(info$size))
  }

  if ("modified" %in% include) {
    data$modified <- as.character(info$mtime)
  }

  list(data = data, error = error)
}

package_file_line_count <- function(path, display_path = path) {
  tryCatch(
    {
      if (package_file_is_binary(path)) {
        return(list(lines = NULL, error = NULL))
      }

      list(lines = package_file_count_lines(path), error = NULL)
    },
    error = function(cnd) {
      list(
        lines = NULL,
        error = list(
          path = fs::path_norm(display_path),
          field = "lines",
          code = "file_read_error",
          message = "Could not read file lines.",
          detail = conditionMessage(cnd)
        )
      )
    }
  )
}

package_file_count_lines <- function(path, raw_count_threshold = 1e6) {
  size <- file.info(path, extra_cols = FALSE)$size
  if (!is.na(size) && size >= raw_count_threshold) {
    return(package_file_count_raw_lines(path))
  }

  length(package_file_read_lines(path))
}

package_file_read_lines <- function(path) {
  readLines(path, warn = FALSE)
}

package_file_count_raw_lines <- function(path, chunk_size = 65536L) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  line_count <- 0L
  saw_bytes <- FALSE
  previous_byte <- raw()

  repeat {
    bytes <- readBin(connection, what = "raw", n = chunk_size)
    if (length(bytes) == 0L) {
      break
    }

    saw_bytes <- TRUE
    line_count <- line_count +
      package_file_count_raw_line_breaks(
        bytes,
        previous_byte = previous_byte
      )
    previous_byte <- bytes[[length(bytes)]]
  }

  if (!saw_bytes) {
    return(0L)
  }
  if (identical(previous_byte, charToRaw("\r"))) {
    return(line_count + 1L)
  }
  line_count + as.integer(!identical(previous_byte, charToRaw("\n")))
}

package_file_count_raw_line_breaks <- function(bytes, previous_byte = raw()) {
  lf <- charToRaw("\n")
  cr <- charToRaw("\r")

  line_breaks <- sum(bytes == lf)
  if (
    length(previous_byte) > 0L &&
      identical(previous_byte, cr) &&
      !identical(bytes[[1L]], lf)
  ) {
    line_breaks <- line_breaks + 1L
  }

  cr_positions <- which(bytes == cr)
  cr_positions <- cr_positions[cr_positions < length(bytes)]
  if (length(cr_positions) == 0L) {
    return(line_breaks)
  }

  line_breaks + sum(bytes[cr_positions + 1L] != lf)
}

package_file_is_binary <- function(path, n = 8192L) {
  connection <- file(path, open = "rb")
  on.exit(close(connection), add = TRUE)

  bytes <- readBin(connection, what = "raw", n = n)

  any(bytes == as.raw(0L))
}

package_file_index <- function(root = ".") {
  root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  repo_root <- package_file_git_repo_root(root)
  entries <- package_file_index_entries(root, root, repo_root)

  empty_index <- data.frame(
    path = character(),
    parent = character(),
    is_dir = logical(),
    is_file = logical()
  )
  if (length(entries) == 0L) {
    return(empty_index)
  }

  entry_info <- file.info(entries, extra_cols = FALSE)
  entry_is_dir <- entry_info$isdir %in% TRUE
  entry_is_file <- !is.na(entry_info$isdir) & !entry_is_dir
  keepable <- entry_is_dir | entry_is_file
  relative_paths <- entries |>
    fs::path_rel(start = root) |>
    fs::path_norm()

  if (!any(keepable)) {
    return(empty_index)
  }

  index <- data.frame(
    path = relative_paths[keepable],
    parent = dirname(relative_paths[keepable]) |>
      fs::path_norm(),
    is_dir = entry_is_dir[keepable],
    is_file = entry_is_file[keepable]
  )

  index[order(index$path), , drop = FALSE]
}

package_file_index_entries <- function(directory, root, repo_root) {
  entries <- suppressWarnings(fs::dir_ls(
    directory,
    all = TRUE,
    recurse = FALSE,
    type = "any",
    fail = FALSE
  )) |>
    unname()

  if (length(entries) == 0L) {
    return(character())
  }

  if (!is.null(repo_root)) {
    relative_paths <- entries |>
      fs::path_rel(start = root) |>
      fs::path_norm()
    ignored <- package_file_git_ignored_with_repo(
      relative_paths,
      root = root,
      repo_root = repo_root
    )
    entries <- entries[!ignored]
  }

  entry_info <- file.info(entries, extra_cols = FALSE)
  child_dirs <- entries[entry_info$isdir %in% TRUE]

  c(
    entries,
    unlist(
      map(
        child_dirs,
        package_file_index_entries,
        root = root,
        repo_root = repo_root
      ),
      use.names = FALSE
    )
  )
}

package_file_index_scope_files <- function(file_index, paths) {
  if (length(paths) == 1L && identical(paths, ".")) {
    return(file_index$path[file_index$is_file])
  }

  in_scope <- map_lgl(
    file_index$path,
    \(path) any(str_starts(path, fixed(paste0(paths, "/"))))
  )

  file_index$path[file_index$is_file & in_scope]
}

package_file_git_ignored_with_repo <- function(paths, root, repo_root) {
  if (length(paths) == 0L) {
    return(rep(FALSE, length(paths)))
  }

  if (is.null(repo_root)) {
    return(rep(FALSE, length(paths)))
  }

  package_root <- normalizePath(root, winslash = "/", mustWork = TRUE)
  repo_paths <- if (identical(package_root, repo_root)) {
    paths
  } else {
    fs::path_abs(paths, start = package_root) |>
      fs::path_rel(start = repo_root) |>
      fs::path_norm()
  }

  tryCatch(
    gert::git_ignore_path_is_ignored(repo_paths, repo = repo_root),
    error = function(cnd) rep(FALSE, length(paths))
  )
}

package_file_git_repo_root <- function(path = ".") {
  tryCatch(
    normalizePath(gert::git_info(path)$path, winslash = "/", mustWork = TRUE),
    error = function(cnd) NULL
  )
}

tool_package_files <- function(service) {
  mcplite::tool(
    function(scope = "R", include = character(), package_root = NULL) {
      root_bound_tool_call(
        service,
        package_root = package_root,
        call = function(selected_service) {
          selected_service$package_files(scope = scope, include = include)
        }
      )
    },
    name = "package_files",
    description = "List package files by development area.",
    arguments = list(
      scope = mcplite::type_string(
        "File area to list: 'R', 'tests', 'docs', or 'all'. Default: 'R'.",
        required = FALSE
      ),
      include = mcplite::type_array(
        items = mcplite::type_string(),
        description = paste(
          "Extra metadata to include: 'lines', 'size', or",
          "'modified'."
        ),
        required = FALSE
      ),
      package_root = package_root_argument()
    )
  )
}
