is_file_uri <- function(value) {
  is.character(value) &&
    length(value) > 0L &&
    str_starts(value[[1L]], fixed("file://"))
}

path_to_uri <- function(path, root_path = ".") {
  if (is_file_uri(path)) {
    return(path[[1L]])
  }

  normalized <- fs::path_abs(path[[1L]], start = root_path) |>
    fs::path_norm() |>
    as.character()
  paste0("file://", encode_file_uri_path(normalized))
}

normalize_root_relative_path <- function(
  path,
  root_path,
  argument = "path"
) {
  if (is.null(path) || (length(path) == 1L && !nzchar(path))) {
    return(NULL)
  }
  if (length(path) != 1L || is.na(path[[1L]]) || !nzchar(path[[1L]])) {
    return(tool_error_record(
      code = "invalid_path",
      message = paste0("`", argument, "` must be a single non-empty path."),
      data = list(argument = argument, path = path)
    ))
  }

  input_path <- as.character(path[[1L]])
  # The bound canonical root is authority, even after filesystem replacement.
  # Canonicalize the filter target, not the root identity it must stay inside.
  root <- root_path
  absolute_path <- if (is_file_uri(input_path)) {
    uri_to_path(input_path, root_path = root, relative = FALSE)
  } else {
    input_path
  }
  absolute_path <- normalize_absolute_path(absolute_path, root_path = root)

  if (!path_is_inside_root(absolute_path, root)) {
    return(tool_error_record(
      code = "path_outside_root",
      message = paste(
        "Path must resolve inside the effective package root.",
        "Use package_root on the package-local tool call to select a",
        "different root, and repeat it on each such call."
      ),
      data = list(
        argument = argument,
        path = input_path,
        resolved_path = absolute_path,
        root_path = root
      )
    ))
  }

  if (identical(absolute_path, root)) {
    return(".")
  }

  fs::path_rel(absolute_path, start = root) |>
    as.character()
}

normalize_absolute_path <- function(path, root_path = NULL) {
  absolute_path <- if (is.null(root_path) || fs::is_absolute_path(path)) {
    fs::path_abs(path)
  } else {
    fs::path_abs(path, start = root_path)
  }
  absolute_path <- absolute_path |>
    fs::path_norm() |>
    as.character()

  if (file.exists(absolute_path)) {
    return(normalizePath(absolute_path, winslash = "/", mustWork = TRUE))
  }

  absolute_path
}

path_is_inside_root <- function(path, root_path) {
  identical(path, root_path) || fs::path_has_parent(path, root_path)
}

contained_source_path <- function(path, root_path) {
  # Keep the bound root authority even if it or an ancestor becomes a symlink.
  # Resolve relative harness roots lexically, without adopting a changed target.
  root <- fs::path_abs(root_path) |>
    as.character()
  # Discovery and saved locations are lexical aliases, not proof of containment.
  # Use the current canonical target for reads/cache identity, not output paths.
  absolute_path <- normalize_absolute_path(path, root_path = root)
  if (path_is_inside_root(absolute_path, root)) {
    absolute_path
  }
}

uri_to_path <- function(uri, root_path = ".", relative = TRUE) {
  uri <- uri[[1L]]
  if (!is_file_uri(uri)) {
    return(uri)
  }

  absolute_path <- decode_file_uri_path(str_remove(uri, fixed("file://")))
  if (!isTRUE(relative)) {
    return(absolute_path)
  }

  root <- fs::path_abs(root_path) |>
    fs::path_norm() |>
    as.character()

  if (
    !identical(absolute_path, root) && fs::path_has_parent(absolute_path, root)
  ) {
    return(as.character(fs::path_rel(absolute_path, start = root)))
  }

  absolute_path
}

encode_file_uri_path <- function(path) {
  utils::URLencode(path, reserved = TRUE, repeated = TRUE) |>
    str_replace_all(fixed("%2F"), "/")
}

decode_file_uri_path <- function(path) {
  utils::URLdecode(path)
}
