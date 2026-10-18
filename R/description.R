read_package_description <- function(root_path = ".", path = "DESCRIPTION") {
  absolute_path <- fs::path(root_path, path)
  if (!file.exists(absolute_path)) {
    return(list(
      description = NULL,
      error = list(
        code = "description_not_found",
        message = "DESCRIPTION file not found.",
        path = path
      )
    ))
  }

  tryCatch(
    list(
      description = read.dcf(absolute_path),
      error = NULL
    ),
    error = function(cnd) {
      list(
        description = NULL,
        error = list(
          code = "description_malformed",
          message = "DESCRIPTION file is malformed.",
          path = path,
          detail = conditionMessage(cnd)
        )
      )
    }
  )
}
