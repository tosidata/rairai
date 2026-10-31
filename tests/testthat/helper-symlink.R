create_test_symlink <- function(target, path) {
  diagnostics <- character()
  created <- withCallingHandlers(
    file.symlink(target, path),
    warning = function(warning) {
      diagnostics <<- c(diagnostics, conditionMessage(warning))
      invokeRestart("muffleWarning")
    }
  )
  if (!isTRUE(created)) {
    reason <- paste(c("Cannot create a symlink", diagnostics), collapse = ": ")
    testthat::skip(reason)
  }
  invisible(created)
}
