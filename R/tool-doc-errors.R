package_not_found_error <- function(package) {
  list(
    code = "package_not_found",
    message = paste0(
      "Package not installed or has no Rd database: ",
      package
    ),
    note = "Install the package in the R environment running rairai, then retry.",
    install_hint = paste0(
      "install.packages(",
      encodeString(package, quote = "\""),
      ")"
    )
  )
}
