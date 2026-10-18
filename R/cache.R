internal_cache_env <- new.env(parent = emptyenv())

cached_value <- function(key, expr, refresh = FALSE, namespace = NULL) {
  structured_key <- list(
    namespace = namespace %||% "default",
    key = key
  )
  hashed_key <- rlang::hash(structured_key)
  store <- internal_cache_env$store

  value <- if (isTRUE(refresh)) {
    cachem::key_missing()
  } else {
    store$get(hashed_key)
  }

  if (cachem::is.key_missing(value)) {
    value <- force(expr)
    store$set(hashed_key, value)
  }

  value
}

reset_internal_cache <- function() {
  internal_cache_env$store <- cachem::cache_mem()
  invisible(internal_cache_env$store)
}
