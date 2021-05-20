`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


vnapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, numeric(1), ...)
}


last <- function(x) {
  x[[length(x)]]
}


cometr_file <- function(...) {
  system.file(..., package = "cometr", mustWork = TRUE)
}
