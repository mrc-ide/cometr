`%||%` <- function(a, b) { # nolint
  if (is.null(a)) b else a
}


scalar <- function(x) {
  jsonlite::unbox(x)
}


vcapply <- function(X, FUN, ...) { # nolint
  vapply(X, FUN, character(1), ...)
}


list_to_numeric <- function(x) {
  vapply(x, identity, numeric(1))
}


list_to_character <- function(x) {
  vapply(x, identity, character(1))
}


last <- function(x) {
  x[[length(x)]]
}


cometr_file <- function(...) {
  system.file(..., package = "cometr", mustWork = TRUE)
}
