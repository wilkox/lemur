#' A constructor function for the assistant_file class
#'
#' @param x A list containing parameters for the assistant file, as documented in
#' assistant_file()
new_assistant_file <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "assistant_file")
}

#' A validator function for the assistant_file class
#'
#' @param x A assistant_file object
validate_assistant_file <- function(x) {

  params <- unclass(x)

  assertList(params)
  for (param in names(params)) assertChoice(param, c("id", "created_at", "assistant_id"))
  qassert(params$id, "S1")
  qassert(params$created_at, "X1")
  qassert(params$assistant_id, "S1")

  x
}

#' A print method for the assistant_file class
#'
#' @param x An assistant_file object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.assistant_file <- function(x, ...) {

  cli::cli_h1("assistant_file object")

  print(unclass(x))
}

#' Helper to create assistant_file objects
#'
#' @param type The type of file, one of "retrieval" (the default), "code_interpreter", or "function"
#' @export
assistant_file <- function(id = NULL, created_at = NULL, assistant_id = NULL) {
  x <- list(id = id, created_at = created_at, assistant_id = assistant_id)
  x <- new_assistant_file(x)
  validate_assistant_file(x)
}
