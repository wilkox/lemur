#' A constructor function for message objects
#'
#' @param x A list containing parameters for the message
new_message <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "message")
}

#' A validator function for message objects
#'
#' @param x A message object
#'
validate_message <- function(x) {

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "metadata"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  if (! is.null(x$metadata)) assertCharacter(x$metadata, max.len = 16, max.chars = 512, names = "named")
  if (! is.null(x$metadata)) assertCharacter(names(x$metadata), max.chars = 64)

  x
}

#' A print method for message objects
#'
#' @param x A message object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.message <- function(x, ...) {

  cli::cli_h1("message object")

  print(unclass(x))
}
