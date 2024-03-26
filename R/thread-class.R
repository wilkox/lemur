#' A constructor function for thread objects
#'
#' @param x A list containing parameters for the thread
new_thread <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "thread")
}

#' A validator function for thread objects
#'
#' @param x A thread object
#'
validate_thread <- function(x) {

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "metadata"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  assertCharacter(x$metadata, max.len = 16, max.chars = 512, names = "named")
  if (length(x$metadata) > 0) assertCharacter(names(x$metadata), max.chars = 64)

  x
}

#' A print method for thread objects
#'
#' @param x A thread object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.thread <- function(x, ...) {

  cli::cli_h1("thread object")

  print(unclass(x))
}

#' An as_thread method for httr responses
#'
#' @param response The httr response
#'
as_thread <- function(response) {

  content <- httr::content(response)
  content$object <- NULL
  content$metadata <- unlist(content$metadata)
  thread <- new_thread(content)
  thread <- validate_thread(thread)
  thread

}

