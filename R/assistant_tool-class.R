#' A constructor function for the assistant_tool class
#'
#' @param x A list containing parameters for the assistant tool, as documented in
#' assistant_tool()
new_assistant_tool <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "assistant_tool")
}

#' A validator function for the assistant_tool class
#'
#' @param x A assistant_tool object
validate_assistant_tool <- function(x) {

  params <- unclass(x)

  assertList(params)
  for (param in names(params)) assertChoice(param, c("type"))
  qassert(params$type, "S1")
  assertChoice(params$type, c("code_interpreter", "file_search", "function"))

  x
}

#' A print method for the assistant_tool class
#'
#' @param x An assistant_tool object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.assistant_tool <- function(x, ...) {

  cli::cli_h1("assistant_tool object")

  print(unclass(x))
}

#' Helper to create assistant_tool objects
#'
#' @param type The type of tool, one of "file_search" (the default), "code_interpreter", or "function"
#' @export
assistant_tool <- function(type = "file_search") {
  x <- list(type = type)
  x <- new_assistant_tool(x)
  validate_assistant_tool(x)
}
