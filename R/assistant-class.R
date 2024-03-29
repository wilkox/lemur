#' A constructor function for the assistant class
#'
#' @param x A list containing parameters for the assistant, as documented in
#' assistant()
new_assistant <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "assistant")
}

#' A validator function for the assistant class
#'
#' @param x A assistant object
validate_assistant <- function(x) {

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "name", "model", "description", "instructions", "tools", "file_ids", "metadata"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  assertString(x$model, max.chars = 256)
  if (! testNull(x$name)) assertString(x$name, max.chars = 256)
  if (! testNull(x$description)) assertString(x$description, max.chars = 512)
  if (! testNull(x$instructions)) assertString(x$instructions, max.chars = 32768)
  if (! testNull(x$tools)) {
    assertList(x$tools, max.len = 128)
    for (tool in x$tools) assertClass(tool, "assistant_tool")
  }
  if (! testNull(x$file_ids)) assertCharacter(x$file_ids, max.len = 20)
  if (! testNull(x$metatdata)) {
    assertCharacter(x$metadata, max.len = 16, names = "named")
    for (key in names(x$metadata)) assertString(key, max.chars = 64)
    for (value in x$metadata) assertString(value, max.chars = 512)
  }

  x
}

#' A print method for the assistant class
#'
#' @param x An assistant object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.assistant <- function(x, ...) {

  cli::cli_h1("assistant object")

  print(unclass(x))
}

#' An as_assistant method for httr responses
#'
#' @param response The httr response
#'
as_assistant.response <- function(response) {

  content <- httr::content(response)
  if (! testNull(content$tools)) content$tools <- lapply(content$tools, function(tool) assistant_tool(type = tool$type))
  if (! testNull(content$object)) content$object <- NULL
  if (! testNull(content$file_ids)) content$file_ids <- as.character(content$file_ids)
  assistant <- new_assistant(content)
  assistant <- validate_assistant(assistant)
  assistant

}

#' @export
as_assistant <- function(x) {
  UseMethod("as_assistant")
}
