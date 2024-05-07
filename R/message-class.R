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
  for (param in names(x)) assertChoice(param, c("id", "created_at", "thread_id", "role", "content", "attachments", "assistant_id", "run_id", "metadata", "file_ids"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  qassert(x$thread_id, "S1")
  assertChoice(x$role, c("user"))
  assertList(x$content, len = 2)
  assertCharacter(x$content$type)
  assertList(x$content$text)
  assertCharacter(x$content$text$value)
  if (! is.null(x$content$annotations)) assertList(x$content$annotations)
  if (! is.null(x$assistant_id)) qassert(x$assistant_id, "S1")
  if (! is.null(x$run_id)) qassert(x$run_id, "S1")
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

#' An as_message method for httr responses
#'
#' @param response The httr response
#'
as_message.response <- function(response) {

  content <- httr::content(response)
  content$object <- NULL
  content$metadata <- unlist(content$metadata)
  content$content <- content$content[[1]]
  message <- new_message(content)
  message <- validate_message(message)
  message
}

as_message <- function(x) {
  UseMethod("as_message")
}
