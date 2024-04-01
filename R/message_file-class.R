#' A constructor function for the message_fileclass
#'
#' @param x A list containing parameters for the message file, as documented in
#' message_file()
new_message_file<- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "message_file")
}

#' A validator function for the message_file class
#'
#' @param x A message_file object
validate_message_file <- function(x) {

  params <- unclass(x)

  assertList(params)
  for (param in names(params)) assertChoice(param, c("id", "created_at", "message_id"))
  qassert(params$id, "S1")
  qassert(params$created_at, "X1")
  qassert(params$message_id, "S1")

  x
}

#' A print method for the message_file class
#'
#' @param x A message_file object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.message_file <- function(x, ...) {

  cli::cli_h1("message_file object")

  print(unclass(x))
}

#' Helper to create message_file objects
#'
#' @param id The id
#' @param created_at The created_at time
#' @param message_id The message id
#'
#' @export
message_file <- function(id = NULL, created_at = NULL, message_id = NULL) {
  x <- list(id = id, created_at = created_at, message_id = message_id)
  x <- new_message_file(x)
  validate_message_file(x)
}

#' An as_message_file method for httr responses
#'
#' @param response The httr response
as_message_file.response <- function(response) {

  content <- httr::content(response)
  content$object <- NULL
  message_file <- new_message_file(content)
  message_file <- validate_message_file(message_file)
  message_file
}

as_message_file <- function(x) {
  UseMethod("as_message_file")
}
