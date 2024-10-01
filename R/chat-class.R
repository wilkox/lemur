#' Create a new chat object
#'
#' @param service The service to use for the chat, defaults to 'openaichat'
#' @param model The model to use for the chat, defaults to 'gpt-4o'
#' @param ... Other service-specific arguments
#'
#' @export
chat <- function(service = "openaichat", model = "gpt-4o", ...) {

  # User-friendly check of service
  if (! service %in% c("openaichat", "openaiassistant", "ollama")) {
    cli::cli_abort(c("Unknown service {.val {service}}", i = "Try {.val openaichat}, {.val openaiassistant}, or {.val ollama} instead"))
  }

  x <- list(service = service, model = model, ...)
  x <- new_chat(x)
  x <- validate_chat(x)
  x <- initialise(x)
  x
}

#' A constructor function for chat objects
#'
#' @param x A list containing parameters for the chat, including at least
#' `service` and `model`
new_chat <- function(x = list(service = character(), model = character())) {
  stopifnot(is.list(x))
  x <- structure(x, class = "chat")
  class(x) <- c(class(x), x$service)
  x
}

#' A validator function for chat
#'
#' @param x A chat object
validate_chat <- function(x) {

  values <- unclass(x)

  # Must be a list
  if (! is.list(x)) cli_abort("Chat object not a list")

  # Must contain a known service
  if (! "service" %in% names(x)) cli_abort("Chat object does not contain service")
  if (! x$service %in% c("openaichat", "openaiassistant", "ollama")) {
    cli_abort("Service {.val {x$service}} is not recognised")
  }

  # Must contain a model
  if (! "model" %in% names(x)) cli_abort("Chat object does not contain model")

  x
}

#' Initialise a chat
#'
#' Performs whatever steps and checks are needed to initialise a chat
#' for a given service. Dispatches to service-specific subclass
#' methods.
#'
#' @param chat The chat object
#' @param ... Other service-specific arguments
#'
#' @export
initialise <- function(chat, ...) {
  UseMethod("initialise")
}

#' @rdname initialise
#'
#' @export
initialise.chat <- function(chat, ...) {
  chat <- NextMethod()
  cli::cli_alert_success("Ready to chat")
  chat
}

#' Sent a message in a chat
#'
#' @param chat The chat object
#' @param content The message content
#' @param ... Other service-specific arguments
#'
#' @export
say <- function(chat, content, ...) {
  UseMethod("say")
}

#' @rdname say
#'
#' @export
say.chat <- function(chat, ...) {
  chat <- NextMethod()
  chat
}
