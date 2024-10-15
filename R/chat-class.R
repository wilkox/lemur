#' Create a new chat object
#'
#' @param service The service to use for the chat, defaults to 'openaichat'
#' @param model The model to use for the chat, defaults to 'gpt-4o'
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param ... Other service-specific arguments
#'
#' @export
chat <- function(service = "openaichat", model = "gpt-4o", .verbose = TRUE, ...) {

  # User-friendly check of service
  if (! service %in% c("openaichat", "openaiassistant", "ollama")) {
    cli::cli_abort(c("Unknown service {.val {service}}", i = "Try {.val openaichat}, {.val openaiassistant}, or {.val ollama} instead"))
  }

  x <- list(service = service, model = model, ...)
  x <- new_chat(x)
  x <- validate_chat(x)
  x <- initialise(x, .verbose = .verbose, ...)
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
  if (! is.list(x)) cli::cli_abort("Chat object not a list")

  # Must contain a known service
  if (! "service" %in% names(x)) cli::cli_abort("Chat object does not contain service")
  if (! x$service %in% c("openaichat", "openaiassistant", "ollama")) {
    cli::cli_abort("Service {.val {x$service}} is not recognised")
  }

  # Must contain a model
  if (! "model" %in% names(x)) cli::cli_abort("Chat object does not contain model")

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
#' @param .verbose If FALSE, progress messages will be suppressed
#'
#' @export
initialise.chat <- function(chat, .verbose = TRUE, ...) {
  chat <- NextMethod()
  if (.verbose) cli::cli_alert_success("Ready to chat")
  chat
}

#' Send a message in a chat
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
say.chat <- function(chat, content, .verbose = TRUE, ...) {
  chat <- NextMethod()
  chat
}

#' Get messages from a chat
#'
#' @param chat The chat object
#' @param ... Other service-specific arguments
#'
#' @return A data frame
#'
#' @export
messages <- function(chat, ...) {
  UseMethod("messages")
}

#' @rdname messages
#'
#' @param .verbose If FALSE, progress messages will be suppressed
#'
#' @export
messages.chat <- function(chat, .verbose = TRUE, ...) {
  chat <- NextMethod()
  chat
}

#' @export
print.chat <- function(x, ...) {

  n_messages <- nrow(messages(x))
  cli::cli_h1("lemur chat with {n_messages} messages")
  cli::cli_dl(c(
    Service = "{.val {x$service}}",
    Model = "{.val {x$model}}"
  ))

  NextMethod()

  cli::cli_alert_info("Use the {.fn lemur::transcript} function to view a transcript of the chat")

}

#' Print a transcript of a chat
#'
#' @param chat The chat object
#' @param ... Other service-specific arguments
#'
#' @export
transcript <- function(chat, ...) {
  UseMethod("transcript")
}

#' @rdname transcript
#'
#' @export
transcript.chat <- function(chat, ...) {
  messages <- messages(chat)

  if (nrow(messages) == 0) {
    cli::cli_alert_info("The chat is empty")
  }
  
  for (i in seq_len(nrow(messages))) {

    cli::cli_h2(messages$role[i])
    cli::cli_verbatim(messages$content[i])

  }
}

#' Get the last response sent by the model in a a chat
#'
#' @param chat The chat object
#' @param ... Other service-specific arguments
#'
#' @export
last_response <- function(chat, ...) {
  UseMethod("last_response")
}

#' @rdname last_response
#'
#' @export
last_response.chat <- function(chat, ...) {
  NextMethod()
}
