#' A constructor function for GPT_thread
#'
#' @param x A list containing parameters for the thread, as documented in
#' GPT_thread()
new_GPT_thread <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "GPT_thread")
}

#' Helper to create GPT_thread objects
#'
#' As part of creating the (local) GPT_thread object, a linked thread entity
#' will be created with the OpenAI API, unless .dry_run is set.
#'
#' API documented at
#' \url{https://platform.openai.com/docs/api-reference/threads/createThread}
#'
#' @param messages A list of GPT_message objects (not GPT_messages objects) to
#' start the thread with
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
#' @export
GPT_thread <- function(
  messages = list(),
  metadata = NULL,
  .dry_run = FALSE
) {

  # Retrieve API key
  openai_api_key <- openai_api_key()

  # Set up and validate the instance
  params <- list(
    messages = messages,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  thread <- new_GPT_thread(c(list(id = character(1), created_at = integer(1)), params))
  validate_GPT_thread(thread)

  # POST to thread endpoint
  if (.dry_run) {
    response <- data.frame(status_code = 200, content = "This is a placeholder response.")

  } else {
    response <- httr::POST(
      "https://api.openai.com/v1/threads",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::add_headers("OpenAI-Beta" = "assistants=v1"),
      httr::content_type_json(),
      body = jsonlite::toJSON(params, auto_unbox = TRUE)
    )
  }

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Extract and set id and created_at
  thread$id <- httr::content(response)$id
  thread$created_at <- httr::content(response)$created_at
  validate_GPT_thread(thread)

  thread
}

#' A validator function for GPT_thread
#'
#' @param x A GPT_thread object
#'
validate_GPT_thread <- function(x) {

  params <- unclass(x)

  # Must be a list
  if (! is.list(x)) { cli::cli_abort("Object of class GPT_thread not a list") }

  # Must not contain any unrecognised parameters
  for (param in names(x)) {
    if (! param %in% c("id", "created_at", "messages", "metadata")) {
      cli::cli_abort("Unknown parameter {param} for GPT_thread object")
    }
  }

  # Must contain id and created_at (non-user-modifiable valued set by the
  # OpenAI API)
  if (is.null(params$id)) {
    cli::cli_abort("Object of class {.cls GPT_thread} must have a {.val id} parameter")
  }
  if (! is.character(params$id) | ! length(params$id) == 1) {
    cli::cli_abort("{.cls GPT_thread} parameter {.val id} must be atomic character vector")
  }
  if (is.null(params$created_at)) {
    cli::cli_abort("Object of class {.cls GPT_thread} must have a {.val created_at} parameter")
  }
  if (! is.integer(params$created_at) | ! length(params$created_at) == 1) {
    cli::cli_abort("{.cls GPT_thread} parameter {.val created_at} must be atomic character vector")
  }

  # If there is 'messages', it must be a list of GPT_message objects
  # up to 256 characters, that only have the 'user' role
  if (! is.null(params$messages)) {
    if (! is.list(params$messages)) { 
      cli::cli_abort("{.cls GPT_thread} parameter {.val messages} must be a list of GPT_message objects")
    }
    if (length(params$messages) > 0) {
      if (! all("GPT_message" %in% unlist(lapply(params$messages, class)))) { 
        cli::cli_abort("{.cls GPT_thread} parameter {.val messages} must be a list of GPT_message objects")
      }
      if (! all(unlist(lapply(params$messages, function(m) m$role)) == "user")) { 
        cli::cli_abort("{.cls GPT_thread} parameter {.val messages} must only contain messages with the {.val user} role")
      }
    }
  }

  # If there is metadata, it must be a named character vector of up to 16
  # key-value pairs with keys up to 64 characters and values up to 512
  # characters
  if (! is.null(params$metadata)) {
    if (! is.character(params$metadata)) {
      cli::cli_abort("{.cls GPT_thread} parameter {.val metadata} must be a character vector")
    }
    if (length(params$metadata) > 16) {
      cli::cli_abort("{.cls GPT_thread} parameter {.val metadata} can have a maximum of 16 elements")
    }
    if (any(stringr::str_length(names(params$metadata)) > 64)) {
      cli::cli_abort("{.cls GPT_thread} parameter {.val metadata} names must be <= 64 characters")
    }
    if (any(stringr::str_length(params$metadata) > 512)) {
      cli::cli_abort("{.cls GPT_thread} parameter {.val metadata} values must be <= 512 characters")
    }
  }

  x
}

#' A print method for GPT_thread
#'
#' Coloured output based on
#' \url{https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r}
#'
#' @param x A GPT_thread object
#' @param get_messages Whether to get an up-to-date list of messages in the
#' thread prior from the OpenAI API prior to printing. TRUE by default
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.GPT_thread <- function(x, get_messages = TRUE, ...) {

  if (get_messages) x <- get_messages(x)

  cli::cli_h1("GPT_thread object")

  print(unclass(x))
}

#' Update a thread with the current list of messages
#'
#' @param x A GPT_thread object
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
get_messages.GPT_thread <- function(x, .dry_run = FALSE) {

  message("Getting messages!! Need to fix this!!")
  return(x)

  # Retrieve API key
  openai_api_key <- openai_api_key()

  # GET from thread endpoint
  if (.dry_run) {
    response <- data.frame(status_code = 200, content = "This is a placeholder response.")

  } else {
    response <- httr::GET(
      url = paste0("https://api.openai.com/v1/threads/", x$id, "/messages"),
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::add_headers("OpenAI-Beta" = "assistants=v1"),
      httr::content_type_json()
    )
  }

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Unpack messages into a list of GPT_message objects
  messages <- list()
  response_data <- httr::content(response)$data
  for (message in response_data) {
    m <- GPT_message(
      thread = x,
      role = message$role,
      content = message$content[[1]]$text$value,
      .dry_run = TRUE
    )
    messages <- c(messages, message)
  }

}

get_messages <- function(x, get_messages = TRUE, .dry_run = FALSE) {
  UseMethod("get_messages")
}

#' Add a message to a GPT_thread
#'
#' @param thread GPT_thread object representing the thread in which the message
#' is to be created
#' @param content The content of the message
#' @param role The role of the entity creating the message. Currently must be
#' 'user'
#' @param files A list of assistant_file objects, maximum of 10, that the message should include
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
#' @export
add_message.GPT_thread <- function(
  x,
  content,
  role = "user",
  files = NULL,
  metadata = NULL,
  .dry_run = FALSE
) {

  # Retrieve API key
  openai_api_key <- openai_api_key()

  # Check that a valid thread object has been provided
  if (is.null(x)) {
      cli::cli_abort("{.cls GPT_message} parameter {.val thread} must be a {.cls GPT_thread} object")
  }
  if (! "GPT_thread" %in% class(x)) {
      cli::cli_abort("{.cls GPT_message} parameter {.val thread} must be a {.cls GPT_thread} object")
  }

  # Check that "content" has been provided
  if (is.null(content)) { 
    cli::cli_abort("{.fun add_message} requires a {.arg content} argument")
  }
  if (! is.character(content) | ! length(content) == 1) { 
    cli::cli_abort("The {.arg content} argument to {.fun add_message} must be an atomic character vector")
  }

  # Check that "role" has been set to "user"
  if (is.null(role)) {
    cli::cli_abort("{.fun add_message} requires a {.arg role} argument")
  }
  if (! is.character(role) | ! length(role) == 1) { 
    cli::cli_abort("The {.arg role} argument to {.fun add_message} must be an atomic character vector")
  }
  if (! role == "user") { 
    cli::cli_abort("The {.arg role} argument to {.fun add_message} must be set to {.val user}")
  }

  # Check that valid file objects have been provided
  if (! is.null(files)) {
    if (is.list(files)) {
      cli::cli_abort("The {.arg files} argument to {.fun add_message} must be a list of {.cls assistant_file} objects")
    }
    if (! all("assistant_file" %in% unlist(lapply(files, class)))) {
      cli::cli_abort("The {.arg files} argument to {.fun add_message} must be a list of {.cls assistant_file} objects")
    }
  }

  # If there is metadata, it must be a named character vector of up to 16
  # key-value pairs with keys up to 64 characters and values up to 512
  # characters
  if (! is.null(metadata)) {
    if (! is.character(metadata)) {
      cli::cli_abort("The {.arg metadata} argument to {.fun add_message} must be a character vector")
    }
    if (length(metadata) > 16) {
      cli::cli_abort("The {.arg metadata} argument to {.fun add_message} can have a maximum of 16 elements")
    }
    if (any(stringr::str_length(names(metadata)) > 64)) {
      cli::cli_abort("The {.arg metadata} argument to {.fun add_message} must have names of <= 64 characters")
    }
    if (any(stringr::str_length(metadata) > 512)) {
      cli::cli_abort("The {.arg metadata} argument to {.fun add_message} must have values must <= 512 characters")
    }
  }

  # Set up parameters to POST
  params <- list(
    role = role,
    content = content,
    files_ids = unlist(lapply(files, function(f) f$id)),
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]

  # POST to threads endpoint
  if (.dry_run) {
    response <- data.frame(status_code = 200, content = "This is a placeholder response.")

  } else {
    response <- httr::POST(
      paste0("https://api.openai.com/v1/threads/", x$id, "/messages"),
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::add_headers("OpenAI-Beta" = "assistants=v1"),
      httr::content_type_json(),
      body = jsonlite::toJSON(params, auto_unbox = TRUE)
    )
  }

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Add messages to thread
  x$messages <- c(x$messages, httr::content(response)$content)

  x
}
