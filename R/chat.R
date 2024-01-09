#' Create chat completions with the OpenAI API
#'
#' API endpoint is documented at
#' \url{https://platform.openai.com/docs/api-reference/chat/create}
#'
#' @param x A GPT_messages object
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
#' @export
complete_GPT.GPT_messages <- function(x, .dry_run = FALSE) {

  # Must be a valid GPT_messages object
  x <- validate_GPT_messages(x)

  # Retrieve and set API key and model
  openai_api_key <- openai_api_key()
  model <- openai_model()

  # Set up params
  params <- list(
    model = model,
    messages = as.data.frame(x)
  )

  # POST to chat completion endpoint
  if (.dry_run) {
    response <- data.frame(status_code = 200, content = "This is a placeholder response.")

  } else {
    response <- httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
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

  # Extract and return message content
  if (.dry_run) {
    content <- response$content
  } else {
    content <- httr::content(response)$choices[[1]]$message$content
  }
  x <- add_message(x, role = "assistant", content = content)
  return(x)
}

complete_GPT <- function(x, .dry_run) {
  UseMethod("complete_GPT")
}

#' tryCatch-buffered complete_GPT
#'
#' @param x A GPT_messages object
#' @param tries How many times to try before giving up
#' @param .dry_run If TRUE, will not make an actual call to the GPT API
#'
complete_GPT_tryCatch <- function(x, tries = 3, .dry_run = FALSE) {

  while (tries > 0) {
    r <- tryCatch(
      list(result = complete_GPT(x, .dry_run = .dry_run), error = NULL),
      error = function(e) list(result = NULL, error = e)
    )
    if (is.null(r$result)) {
      cli::cli_alert_warning("GPT API call failed:")
      cli::cli_alert_warning(r$error$message)
      for (b in r$error$body) cli::cli_alert_warning(b)
      tries <- tries - 1
      if (tries > 0) cli::cli_alert_warning("Retrying ({tries} tr{?y/ies} remaining)")

    } else {
      return(r$result)
    }
  }
  cli::cli_abort("Too many failed calls to GPT API")
}

#' A print method for GPT_messages
#'
#' Coloured output based on
#' \url{https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r}
#'
#' @param x A GPT_messages object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.GPT_messages <- function(x, ...) {

  cli::cli_h1("Conversation with gpt-4")

  for (i in seq_len(length(x$role))) {
    cli::cli_div(theme = list(span.message = list(color = dplyr::case_when(x$role[i] == "system" ~ "red", x$role[i] == "user" ~ "blue", x$role[i] == "assistant" ~ "yellow", TRUE ~ "white"))))
    cli::cli_h2(stringr::str_to_upper(x$role[i]))
    msg <- stringr::str_replace_all(x$content[i], "\n", "\u000c") # Let newlines be newlines
    cli::cli_text("{.message {msg}}")
  }
}

#' A validator function for GPT_messages
#'
#' @param x A GPT_messages object
validate_GPT_messages <- function(x) {

  values <- unclass(x)

  # Must be a list
  if (! is.list(x)) { stop("Object of class GPT_messages not a list", call. = FALSE) }

  # Must contain exactly two character vectors, named "role" and "content", of
  # equal length
  if (! length(values) == 2 | ! all(vapply(values, is.character, logical(1))) | 
      ! all(names(values) %in% c("role", "content")) | 
      ! all(vapply(values, length, integer(1)) == length(values[[1]]))) { 
    stop("Object of class GPT_messages must contain exactly two character vectors, named 'role' and 'content', of equal length", call. = FALSE)
  }

  # All 'role's must be one of 'system', 'user', or 'assistant'
  if (! all(values$role %in% c("system", "user", "assistant"))) {
    stop("Object of class GPT_messages must have all values for 'role' one of 'system', 'user', or 'assistant'", call. = FALSE)
  }

  x
}

#' A constructor function for GPT_messages
#'
#' @param x A list contatining a character vector of roles and a character vector of messages
new_GPT_messages <- function(x = list(role = character(), content = character())) {
  stopifnot(is.list(x))
  stopifnot(is.character(x$role))
  stopifnot(is.character(x$content))
  structure(x, class = "GPT_messages")
}

#' Helper to create GPT_messages objects
#'
#' @param role A character vector of roles in the conversation ("system", "user" or "assistant")
#' @param content A character vector of messages in the conversation
#' @export
GPT_messages <- function(role = character(), content = character()) {
  x <- list(role = role, content = content)
  x <- new_GPT_messages(x)
  validate_GPT_messages(x)
}

#' Convert data frame to GPT_messages object
#'
#' @param df A data frame with columns 'role' and 'content' of character type
#' @export
as_GPT_messages.data.frame <- function(df = data.frame()) {
  x <- new_GPT_messages(unclass(df))
  validate_GPT_messages(x)
}

as_GPT_messages <- function(df) {
  UseMethod("as_GPT_messages")
}

#' Convert GPT_messages object to data frame
#'
#' @param x A GPT_messages object
#' @param ... Other arguments to be passed to data.frame()
#' @export
as.data.frame.GPT_messages <- function(x, ...) {
  data.frame(role = x$role, content = x$content, ...)
}

#' Add a message to a GPT_messages object
#'
#' @param x A GPT_messages object
#' @param content The content of the message
#' @param role The role of the message (defaults to "user")
#' @export
add_message.GPT_messages <- function(x, content, role = "user") {

  # Validate inputs
  if (! is.character(role) | ! length(role) == 1 | ! role %in% c("system", "user", "assistant")) {
    stop("'role' must be one of 'system', 'user' or 'assistant'", call. = FALSE)
  }
  if (! is.character(content) | ! length(content) == 1) {
    stop("'content' must be a character vector of length 1", call. = FALSE)
  }

  # Add message
  x$role <- c(x$role, role)
  x$content <- c(x$content, content)

  # Return
  return(x)

}

add_message <- function(x, content, role) {
  UseMethod("add_message")
}

#' Remove the last message from a GPT_messages object
#'
#' @param x A GPT_messages object
#' @export
remove_message.GPT_messages <- function(x) {

  # Remove the last message
  x$role <- x$role[-length(x$role)]
  x$content <- x$content[-length(x$content)]

  # Return
  return(x)

}

remove_message <- function(x) {
  UseMethod("remove_message")
}

#' Return the last message from a GPT_messages object
#'
#' @param x A GPT_messages object
#' @export
last_message.GPT_messages <- function(x) {

  # Return the last message
  return(x$content[length(x$content)])
}

last_message <- function(x) {
  UseMethod("last_message")
}

#' Convert GPT_messages object to a human-readable vector
#'
#' @param x A GPT_messages object
#' @param mode For compatibility with as.vector(); ignored
#' @export
as.vector.GPT_messages <- function(x, mode = "any") {
  vapply(
    seq_len(length(x$role)),
    function(i) {paste0(stringr::str_to_upper(x$role[i]), ": ", x$content[i])},
    character(1)
  )
}

#' A convenience function to send a message to GPT and capture the response
#'
#' @param x A GPT_messages object
#' @param content The content of the message
#' @param role The role of the message (defaults to "user")
#' @param .dry_run If TRUE, will not make an actual call to the GPT API
#' @export
say_GPT.GPT_messages <- function(x, content, role = "user", .dry_run = FALSE) {
  x <- add_message(x, role = role, content = content)
  x <- complete_GPT(x, .dry_run = .dry_run)
  x
}

say_GPT <- function(x, content, role = "user", .dry_run) {
  UseMethod("say_GPT")
}
