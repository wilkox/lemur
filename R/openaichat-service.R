#' Initialise an openaichat chat
#'
#' @param chat The chat object
#'
#' @export
initialise.openaichat <- function(chat) {

  cli::cli_alert_info("Initialising OpenAI chat...")

  # Check and set OpenAI API key
  chat$openai_api_key <- openai_api_key()

  # Check model
  if (! chat$model %in% c("gpt-4o")) {
    cli::cli_abort("Unrecognised model {.val {chat$model}}")
  }

  # Set up messages data frame
  chat$messages <- data.frame(role = character(), content = character())

  chat
}

#' Send a message in an openaichat chat
#'
#' @param chat The chat
#' @param content The message content. Text only
#' @param role The role, one of "user" (default) or "system"
#'
#' @export
say.openaichat <- function(chat, content, role = "user") {

  # Check content
  if (! checkmate::qtest(content, "S1")) {
    cli::cli_abort("Content must be a character vector of length 1")
  }

  # Check role
  if (! role %in% c("user", "system")) {
    cli::cli_abort("{.arg role} must be one of {.val user} or {.val system}")
  }

  # Add message to messages
  chat$messages <- rbind(chat$messages, data.frame(role = role, content = content))

  # POST to chat completion endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(model = chat$model, messages = chat$messages), auto_unbox = TRUE)
  )
  check_openai_response(response)

  # Extract the reply
  reply <- httr::content(response)$choices[[1]]$message$content
  chat$messages <- rbind(chat$messages, data.frame(role = "assistant", content = reply))

  chat
}

#' Get messages from an openaichat chat
#'
#' @param chat The chat
#'
#' @export
messages.openaichat <- function(chat) {
  chat$messages
}

#' Print information about an openaichat chat
#'
#' @param chat The chat
#'
#' @export
print.openaichat <- function(chat) {
  chat
}

#' Get the last response sent by the model in an openaichat chat
#'
#' @param chat The chat
#'
#' @export
last_response.openaichat <- function(chat) {
  messages <- messages(chat)
  messages <- messages[which(messages$role == "assistant"), ]
  if (nrow(messages) == 0) return(NA_character_)
  tail(messages$content, 1)
}
