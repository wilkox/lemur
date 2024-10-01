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

  # Set up transcript
  chat$transcript <- data.frame(role = character(), content = character())

  chat
}

#' Send a message in an openaichat chat
#'
#' @param chat The chat
#' @param content The message content. Text only
#' @param role The role, one of "user" (default), "system", or "assistant"
#'
#' @export
say.openaichat <- function(chat, content, role = "user") {

  # Check content
  if (! checkmate::qtest(content, "S1")) {
    cli::cli_abort("Content must be a character vector of length 1")
  }

  # Check role
  if (! role %in% c("user", "system", "assistant")) {
    cli::cli_abort("{.arg role} must be one of {.val user}, {.val system}, or {.val assistant}")
  }

  # Add message to transcript
  chat$transcript <- rbind(chat$transcript, data.frame(role = role, content = content))

  # POST to chat completion endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/chat/completions",
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(model = chat$model, messages = chat$transcript), auto_unbox = TRUE)
  )
  check_openai_response(response)

  # Extract the reply
  reply <- httr::content(response)$choices[[1]]$message$content
  chat$transcript <- rbind(chat$transcript, data.frame(role = "assistant", content = reply))

  chat
}

#' The transcript of an openaichat chat
#'
#' @param chat The chat
#'
#' @export
transcript.openaichat <- function(chat) {
  chat$transcript
}
