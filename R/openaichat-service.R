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

  chat
}
