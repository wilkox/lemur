#' Retrieve OpenAI API key from the OPENAI_API_KEY environmental variable
#'
openai_api_key <- function() {
  cli::cli_alert_info("Retrieving OpenAI API key from {.envvar OPENAI_API_KEY} environmental variable...")
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }
  cli::cli_alert_success("OpenAI API key retrieved")
  openai_api_key
}
