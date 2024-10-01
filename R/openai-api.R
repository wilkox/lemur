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

#' Check the status code of a response from the OpenAI API
#'
#' @param response The response from the httr call
#'
check_openai_response <- function(response) {
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }
}
