#' Retrieve OpenAI API key from the OPENAI_API_KEY environmental variable
#'
openai_api_key <- function() {
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }
  openai_api_key
}

#' Retrieve model from the OPENAI_MODEL environmental variable
#'
openai_model <- function() {
  model <- Sys.getenv("OPENAI_MODEL")
  if (stringr::str_length(model) == 0 | is.na(model)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_MODEL}")
  }
  model
}

#' Set model to the OPENAI_MODEL environmental variable
#'
#' @export
set_model <- function() {
  model <- "gpt-4"
  Sys.setenv(OPENAI_MODEL = model)
  msg <- paste0(
    cli::col_blue("{cli::symbol$info}"),
    " The model is set to ",
    cli::col_blue(model)
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

#' Check that the OPENAI_API_KEY environmental variable is set
#'
#' @param show_key If TRUE, and if the environmental variable is set, the key
#' will be printed to the console. Defaults to FALSE.
#'
#' @export
check_API_key <- function(show_key = FALSE) {

  key <- Sys.getenv("OPENAI_API_KEY")

  if (key == "") {
    msg <- paste0(
      cli::col_red("{cli::symbol$cross}"),
      " The {.envvar OPENAI_API_KEY} environmental variable is not set\n",
      cli::col_blue("{cli::symbol$info}"),
      " This is required for GPTscreenR to access the OpenAI API\n",
      cli::col_blue("{cli::symbol$info}"),
      " Run {.code vignette(\"api-key\")} for instructions"
    )
    rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
    return()
  }

  msg <- paste0(
    cli::col_green("{cli::symbol$tick}"),
    " The {.envvar OPENAI_API_KEY} environmental variable is set"
  )
  if (show_key) msg <- paste0(
    msg,
    "\n",
    cli::col_blue("{cli::symbol$info}"),
    " Key: ",
    "{.val {key}}"
  )
  rlang::inform(cli::format_inline(msg), class = "packageStartupMessage")
}

#' Check the status code of a response from the OpenAI API
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
