#' Upload a file
#'
#' @param file The file to upload
#' @param purpose The purpose of the file, usually either 'assistants' or 'fine-tuning'
#'
#' @export
upload_file <- function(file, purpose = "assistants") {

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }

  # Check that the file exists
  if (! fs::file_exists(file)) {
    cli::cli_abort("File {.file {file}} does not appear to exist")
  }

  # Check that valid purpose has been provided
  if (! is.character(purpose) | purpose == "" | ! length(purpose) == 1) {
    cli::cli_abort("{.arg purpose} must be an atomic character vector")
  }

  # Set up params
  params <- list(
    file = httr::upload_file(file),
    purpose = purpose
  )

  # POST to files endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/files",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
    body = params,
    encode = "multipart"
  )

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Parse and return response
  response <- httr::content(response)
  cli::cli_alert_success("Uploaded with id {response$id}")
  return(response$id)
}

#' List files belonging to the user's organisation
#'
#'
#' @export
list_files <- function() {

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }

  # GET from files endpoint
  response <- httr::GET(
    "https://api.openai.com/v1/files",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key))
  )

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Response
  response <- httr::content(response)$data
  files <- as.data.frame(do.call(rbind, response))
  return(files)

}
