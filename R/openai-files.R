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

  # POST to files endpoint
  response <- httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_headers("Authorization" = paste("Bearer", openai_api_key)) |>
    httr2::req_body_json(purpose = purpose) |>
    httr2::req_body_file(file) |>
    httr2::req_perform()

  # Check status code of response
  check_openai_response(response)

  # Parse and return response
  response <- httr2::resp_body_json(response)
  cli::cli_alert_success("Uploaded with id {response$id}")
  return(response$id)
}

#' List files belonging to the user's organisation
#'
#' @export
#'
#' @return A data frame of files
list_files <- function() {

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }

  # GET from files endpoint
  response <- httr2::request("https://api.openai.com/v1/files") |>
    httr2::req_headers("Authorization" = paste("Bearer", openai_api_key)) |>
    httr2::req_perform()

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr2::resp_body_json(response)$data
  files <- as.data.frame(do.call(rbind, response))
  return(files)

}

#' Delete a file
#'
#' @references \url{https://platform.openai.com/docs/api-reference/files/delete}
#'
#' @param file_id The ID of the file to delete
#'
#' @export
#'
delete_file <- function(file_id) {

  # Check arguments
  qassert(file_id, "S1")

  # DELETE to threads endpoint
  response <- httr2::request(glue::glue("https://api.openai.com/v1/files/{file_id}")) |>
    httr2::req_method("DELETE") |>
    httr2::req_headers("Authorization" = paste("Bearer", openai_api_key()), "OpenAI-Beta" = "assistants=v1") |>
    httr2::req_perform()

  # Check status code of response
  check_openai_response(response)

  cli::cli_alert_success("File with id {.val {file_id}} deleted")

}
