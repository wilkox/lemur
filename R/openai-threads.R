#' Create a thread.
#'
#' @references \url{https://platform.openai.com/docs/api-reference/threads/createThread}
#'
#' @param messages A list of message objects, to start the thread with (optional)
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @return A thread object
#'
#' @export
create_thread <- function(messages = NULL, metadata = NULL) {

  # Set up and validate the arguments
  params <- list(
    messages = messages,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  if (! is.null(messages)) {
    assertList(messages)
    if (length(messages) > 0) for (m in messages) assertClass(m, "message")
  }
  if (! is.null(metadata)) {
    assertCharacter(metadata, max.len = 16, max.chars = 512, names = "named")
    if (length(metadata) > 0) assertCharacter(names(metadata), max.chars = 64)
  }

  # Mung parameters into the format expected by the API
  if (! testNull(params$messages)) params$messages <- lapply(params$messages, unclass)
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to thread endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/threads",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Create and return thread object
  thread <- as_thread(response)
  validate_thread(thread)

  return(thread)

}

#' Retrieve a thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/threads/getThread}
#'
#' @param thread_id The ID of the thread
#'
#' @return A thread object
#'
#' @export
retrieve_thread <- function(thread_id) {

  # Check arguments
  qassert(thread_id, "S1")

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  thread <- as_thread(response)
  validate_thread(thread)
  return(thread)
}

#' Modify a thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/threads/modifyThread}
#'
#' @param thread_id The ID of the thread
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @return A thread object
#'
#' @export
modify_thread <- function(thread_id, metadata = NULL) {

  # Check arguments
  qassert(thread_id, "S1")

  # Set up params
  params <- list(metadata = metadata)
  params <- params[! unlist(lapply(params, is.null))]
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  thread <- as_thread(response)
  validate_thread(thread)
  return(thread)
}

#' Delete a thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/threads/deleteThread}
#'
#' @param thread_id The ID of the thread
#'
#' @return Nothing
#'
#' @export
delete_thread <- function(thread_id) {

  # Check arguments
  qassert(thread_id, "S1")

  # DELETE to threads endpoint
  response <- httr::DELETE(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  cli::cli_alert_success("Thread with id {.val {thread_id}} deleted")
}
