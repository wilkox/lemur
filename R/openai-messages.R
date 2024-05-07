#' Create a message within a thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/messages/createMessage}
#'
#' @param thread_id The ID of the thread to create the message in (required)
#' @param role The role of the entity that is creating the message, at present must be "user" (required)
#' @param content The content of the message (required)
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @return A thread object
#'
#' @export
create_message <- function(
  thread_id,
  role = "user",
  content,
  metadata = NULL
) {

  # Set up and validate the arguments
  params <- list(
    role = role,
    content = content,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  qassert(thread_id, "S1")
  assertChoice(role, c("user"))
  qassert(content, "S1")
  if (! is.null(metadata)) {
    assertCharacter(metadata, max.len = 16, max.chars = 512, names = "named")
    if (length(metadata) > 0) assertCharacter(names(metadata), max.chars = 64)
  }

  # Mung parameters into the format expected by the API
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/messages"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Create and return message object
  message <- as_message(response)
  validate_message(message)

  return(message)
}

#' List messages
#'
#' List messages for a given thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/messages/listMessages}
#'
#' @param thread_id The ID of the thread to list messages for (required)
#' @param limit A limit on the number of assistants to be returned. Can be
#' between 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'desc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#'
#' @return Data frame of messages
#'
#' @export
list_messages <- function(thread_id, limit = 20, order = "desc", after = NULL, before = NULL) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(limit, "x1[0,100]")
  assertChoice(order, choices = c("asc", "desc"))
  if (! testNull(before)) qassert(before, "s1")
  if (! testNull(after)) qassert(after, "s1")

  # Set up params
  params <- list(limit = limit, order = order, before = before, after = after)
  params <- Filter(Negate(is.null), params)

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/messages"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  messages <- as.data.frame(do.call(rbind, response))
  return(messages)
}

#' Retrieve a message
#'
#' Retrieve a message
#'
#' @references \url{https://platform.openai.com/docs/api-reference/messages/getMessage}
#'
#' @param thread_id The ID of the thread (required)
#' @param message_id The ID of the message (required)
#'
#' @return A message object
#'
#' @export
retrieve_message <- function(thread_id, message_id) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(message_id, "S1")

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  message <- as_message(response)
  return(message)
}

#' Modify a message within a thread
#'
#' @references \url{https://platform.openai.com/docs/api-reference/messages/modifyMessage}
#'
#' @param thread_id The ID of the thread to create the message in (required)
#' @param message_id The ID of the message (required)
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @return A message object
#'
modify_message <- function(
  thread_id,
  message_id,
  metadata = NULL
) {

  # Set up and validate the arguments
  params <- list(
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  qassert(thread_id, "S1")
  qassert(message_id, "S1")
  if (! is.null(metadata)) {
    assertCharacter(metadata, max.len = 16, max.chars = 512, names = "named")
    if (length(metadata) > 0) assertCharacter(names(metadata), max.chars = 64)
  }

  # Mung parameters into the format expected by the API
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/messages/{message_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)
  response <<- response

  # Create and return message object
  message <- as_message(response)
  validate_message(message)

  return(message)
}
