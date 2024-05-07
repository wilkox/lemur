#' Create an assistant
#'
#' The model will be set from the OPENAI_MODEL environmental variable. All
#' other parameters are optional.
#'
#' @return An assistant object.
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/createAssistant}
#'
#' @param name The name of the assistant, an atomic character vector of maximum
#' length 256 characters
#' @param description A description of the assistant, an atomic character
#' vector of maximum length 512 characters
#' @param instructions Instructions for the assistant, an atomic character
#' vector of maximum length 32,768 characters
#' @param tools A list of assistant_tool objects, maximum of 128
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @export
create_assistant <- function(
  name = NULL,
  description = NULL,
  instructions = NULL,
  tools = NULL,
  metadata = NULL
) {

  # Set up and validate the parameters
  params <- list(
    model = openai_model(),
    name = name,
    description = description,
    instructions = instructions,
    tools = tools,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  assistant <- new_assistant(c(list(id = character(1), created_at = integer(1)), params))
  validate_assistant(assistant)

  # Mung parameters into the format expected by the API
  if (! testNull(params$tools)) params$tools <- lapply(params$tools, unclass)
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to assistants endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/assistants",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Extract and set id and created_at
  assistant$id <- httr::content(response)$id
  assistant$created_at <- httr::content(response)$created_at
  validate_assistant(assistant)

  return(assistant)
}

#' Create an assistant file
#'
#' @return An assistant_file object.
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/createAssistantFile}
#'
#' @param assistant_id The ID of the assistant
#' @param file_id The ID of the file
#'
#' @export
create_assistant_file <- function(assistant_id, file_id) {

  # Check arguments
  qassert(assistant_id, "S1")
  qassert(file_id, "S1")

  # Set params
  params <- list(file_id = file_id)

  # POST to assistants endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}/files"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  assistant_file <- assistant_file(
    id = httr::content(response)$id,
    created_at = httr::content(response)$created_at,
    assistant_id = httr::content(response)$assistant_id
  )
  validate_assistant_file(assistant_file)

  return(assistant_file)
}

#' List assistants
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/listAssistants}
#'
#' @param limit A limit on the number of assistants to be returned. Can be
#' between 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'desc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#'
#' @return Data frame of GPT assistants
#'
#' @export
list_assistants <- function(limit = 20, order = "desc", before = NULL, after = NULL) {

  # Check arguments
  qassert(limit, "x1[0,100]")
  assertChoice(order, choices = c("asc", "desc"))
  if (! testNull(before)) qassert(before, "s1")
  if (! testNull(after)) qassert(after, "s1")

  # Set up params
  params <- list(limit = limit, order = order, before = before, after = after)
  params <- Filter(Negate(is.null), params)

  # GET from assistants endpoint
  response <- httr::GET(
    "https://api.openai.com/v1/assistants",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  assistants <- as.data.frame(do.call(rbind, response))
  return(assistants)
}

#' List files associated with an assistant
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/listAssistantFiles}
#'
#' @param assistant_id The ID of the assistant
#' @param limit A limit on the number of files to be returned. Can be between
#' 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'desc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#'
#' @return Data frame of files
#'
#' @export
list_assistant_files <- function(assistant_id, limit = 20, order = "desc", before = NULL, after = NULL) {

  # Check arguments
  qassert(assistant_id, "S1")
  qassert(limit, "x1[0,100]")
  assertChoice(order, choices = c("asc", "desc"))
  if (! testNull(before)) qassert(before, "s1")
  if (! testNull(after)) qassert(after, "s1")

  # Set up params
  params <- list(limit = limit, order = order, before = before, after = after)
  params <- Filter(Negate(is.null), params)

  # GET from assistants endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}/files"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  files <- as.data.frame(do.call(rbind, response))
  return(files)
}

#' Retrieve an assistant
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/getAssistant}
#'
#' @param assistant_id The ID of the assistant
#'
#' @return An assistant object
#'
#' @export
retrieve_assistant <- function(assistant_id) {

  # Check arguments
  qassert(assistant_id, "S1")

  # GET from assistants endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  assistant <- as_assistant(response)
  validate_assistant(assistant)
  return(assistant)
}

#' Retrieve an assistant file
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/getAssistantFile}
#'
#' @param assistant_id The ID of the assistant
#' @param file_id The ID of the assistant file
#'
#' @return An assistant_file object
#'
#' @export
retrieve_assistant_file <- function(assistant_id, file_id) {

  # Check arguments
  qassert(assistant_id, "S1")
  qassert(file_id, "S1")

  # GET from assistants endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}/files/{file_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  assistant_file <- assistant_file(
    id = httr::content(response)$id,
    created_at = httr::content(response)$created_at,
    assistant_id = httr::content(response)$assistant_id
  )
  validate_assistant_file(assistant_file)
  return(assistant_file)
}

#' Modify an assistant
#'
#' The model will be set from the OPENAI_MODEL environmental variable.
#'
#' @return An assistant object.
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/modifyAssistant}
#'
#' @param assistant_id The ID of the assistant to modify (required)
#' @param name The name of the assistant, an atomic character vector of maximum
#' length 256 characters
#' @param description A description of the assistant, an atomic character
#' vector of maximum length 512 characters
#' @param instructions Instructions for the assistance, an atomic character
#' vector of maximum length 32,768 characters
#' @param tools A list of assistant_tool objects, maximum of 128
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @export
modify_assistant <- function(
  assistant_id,
  name = NULL,
  description = NULL,
  instructions = NULL,
  tools = NULL,
  metadata = NULL
) {

  # Set up and validate the instance
  qassert(assistant_id, "S1")
  params <- list(
    model = openai_model(),
    name = name,
    description = description,
    instructions = instructions,
    tools = tools,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  assistant <- new_assistant(c(list(id = character(1), created_at = integer(1)), params))
  validate_assistant(assistant)

  # Mung parameters into the format expected by the API
  if (! testNull(params$tools)) params$tools <- lapply(params$tools, unclass)

  # POST to assistants endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)
  
  # Extract params of new assistant
  assistant <- as_assistant(response)
  validate_assistant(assistant)

  return(assistant)
}

#' Delete an assistant
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/deleteAssistant}
#'
#' @param assistant_id The ID of the assistant
#'
#' @return Nothing
#'
#' @export
delete_assistant <- function(assistant_id) {

  # Check arguments
  qassert(assistant_id, "S1")

  # DELETE to assistants endpoint
  response <- httr::DELETE(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  cli::cli_alert_success("Assistant with id {.val {assistant_id}} deleted")
}

#' Delete an assistant file
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/deleteAssistantFile}
#'
#' @param assistant_id The ID of the assistant
#' @param file_id The ID of the assistant file
#'
#' @return Nothing
#'
#' @export
delete_assistant_file <- function(assistant_id, file_id) {

  # Check arguments
  qassert(assistant_id, "S1")
  qassert(file_id, "S1")

  # DELETE to assistants endpoint
  response <- httr::DELETE(
    glue::glue("https://api.openai.com/v1/assistants/{assistant_id}/files/{file_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2")
  )

  # Check status code of response
  check_openai_response(response)

  cli::cli_alert_success("Assistant file with id {.val {file_id}} deleted from assistant with id {.val {assistant_id}}")
}

