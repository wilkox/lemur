#' Initialise an openaiassistant chat
#'
#' @param chat The chat object
#' @param json_schema (optional) A JSON schema that the response must strictly
#' adhere to
#' @param paths (optional) Paths to files to be used with the file_search tool;
#' they will be uploaded to a vector store
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param ... Further arguments passed from other methods
#'
#' @export
initialise.openaiassistant <- function(chat, json_schema = NULL, paths = NULL, .verbose = TRUE, ...) {

  if (.verbose) cli::cli_alert_info("Initialising OpenAI assistant chat...")

  # Check and set OpenAI API key
  chat$openai_api_key <- openai_api_key()

  # Set up messages data frame
  chat$messages <- data.frame(role = character(), content = character())

  # Set up parameters for the assistant
  if (is.null(json_schema)) {
    response_format <- "auto"
    chat$json_schema <- FALSE
  } else {
    response_format <- list(type = "json_schema", "json_schema" = list(
      name = "JSON_response_schema",
      schema = jsonlite::fromJSON(json_schema),
      strict = TRUE
    ))
    chat$json_schema <- json_schema
  }
  params <- list(
    model = chat$model,
    response_format = response_format
  )
  if (! is.null(paths)) {
    params <- c(
      params,
      list(
        tools = data.frame(type = "file_search"),
        tool_resources = list(file_search = list(vector_store_ids = I(create_vector_store(paths))))
      )
    )
  }

  # POST to assistants endpoint
  if (.verbose) cli::cli_alert_info("Setting up assistant...")
  response <- httr::POST(
    "https://api.openai.com/v1/assistants",
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Extract and set assistant ID
  chat$assistant_id <- httr::content(response)$id

  # POST to thread endpoint
  if (.verbose) cli::cli_alert_info("Setting up thread...")
  response <- httr::POST(
    "https://api.openai.com/v1/threads",
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json()
  )

  # Check status code of response
  check_openai_response(response)

  # Extract and set thread ID
  chat$thread_id <- httr::content(response)$id

  chat
}

#' Print information about an openaiassistant chat
#'
#' @param x The chat
#' @param ... Further arguments passed from other methods
#'
#' @export
print.openaiassistant <- function(x, ...) {
  cli::cli_dl(c(
    `JSON schema` = "{.val {ifelse(is.logical(x$json_schema), x$json_schema, TRUE)}}"
  ))
  chat
}

#' @rdname say
#'
#' @param chat The chat
#' @param content The message content. Text only
#' @param respond If TRUE (the default), after sending the message the
#' assistant will be run to generate a response. Setting `respond = FALSE`
#' allows sending multiple messages to the assistant before it is asked to
#' respond
#'
#' @export
say.openaiassistant <- function(chat, content, respond = TRUE, .verbose = TRUE, ...) {

  # Check content
  if (! checkmate::qtest(content, "S1")) {
    cli::cli_abort("Content must be a character vector of length 1")
  }

  # Add message to thread
  if (.verbose) cli::cli_alert_info("Adding message to thread...")
  params <- list(
    role = "user",
    content = content
  )

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{chat$thread_id}/messages"),
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # If no response is needed, can finish here
  if (! respond) {
    return(chat)
  }

  # Set up and validate the parameters
  if (.verbose) cli::cli_alert_info("Running assistant on thread...")
  params <- list(
    model = chat$model,
    assistant_id = chat$assistant_id,
    # tools = tools,
    temperature = 1,
    stream = FALSE
  )
  params <- params[! unlist(lapply(params, is.null))]

  # Mung parameters into the format expected by the API
  # if (! testNull(params$tools)) params$tools <- lapply(params$tools, unclass)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{chat$thread_id}/runs"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Get run id
  response <- response |> httr::content() 
  chat$run_id <- response$id

  # Wait for the run status to be 'completed'
  run_status <- "pending"
  while (! run_status == "completed") {
    Sys.sleep(1)
    run <- httr::GET(
      glue::glue("https://api.openai.com/v1/threads/{chat$thread_id}/runs/{chat$run_id}"),
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
      httr::add_headers("OpenAI-Beta" = "assistants=v2")
    ) |> httr::content()
    run_status <- run$status
    if (.verbose) cli::cli_alert_info("Run status is {.val {run_status}}...")
    if (run_status == "failed") cli::cli_abort("Run failed")
  }
  if (.verbose) cli::cli_alert_success("Run complete")

  chat
}

#' Get messages from an openaiassistant chat
#'
#' @param chat The chat
#' @param .verbose If FALSE, progress messages will be suppressed
#' @param ... Further arguments passed from other methods
#'
#' @export
messages.openaiassistant <- function(chat, .verbose = TRUE, ...) {

  # Set up params
  params <- list(limit = 100, order = "asc")

  # GET from threads endpoint
  if (.verbose) cli::cli_alert_info("Retrieving messages from thread...")
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{chat$thread_id}/messages"),
    httr::add_headers("Authorization" = paste("Bearer", chat$openai_api_key)),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  messages <- as.data.frame(do.call(rbind, response))

  # Clean up response
  if (nrow(messages) == 0) {
    messages <- data.frame(role = character(), content = character())
  } else {
    messages <- messages[, c("role", "content")]
    messages$role <- messages$role |> unlist()
    messages$content <- messages$content |>
      lapply(function(x) x[[1]]$text$value) |>
      unlist()
  }

  # Threads with >= 100 messages will require pagination
  if (nrow(messages) >= 100) {
    cli::cli_abort("OpenAI assistants threads with 100 or more messages are not yet supported by lemur")
  }

  if (.verbose) cli::cli_alert_success("The thread has {cli::no(nrow(messages))} message{?s}")
  messages
}

#' Get the last response sent by the model in an openaiassistant chat
#'
#' @param chat The chat
#' @param ... Further arguments passed from other methods
#'
#' @export
last_response.openaiassistant <- function(chat, ...) {
  messages <- messages(chat)
  messages <- messages[which(messages$role == "assistant"), ]
  if (nrow(messages) == 0) return(NA_character_)
  utils::tail(messages$content, 1)
}

#' Create a vector store with associated file(s), to use with the file_search
#' tool
#'
#' @param paths List of paths to files to be attached to the vector store
create_vector_store <- function(paths) {

  # Check that paths are valid
  if (length(paths) == 0) { cli::cli_abort("Must specify at least one file path") }
  for (path in paths) {
    if (! fs::file_exists(path)) {
      cli::cli_abort("Path {path} does not exist")
    }
  }

  # Upload each file
  file_ids <- vapply(paths, function(path) {

    cli::cli_alert_info("Uploading {path} to vector store...")

    response <- httr::POST(
      "https://api.openai.com/v1/files",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
      httr::add_headers("OpenAI-Beta" = "assistants=v2"),
      body = list(file = httr::upload_file(path), purpose = "assistants"),
      endcode = "multipart"
    )

    # Check status code of response
    check_openai_response(response)

    # Get file id
    response <- response |> httr::content() 
    response$id

  }, character(1))

  # Create vector store
  response <- httr::POST(
    "https://api.openai.com/v1/vector_stores",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v2"),
    httr::content_type_json(),
    body = jsonlite::toJSON(list(file_ids = unname(file_ids)))
  )

  # Check status code of response
  check_openai_response(response)

  # Return the vector store ID
  response <- response |> httr::content()
  return(response$id)
}
