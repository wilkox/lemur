#' Initialise an openaiassistant chat
#'
#' @param chat The chat object
#' @param json If TRUE, the model will respond in JSON. Defaults to FALSE
#'
#' @export
initialise.openaiassistant <- function(chat, json = FALSE) {

  cli::cli_alert_info("Initialising OpenAI assistant chat...")

  # Check and set OpenAI API key
  chat$openai_api_key <- openai_api_key()

  # Check model
  if (! chat$model %in% c("gpt-4o")) {
    cli::cli_abort("Unrecognised model {.val {chat$model}}")
  }

  # Set up messages data frame
  chat$messages <- data.frame(role = character(), content = character())

  # Set up parameters for the assistant
  params <- list(
    model = chat$model,
    # tools = tools,
    response_format = list(type = ifelse(json, "json_object", "text"))
  )

  # Mung parameters into the format expected by the API
  # if (! testNull(params$tools)) params$tools <- lapply(params$tools, unclass)

  # POST to assistants endpoint
  cli::cli_progress_step(
    "Setting up assistant...",
    msg_done = "Set up assistant"
  )
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
  cli::cli_progress_step(
    "Setting up thread...",
    msg_done = "Set up thread"
  )
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
#' @param chat The chat
#'
#' @export
print.openaiassistant <- function(chat) {
  cli::cli_dl(c(
    `JSON mode` = "{.val {chat$json}}"
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
say.openaiassistant <- function(chat, content, respond = TRUE) {

  # Check content
  if (! checkmate::qtest(content, "S1")) {
    cli::cli_abort("Content must be a character vector of length 1")
  }

  # Add message to thread
  cli::cli_progress_step("Adding message to thread...", msg_done = "Added message to thread")
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
  cli::cli_progress_step("Running assistant on thread...", msg_done = "Ran assistant on thread")
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

  chat
}

#' Get messages from an openaiassistant chat
#'
#' @param chat The chat
#'
#' @export
messages.openaiassistant <- function(chat) {

  # Set up params
  params <- list(limit = 100, order = "asc")

  # GET from threads endpoint
  cli::cli_progress_step("Retrieving messages from thread...", msg_done = "Retrieved messages from thread")
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

  cli::cli_alert_success("The thread has {cli::no(nrow(messages))} message{?s}")
  messages
}

#' Get the last response sent by the model in an openaiassistant chat
#'
#' @param chat The chat
#'
#' @export
last_response.openaiassistant <- function(chat) {
  messages <- messages(chat)
  messages <- messages[which(messages$role == "assistant"), ]
  if (nrow(messages) == 0) return(NA_character_)
  tail(messages$content, 1)
}
