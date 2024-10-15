#' @rdname initialise
#'
#' @export
initialise.ollama <- function(chat, .verbose = TRUE, ...) {

  if (.verbose) cli::cli_alert_info("Initialising ollama chat with model {.val {chat$model}}...")

  # Check if ollama server is running
  invisible(olist <- system2("ollama", "list", stderr = TRUE, stdout = TRUE))
  if (is.null(attr(olist, "status"))) {
    if (.verbose) cli::cli_alert_success("ollama server running")
  } else if (attr(olist, "status") == 1) {
    if (.verbose) cli::cli_alert_info("ollama server not running, starting with {.code ollama serve}...")
    invisible(system2("ollama", "serve", stdout = FALSE, stderr = FALSE, wait = FALSE))
    invisible(olist <- system2("ollama", "list", stderr = TRUE, stdout = TRUE))
    if (attr(olist, "status") == 1) {
      cli::cli_abort("Failed to launch ollama server")
    }
  } else {
    cli::cli_abort("Unrecognised ollama status")
  }

  # Load the model into memory
  if (.verbose) cli::cli_alert_info("Loading model {.val {chat$model}} into memory...")
  response <- httr::POST(
    "http://localhost:11434/api/generate",
    httr::content_type_json(),
    body = jsonlite::toJSON(list(model = jsonlite::unbox(chat$model)))
  )
  if (is.null(httr::content(response)$done)) {
    cli::cli_abort(c(
      "Failed to load model {.val {chat$model}}.",
      i = "Has it been downloaded?",
      i = "Try running {.code ollama pull {chat$model}} at the command line"
    ))
  } else if (httr::content(response)$done) {
    if (.verbose) cli::cli_alert_success("Model loaded")
  } else {
    cli::cli_abort("Unexpected response from model load")
  }

  # Set up messages data frame
  chat$messages <- data.frame(role = character(), content = character())

  return(chat)
}

#' Check the status code of a response from the ollama API
#'
#' @param response The response from the httr call
#'
check_ollama_response <- function(response) {
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The ollama API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }
}

#' @rdname say
#'
#' @param ... Further arguments passed from other methods
#'
#' @export
say.ollama <- function(chat, content, .verbose = TRUE, ...) {

  # Check content
  if (! checkmate::qtest(content, "S1")) {
    cli::cli_abort("Content must be a character vector of length 1")
  }

  # Add message to messages
  chat$messages <- rbind(chat$messages, data.frame(role = "user", content = content))

  # POST chat request
  if (.verbose) cli::cli_alert_info("Sending message...")
  response <- httr::POST(
    "http://localhost:11434/api/chat",
    httr::content_type_json(),
    body = jsonlite::toJSON(list(
      model = jsonlite::unbox(chat$model),
      messages = lapply(
        chat$messages |> nrow() |> seq_len(),
        function(i) { list(
          role = chat$messages$role[i] |> jsonlite::unbox(),
          content = chat$messages$content[i] |> jsonlite::unbox()
        ) }
      ),
      stream = jsonlite::unbox(FALSE)
    ))
  )
  check_ollama_response(response)

  # Extract the reply
  reply <- httr::content(response)$message$content
  chat$messages <- rbind(chat$messages, data.frame(role = "assistant", content = reply))
  if (.verbose) cli::cli_alert_success("Response received")

  return(chat)
}

#' Get messages from an ollama chat
#'
#' @param chat The chat
#' @param ... Further arguments passed from other methods
#'
#' @export
messages.ollama <- function(chat, ...) {
  chat$messages
}

#' Print information about an ollama chat
#'
#' @param x The chat
#' @param ... Further arguments passed from other methods
#'
#' @export
print.ollama <- function(x, ...) {
  x
}

#' Get the last response sent by the model in an ollama chat
#'
#' @param chat The chat
#' @param ... Further arguments passed from other methods
#'
#' @export
last_response.ollama <- function(chat, ...) {
  messages <- messages(chat)
  messages <- messages[which(messages$role == "assistant"), ]
  if (nrow(messages) == 0) return(NA_character_)
  utils::tail(messages$content, 1)
}
