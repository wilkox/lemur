#' A constructor function for GPT_assistant
#'
#' @param x A list containing parameters for the assistant, as documented in
#' GPT_assistant()
new_GPT_assistant <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "GPT_assistant")
}

#' Helper to create GPT_assistant objects
#'
#' The model will be set from the OPENAI_MODEL environmental variable. All
#' other parameters are optional.
#'
#' As part of creating the (local) GPT_assistant object, a linked assistant
#' object will be created with the OpenAI API, unless .dry_run is set.
#'
#' API documented at
#' \url{https://platform.openai.com/docs/api-reference/assistants/createAssistant}
#'
#' @param name The name of the assistant, an atomic character vector of maximum
#' length 256 characters
#' @param description A description of the assistant, an atomic character
#' vector of maximum length 512 characters
#' @param instructions Instructions for the assistance, an atomic character
#' vector of maximum length 32,768 characters
#' @param tools A list of assistant_tool objects, maximum of 128
#' @param files A list of assistant_file objects, maximum of 20
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
#' @export
GPT_assistant <- function(
  name = NULL,
  description = NULL,
  instructions = NULL,
  tools = NULL,
  files = NULL,
  metadata = NULL,
  .dry_run = FALSE
) {

  # Retrieve and set API key and model
  openai_api_key <- openai_api_key()
  model <- openai_model()

  # Set up and validate the instance
  params <- list(
    model = model,
    name = name,
    description = description,
    instructions = instructions,
    tools = tools,
    files = files,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  assistant <- new_GPT_assistant(c(list(id = character(1), created_at = integer(1)), params))
  validate_GPT_assistant(assistant)

  # POST to assistants endpoint
  if (.dry_run) {
    assistant$id <- "dry run id"
    assistant$created_at <- 123456L
    validate_GPT_assistant(assistant)
    return(assistant)

  } else {
    response <- httr::POST(
      "https://api.openai.com/v1/assistants",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::add_headers("OpenAI-Beta" = "assistants=v1"),
      httr::content_type_json(),
      body = jsonlite::toJSON(params, auto_unbox = TRUE)
    )
  }

  # Check status code of response
  if (! response$status_code %in% 200:299) {
    cli::cli_abort(c(
        "The OpenAI API returned an error:",
        "!" = "  Code: {response$status_code}",
        "!" = "  Type: {httr::content(response)$error$type}",
        "!" = "  Message: {httr::content(response)$error$message}"
    ))
  }

  # Extract and set id and created_at
  assistant$id <- httr::content(response)$id
  assistant$created_at <- httr::content(response)$created_at
  validate_GPT_assistant(assistant)

  return(assistant)
}

#' A validator function for GPT_assistant
#'
#' @param x A GPT_assistant object
validate_GPT_assistant <- function(x) {

  params <- unclass(x)

  # Must be a list
  if (! is.list(x)) { cli::cli_abort("Object of class GPT_assistant not a list") }

  # Must not contain any unrecognised parameters
  for (param in names(x)) {
    if (! param %in% c("id", "created_at", "name", "model", "description", "instructions", "tools", "files", "metadata")) {
      cli::cli_abort("Unknown parameter {param} for GPT_assistant object")
    }
  }

  # Must contain id and created_at (non-user-modifiable valued set by the
  # OpenAI API)
  if (is.null(params$id)) {
    cli::cli_abort("Object of class {.cls GPT_assistant} must have a {.val id} parameter")
  }
  if (! is.character(params$id) | ! length(params$id) == 1) {
    cli::cli_abort("{.cls GPT_assistant} parameter {.val id} must be atomic character vector")
  }
  if (is.null(params$created_at)) {
    cli::cli_abort("Object of class {.cls GPT_assistant} must have a {.val created_at} parameter")
  }
  if (! is.integer(params$created_at) | ! length(params$created_at) == 1) {
    cli::cli_abort("{.cls GPT_assistant} parameter {.val created_at} must be atomic character vector")
  }

  # Must contain a character vector named "model" with a single element of
  # up to 256 characters
  if (is.null(params$model)) { 
    cli::cli_abort("Object of class {.cls GPT_assistant} must have a {.val model} parameter")
  }
  if (! is.character(params$model) | ! length(params$model) == 1 | stringr::str_length(params$model) > 256) { 
    cli::cli_abort("{.cls GPT_assistant} parameter {.val model} must be a character vector with a single element of <= 256 characters")
  }

  # If there is a name, it must be a character vector with a single element of
  # up to 256 characters
  if (! is.null(params$name)) {
    if (! is.character(params$name) | ! length(params$name) == 1 | stringr::str_length(params$name) > 256) { 
      cli::cli_abort("{.cls GPT_assistant} parameter {.val name} must be a character vector with a single element of <= 256 characters")
    }
  }

  # If there is a description, it must be a character vector with a single
  # element of up to 512 characters
  if (! is.null(params$description)) {
    if (! is.character(params$description) | ! length(params$description) == 1 | stringr::str_length(params$description) > 512) { 
      cli::cli_abort("{.cls GPT_assistant} parameter {.val description} must be a character vector with a single element of <= 512 characters")
    }
  }

  # If there are instructions, they must be in a character vector with a single
  # element of up to 32,768 characters
  if (! is.null(params$instructions)) {
    if (! is.character(params$instructions) | ! length(params$instructions) == 1 | stringr::str_length(params$instructions) > 32768) { 
      cli::cli_abort("{.cls GPT_assistant} parameter {.val instructions} must be a character vector with a single element of <= 32,768 characters")
    }
  }

  # If there are tools, they must be provided as a list of up to 128
  # assistant_tool objects
  if (! is.null(params$tools)) {
    if (! is.list(params$tools)) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val tools} must be a list of {.cls assistant_tool} objects")
    }
    if (length(params$tools) > 128) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val tools} can have a maximum of 128 {.cls assistant_tool} objects")
    }
    if (! all(unlist(lapply(params$tools, class)) == "assistant_tool")) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val tools} must be a list of {.cls assistant_tool} objects")
    }
  }

  # If there are files, they must be provided as a list of up to 20
  # assistant_file objects
  if (! is.null(params$files)) {
    if (! is.list(params$files)) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val files} must be a list of {.cls assistant_file} objects")
    }
    if (length(params$files) > 20) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val files} can have a maximum of 20 {.cls assistant_file} objects")
    }
    if (! all(unlist(lapply(params$files, class)) == "assistant_file")) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val files} must be a list of {.cls assistant_file} objects")
    }
  }

  # If there is metadata, it must be a named character vector of up to 16
  # key-value pairs with keys up to 64 characters and values up to 512
  # characters
  if (! is.null(params$metadata)) {
    if (! is.character(params$metadata)) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val metadata} must be a character vector")
    }
    if (length(params$metadata) > 16) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val metadata} can have a maximum of 16 elements")
    }
    if (any(stringr::str_length(names(params$metadata)) > 64)) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val metadata} names must be <= 64 characters")
    }
    if (any(stringr::str_length(params$metadata) > 512)) {
      cli::cli_abort("{.cls GPT_assistant} parameter {.val metadata} values must be <= 512 characters")
    }
  }

  x
}

#' A print method for GPT_assistant
#'
#' Coloured output based on
#' \url{https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r}
#'
#' @param x A GPT_assistant object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.GPT_assistant <- function(x, ...) {

  cli::cli_h1("GPT_assistant object")

  print(unclass(x))
}

#' List GPT assistants
#'
#' @references \url{https://platform.openai.com/docs/api-reference/assistants/listAssistants}
#'
#' @param name limit A limit on the number of assistants to be returned. Can be
#' between 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'dsc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#' @param .dry_run If TRUE, will merely pretend to call the OpenAI API
#'
#' @return List of GPT assistants as a data frame
#'
#' @export
list_GPT_assistants <- function(limit = 20, order = "dsc", before = NULL, after = NULL, .dry_run = FALSE) {

  # Check arguments
  qassert(limit, "x1[0,100]")
  assertChoice(order, choices = c("asc", "dsc"))
  if (! testNull(before)) qassert(before, "s1")
  if (! testNull(after)) qassert(after, "s1")
  qassert(.dry_run, "b1")

  # Retrieve and set API key
  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    cli::cli_abort("Cannot find environmental variable {.envvar OPENAI_API_KEY}")
  }

  # GET from assiatants endpoint
  if (.dry_run) {
    assistants <- data.frame(id = "dry run")
    return(assistants)

  } else {
    response <- httr::GET(
      "https://api.openai.com/v1/assistants",
      httr::add_headers("Authorization" = paste("Bearer", openai_api_key)),
      httr::add_headers("OpenAI-Beta" = "assistants=v1")
    )
  }

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
  assistants <- as.data.frame(do.call(rbind, response))
  return(assistants)
}
