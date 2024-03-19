#' A constructor function for assistant objects
#'
#' @param x A list containing parameters for the assistant, as documented in
#' assistant()
new_assistant <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "assistant")
}

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
#' @param instructions Instructions for the assistance, an atomic character
#' vector of maximum length 32,768 characters
#' @param tools A list of assistant_tool objects, maximum of 128
#' @param files A list of assistant_file objects, maximum of 20
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
  files = NULL,
  metadata = NULL
) {

  # Set up and validate the instance
  params <- list(
    model = openai_model(),
    name = name,
    description = description,
    instructions = instructions,
    tools = tools,
    files = files,
    metadata = metadata
  )
  params <- params[! unlist(lapply(params, is.null))]
  assistant <- new_assistant(c(list(id = character(1), created_at = integer(1)), params))
  validate_assistant(assistant)

  # POST to assistants endpoint
  response <- httr::POST(
    "https://api.openai.com/v1/assistants",
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
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

#' A validator function for assistant objects
#'
#' @param x A assistant object
validate_assistant <- function(x) {

  params <- unclass(x)

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "name", "model", "description", "instructions", "tools", "files", "metadata"))
  qassert(params$id, "S1")
  qassert(params$created_at, "X1")
  assertString(params$model, max.chars = 256)
  if (! testNull(params$name)) assertString(params$name, max.chars = 256)
  if (! testNull(params$description)) assertString(params$description, max.chars = 512)
  if (! testNull(params$instructions)) assertString(params$instructions, max.chars = 32768)
  if (! testNull(params$tools)) {
    assertList(params$tools, max.len = 128)
    for (tool in params$tools) assertClass(tool, "assistant_tool")
  }
  if (! testNull(params$files)) {
    assertList(params$files, max.len = 20)
    for (file in params$files) assertClass(tool, "assistant_file")
  }
  if (! testNull(params$metatdata)) {
    assertCharacter(params$metadata, max.len = 16, names = "named")
    for (key in names(params$metadata)) assertString(key, max.chars = 64)
    for (value in params$metadata) assertString(value, max.chars = 512)
  }

  x
}

#' A print method for assistants
#'
#' Coloured output based on
#' \url{https://github.com/r-lib/testthat/blob/717b02164def5c1f027d3a20b889dae35428b6d7/R/colour-text.r}
#'
#' @param x An assistant object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.assistant <- function(x, ...) {

  cli::cli_h1("assistant object")

  print(unclass(x))
}

#' List GPT assistants
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
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
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
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  files <- as.data.frame(do.call(rbind, response))
  return(files)
}

#' Retrieve an assitant
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
    httr::add_headers("OpenAI-Beta" = "assistants=v1")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  assistant <- new_assistant(httr::content(response))
  return(assistant)
}
