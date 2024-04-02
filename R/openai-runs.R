#' Create a run
#'
#' The model will be set from the OPENAI_MODEL environmental variable. 
#'
#' @return A run object
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/createRun}#'
#' @param thread_id The ID of the thread to run (required)
#' @param assistant_id The ID of the assistant to be used to execute the run (required)
#' @param instructions Instructions for the assistant, an atomic character
#' vector of maximum length 32,768 characters. If provided, will override any
#' existing instructions for the assistant.
#' @param additional_instructions Additional instructions specific for the run.
#' An atomic character vector of maximum length 32,768 characters. Will not
#' override any existing instructions for the assistant.
#' @param tools A list of assistant_tool objects, maximum of 128
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#' @param temperature The sampling temperature to be used for the run, a double
#' between 0 and 2. Defaults to 1.
#' @param stream Logical value; if TRUE, will return a stream. Defaults to
#' FALSE.
#'
#' @export
create_run <- function(
  thread_id,
  assistant_id,
  instructions = NULL,
  additional_instructions = NULL,
  tools = NULL,
  metadata = NULL,
  temperature = 1,
  stream = FALSE
) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(assistant_id, "S1")
  qassert(temperature, "N1[0,2]")

  # Set up and validate the parameters
  params <- list(
    model = openai_model(),
    assistant_id = assistant_id,
    instructions = instructions,
    additional_instructions = additional_instructions,
    tools = tools,
    metadata = metadata,
    temperature = temperature,
    stream = stream
  )
  params <- params[! unlist(lapply(params, is.null))]

  # Mung parameters into the format expected by the API
  if (! testNull(params$tools)) params$tools <- lapply(params$tools, unclass)
  if (! testNull(params$file_ids)) params$file_ids <- as.list(params$file_ids)
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Reponse
  run <- as_run(response)
  validate_run(run)
  return(run)
}

#' List runs
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/listRuns}
#'
#' @param thread_id The ID of the thread to list runs for (required)
#' @param limit A limit on the number of assistants to be returned. Can be
#' between 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'desc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#'
#' @return Data frame of runs
#'
#' @export
list_runs <- function(thread_id, limit = 20, order = "desc", before = NULL, after = NULL) {

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
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  runs <- as.data.frame(do.call(rbind, response))
  return(runs)
}

#' List run steps
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/listRunSteps}
#'
#' @param thread_id The ID of the thread the run belongs to (required)
#' @param run_id The ID of the run to list steps for (required)
#' @param limit A limit on the number of assistants to be returned. Can be
#' between 1-100, defaults to 20.
#' @param order Sort order for the created_at timestamp. Either 'asc' for
#' ascending or 'desc' (default) for descending.
#' @param after Return assistants after the named assistant id
#' @param before Return assistants before the named assistant id
#'
#' @return Data frame of run steps
#'
#' @export
list_run_steps <- function(thread_id, run_id, limit = 20, order = "desc", before = NULL, after = NULL) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(run_id, "S1")
  qassert(limit, "x1[0,100]")
  assertChoice(order, choices = c("asc", "desc"))
  if (! testNull(before)) qassert(before, "s1")
  if (! testNull(after)) qassert(after, "s1")

  # Set up params
  params <- list(limit = limit, order = order, before = before, after = after)
  params <- Filter(Negate(is.null), params)

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/steps"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
    query = params
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  response <- httr::content(response)$data
  run_steps <- as.data.frame(do.call(rbind, response))
  return(run_steps)
}

#' Retrieve a run
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/getRun}
#'
#' @param thread_id The ID of the thread the run belongs to (required)
#' @param run_id The ID of the run to retrieve (required)
#'
#' @return A run object
#'
#' @export
retrieve_run <- function(thread_id, run_id) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(run_id, "S1")

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  run <- as_run(response)
  validate_run(run)
  return(run)
}

#' Retrieve a run step
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/getRunStep}
#'
#' @param thread_id The ID of the thread the run belongs to (required)
#' @param run_id The ID of the run the step belongs to (required)
#' @param step_id The ID of the step to retrieve (required)
#'
#' @return A run step object
#'
#' @export
retrieve_run_step <- function(thread_id, run_id, step_id) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(run_id, "S1")
  qassert(step_id, "S1")

  # GET from threads endpoint
  response <- httr::GET(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/steps/{step_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  run_step <- as_run_step(response)
  validate_run_step(run_step)
  return(run_step)
}

#' Modify a run
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/modifyRun}
#'
#' @param thread_id The ID of the thread the run belongs to (required)
#' @param run_id The ID of the run to modify (required)
#' @param metadata A named character vector of up to 16 metadata values, with
#' names (keys) maximum 64 characters long and values maximum 512 characters
#' long
#'
#' @return The modified run object
#'
#' @export
modify_run <- function(thread_id, run_id, metadata = NULL) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(run_id, "S1")

  # Set up and validate the parameters
  params <- list(metadata = metadata)
  params <- params[! unlist(lapply(params, is.null))]

  # Mung parameters into the format expected by the API
  if (! testNull(params$metadata)) params$metadata <- as.list(params$metadata)

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1"),
    httr::content_type_json(),
    body = jsonlite::toJSON(params, auto_unbox = TRUE)
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  run <- as_run(response)
  validate_run(run)
  return(run)
}

#' Cancel a run
#'
#' @references \url{https://platform.openai.com/docs/api-reference/runs/cancelRun}
#'
#' @param thread_id The ID of the thread the run belongs to (required)
#' @param run_id The ID of the run to cancel (required)
#'
#' @return The modified run object
#'
#' @export
cancel_run <- function(thread_id, run_id) {

  # Check arguments
  qassert(thread_id, "S1")
  qassert(run_id, "S1")

  # POST to threads endpoint
  response <- httr::POST(
    glue::glue("https://api.openai.com/v1/threads/{thread_id}/runs/{run_id}/cancel"),
    httr::add_headers("Authorization" = paste("Bearer", openai_api_key())),
    httr::add_headers("OpenAI-Beta" = "assistants=v1")
  )

  # Check status code of response
  check_openai_response(response)

  # Response
  run <- as_run(response)
  validate_run(run)
  return(run)
}
