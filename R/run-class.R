#' A constructor function for the run class
#'
#' @param x A list containing parameters for the run, as documented in
#' run()
new_run <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "run")
}

#' A validator function for the run class
#'
#' @param x A run object
validate_run <- function(x) {

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "assistant_id", "thread_id", "status", "started_at", "expires_at", "cancelled_at", "failed_at", "completed_at", "last_error", "model", "instructions", "tools", "metadata", "usage", "temperature", "required_action", "top_p", "max_completion_tokens", "max_prompt_tokens", "truncation_strategy", "incomplete_details", "response_format", "tool_choice", "tool_resources"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  qassert(x$assistant_id, "S1")
  qassert(x$thread_id, "S1")
  assertChoice(x$status, c("queued", "in_progress", "requires_action", "cancelling", "cancelled", "failed", "completed", "expired"))
  if (! testNull(x$started_at)) qassert(x$started_at, "X1")
  if (! testNull(x$expires_at)) qassert(x$expires_at, "X1")
  if (! testNull(x$cancelled_at)) qassert(x$cancelled_at, "X1")
  if (! testNull(x$failed_at)) qassert(x$failed_at, "X1")
  if (! testNull(x$completed_at)) qassert(x$completed_at, "X1")
  if (! testNull(x$last_error)) {
    assertList(x$last_error, len = 2)
    assertSetEqual(names(x$last_error), c("code", "message"))
    assertChoice(x$last_error$code, c("server_error", "rate_limit_exceeded", "invalid_prompt"))
    qassert(x$last_error$message, "S1")
  }
  qassert(x$model, "S1")
  if (! testNull(x$instructions)) qassert(x$instructions, "S1")
  if (! testNull(x$tools)) {
    assertList(x$tools, max.len = 128)
  }
  if (! testNull(x$metadata)) {
    assertCharacter(x$metadata, max.len = 16, names = "named")
    for (key in names(x$metadata)) assertString(key, max.chars = 64)
    for (value in x$metadata) assertString(value, max.chars = 512)
  }
  if (! testNull(x$usage)) {
    assertList(x$usage, len = 3)
    assertSetEqual(names(x$usage), c("completion_tokens", "prompt_tokens", "total_tokens"))
    qassert(x$usage$completion_tokens, "X1")
    qassert(x$usage$prompt_tokens, "X1")
    qassert(x$usage$total_tokens, "X1")
  }
  if (! testNull(x$temperature)) qassert(x$temperature, "N1")
  x
}

#' A print method for the run class
#'
#' @param x A run object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.run <- function(x, ...) {

  cli::cli_h1("run object")

  print(unclass(x))
}

#' An as_run method for httr responses
#'
#' @param response The httr response
#'
as_run.response <- function(response) {

  content <- httr::content(response)
  if (! testNull(content$object)) content$object <- NULL
  if (! testNull(content$metadata)) content$metadata <- unlist(content$metadata)
  run <- new_run(content)
  run <- validate_run(run)
  run

}

as_run <- function(x) {
  UseMethod("as_run")
}
