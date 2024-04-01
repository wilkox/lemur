#' A constructor function for the run_step class
#'
#' @param x A list containing parameters for the run step, as documented in
#' run_step()
new_run_step <- function(x = list()) {
  stopifnot(! missing(x))
  structure(x, class = "run_step")
}

#' A validator function for the run_step class
#'
#' @param x A run_step object
validate_run_step <- function(x) {

  assertList(x)
  for (param in names(x)) assertChoice(param, c("id", "created_at", "run_id", "assistant_id", "thread_id", "type", "status", "expires_at", "cancelled_at", "failed_at", "completed_at", "last_error", "step_details", "metadata", "usage"))
  qassert(x$id, "S1")
  qassert(x$created_at, "X1")
  qassert(x$assistant_id, "S1")
  qassert(x$thread_id, "S1")
  assertChoice(x$status, c("in_progress", "cancelled", "failed", "completed", "expired"))
  assertList(x$step_details, len = 2)
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
  x
}

#' A print method for the run_step class
#'
#' @param x A run_step object
#' @param ... Other arguments to be passed to print()
#'
#' @export
print.run_step <- function(x, ...) {

  cli::cli_h1("run_step object")

  print(unclass(x))
}

#' An as_run_step method for httr responses
#'
#' @param response The httr response
#'
as_run_step.response <- function(response) {

  content <- httr::content(response)
  if (! testNull(content$tools)) content$tools <- lapply(content$tools, function(tool) assistant_tool(type = tool$type))
  if (! testNull(content$object)) content$object <- NULL
  if (! testNull(content$file_ids)) content$file_ids <- as.character(content$file_ids)
  run_step <- new_run_step(content)
  run_step <- validate_run_step(run_step)
  run_step

}

as_run_step <- function(x) {
  UseMethod("as_run_step")
}
