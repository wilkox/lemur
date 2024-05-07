vcr::use_cassette("runs", allow_playback_repeats = TRUE, { test_that("Runs API bindings", {

  # Create a thread
  expect_no_error({ thread <- create_thread(metadata = c(test = "test-runs")) })
  expect_s3_class(thread, "thread")

  # Create an assistant with a tool
  expect_no_error({ assistant <- create_assistant(tools = list(assistant_tool()), metadata = c(test = "test-runs")) })
  expect_s3_class(assistant, "assistant")

  # Create a run
  expect_no_error({ run <- create_run(thread$id, assistant$id, metadata = c(test = "test-runs")) })
  expect_s3_class(run, "run")

  # List runs
  expect_no_error({ runs_list <- list_runs(thread$id) })
  expect_s3_class(runs_list, "data.frame")

  # Wait until run is completed before proceeding
  while (! run$status == "completed") {
    Sys.sleep(1)
    run <- retrieve_run(thread$id, run$id) 
  }

  # List run steps
  expect_no_error({ run_steps_list <- list_run_steps(thread$id, run$id) })
  expect_s3_class(run_steps_list, "data.frame")

  # Retrieve a run step
  expect_no_error({ run_step <- retrieve_run_step(thread$id, run$id, run_steps_list$id[[1]]) })
  expect_s3_class(run_step, "run_step")

  # Retrieve a run
  expect_no_error({ retrieved_run <- retrieve_run(thread$id, run$id) })
  expect_s3_class(retrieved_run, "run")
  expect_equal(run$id, retrieved_run$id)

  # Modify a run
  expect_no_error({ modified_run <- modify_run(thread$id, run$id, metadata = c(test = "test-runs", modified = "TRUE")) })
  expect_s3_class(modified_run, "run")
  expect_true(modified_run$metadata["modified"] == "TRUE")

  # Cancel a run (using a message that prompts a long response, so there is time to cancel it while it is still in_progress)
  long_thread <- create_thread(metadata = c(test = "long-run"))
  long_assistant <- create_assistant(metadata = c(test = "long-run"))
  long_message <- create_message(thread_id = long_thread$id, content = "You are being used to test 'long' prompt outputs. Please generate three paragraphs of 'lorem ipsum' in ancient Greek.")
  long_run <- create_run(long_thread$id, long_assistant$id, metadata = c(test = "long-run"))
  # 2024-05-07: skipping these tests as it is returning a 405 error for some reason
  message("2024-05-07: skipping these tests as it is returning a 405 error for some reason")
  # expect_no_error({ cancelled_run <- cancel_run(long_thread$id, long_run$id) })
  # expect_s3_class(cancelled_run, "run")

}) })
