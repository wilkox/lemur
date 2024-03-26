with_mock_api({ test_that("Assistants API bindings", {

  # Create an assistant_tool
  expect_no_error({ assistant_tool <- assistant_tool() })
  expect_s3_class(assistant_tool, "assistant_tool")

  # Create an assistant
  expect_no_error({ assistant <- create_assistant(tools = list(assistant_tool()), name = "Created in automated testing") })
  expect_s3_class(assistant, "assistant")

  # List assistants
  expect_no_error({ assistants_list <- list_assistants() })
  assistant_id <- assistants_list$id[[1]]
  expect_s3_class(assistants_list, "data.frame")

  # Create an assistant_file
  expect_no_error({ assistant_file <- create_assistant_file(assistant$id, "file-sN6eG4TkNq2Quj1LpgCdUVdo") })
  expect_s3_class(assistant_file, "assistant_file")

  # List assistant files
  expect_no_error({ assistant_files_list <- list_assistant_files(assistant_id) })
  expect_s3_class(assistant_files_list, "data.frame")

  # Retrieve an assistant
  expect_no_error({ assistant <- retrieve_assistant(assistant_id) })
  expect_s3_class(assistant, "assistant")

}) })
