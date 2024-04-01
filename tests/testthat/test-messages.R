with_mock_api({ test_that("Messages API bindings", {

  # Create an assistant_tool
  expect_no_error({ assistant_tool <- assistant_tool() })
  expect_s3_class(assistant_tool, "assistant_tool")

  # Create an assistant
  expect_no_error({ assistant <- create_assistant(tools = list(assistant_tool()), metadata = c(test = "test-messages")) })
  expect_s3_class(assistant, "assistant")

  # Create an assistant_file
  expect_no_error({ assistant_file <- create_assistant_file(assistant$id, "file-sN6eG4TkNq2Quj1LpgCdUVdo") })
  expect_s3_class(assistant_file, "assistant_file")

  # Create a thread
  expect_no_error({ thread <- create_thread(metadata = c(test = "test-messages")) })
  expect_s3_class(thread, "thread")

  # Create a message
  expect_no_error({ message <- create_message(thread_id = thread$id, content = "Hello, world!", file_ids = assistant_file$id) })
  expect_s3_class(message, "message")

  # List messages
  expect_no_error({ message_list <- list_messages(thread$id) })
  expect_s3_class(message_list, "data.frame")

  # List message files
  expect_no_error({ message_files_list <- list_message_files(thread$id, message$id) })
  expect_s3_class(message_files_list, "data.frame")
  expect_equal(message_files_list$id[[1]], "file-sN6eG4TkNq2Quj1LpgCdUVdo")

  # Retrieve a message
  expect_no_error({ retrieved_message <- retrieve_message(thread$id, message$id) })
  expect_s3_class(retrieved_message, "message")
  expect_equal(retrieved_message, message)

  # Retrieve a message file
  expect_no_error({ retrieved_message_file <- retrieve_message_file(thread$id, message$id, assistant_file$id) })
  expect_s3_class(retrieved_message_file, "message_file")
  expect_equal(retrieved_message_file$id, assistant_file$id)

  # Modify a message
  expect_no_error({ message <- modify_message(thread$id, message$id, metadata = c(test = "test-messages", modified = TRUE)) })
  expect_s3_class(message, "message")
  expect_true(message$metadata["modified"] == "TRUE")

}) })