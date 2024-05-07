vcr::use_cassette("messages", { test_that("Messages API bindings", {

  # Create an assistant_tool
  expect_no_error({ assistant_tool <- assistant_tool() })
  expect_s3_class(assistant_tool, "assistant_tool")

  # Create an assistant
  expect_no_error({ assistant <- create_assistant(tools = list(assistant_tool()), metadata = c(test = "test-messages")) })
  expect_s3_class(assistant, "assistant")

  # Create an assistant_file
  expect_no_error({ assistant_file <- create_assistant_file(assistant$id, "file-P6XH5bDZSo311u3yKlM6bFFn") })
  expect_s3_class(assistant_file, "assistant_file")

  # Create a thread
  expect_no_error({ thread <- create_thread(metadata = c(test = "test-messages")) })
  expect_s3_class(thread, "thread")

  # Create a message
  expect_no_error({ message <- create_message(thread_id = thread$id, content = "Hello, world!") })
  expect_s3_class(message, "message")

  # List messages
  expect_no_error({ message_list <- list_messages(thread$id) })
  expect_s3_class(message_list, "data.frame")

  # Retrieve a message
  expect_no_error({ retrieved_message <- retrieve_message(thread$id, message$id) })
  expect_s3_class(retrieved_message, "message")
  expect_equal(retrieved_message, message)

  # Modify a message
  expect_no_error({ message <- modify_message(thread$id, message$id, metadata = c(test = "test-messages", modified = TRUE)) })
  expect_s3_class(message, "message")
  expect_true(message$metadata["modified"] == "TRUE")

}) })
