with_mock_api({ test_that("Threads API bindings", {

  # Create a thread
  expect_no_error({ thread <- create_thread(metadata = c(country = "Australia")) })
  expect_s3_class(thread, "thread")

  # Retrieve a thread
  expect_no_error({ thread <- retrieve_thread(thread_id = thread$id) })
  expect_s3_class(thread, "thread")

  # Modify a thread
  expect_no_error({ thread <- modify_thread(thread_id = thread$id, metadata = c(country = "New Zealand", bird = "Kiwi")) })
  expect_s3_class(thread, "thread")

  # Delete a thread
  expect_message({ delete_thread(thread$id) }, "deleted")

}) })
