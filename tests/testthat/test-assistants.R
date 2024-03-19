with_mock_api({ test_that("Assistants API bindings", {

  # Create an assistant
  expect_no_error({ assistant <- GPT_assistant() })
  expect_s3_class(assistant, "GPT_assistant")

  # List assistants
  expect_no_error({ assistants_list <- list_GPT_assistants() })
  expect_s3_class(assistants_list, "data.frame")
}) })
