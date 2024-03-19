test_that("Assistants API bindings", {

  message(".dry_run", .dry_run)

  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  has_key <- testString(openai_api_key, min.chars = 10)

  # Create an assistant
  expect_no_error({ assistant <- GPT_assistant(.dry_run = TRUE) })
  expect_s3_class(assistant, "GPT_assistant")

  # List assistants
  expect_no_error({ assistants_list <- list_GPT_assistants(.dry_run = TRUE) })
  expect_s3_class(assistants_list, "data.frame")
})
