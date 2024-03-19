with_mock_api({ test_that("Chat completion works", {

  openai_api_key <- Sys.getenv("OPENAI_API_KEY")
  has_key <- TRUE
  if (stringr::str_length(openai_api_key) == 0 | is.na(openai_api_key)) {
    has_key <- FALSE
  }

  expect_error(complete_GPT(1))

  messages <- rbind(
    data.frame(role = "system", content = "You are an expert system used to teach children aged 5–12 about naked-eye astronomy. Your responses must use common words and short, simple sentences."),
    data.frame(role = "user", content = "Hi"),
    data.frame(role = "assistant", content = "Hi there :)"),
    data.frame(role = "user", content = "What is the Southern Cross")
  )
  messages <- as_GPT_messages(messages)
  if (has_key) {
    completion <- complete_GPT(messages)
  } else {
    completion <- complete_GPT(messages)
  }
  expect_no_error(completion)
  expect_type(completion, "list")
  expect_s3_class(completion, "GPT_messages")
  expect_length(completion$content, 5)
}) })
