vcr::use_cassette("API", { test_that("Chat completion works", {

  expect_error({ complete_GPT(1) })

  messages <- rbind(
    data.frame(role = "system", content = "You are an expert system used to teach children aged 5–12 about naked-eye astronomy. Your responses must use common words and short, simple sentences."),
    data.frame(role = "user", content = "Hi"),
    data.frame(role = "assistant", content = "Hi there :)"),
    data.frame(role = "user", content = "What is the Southern Cross")
  )
  expect_no_error({ messages <- as_chat(messages) })
  expect_no_error({ completion <- complete_GPT(messages) })
  expect_type(completion, "list")
  expect_s3_class(completion, "chat")
  expect_length(completion$content, 5)
}) })
