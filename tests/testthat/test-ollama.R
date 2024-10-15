vcr::use_cassette("ollama", { test_that("Chats with ollama work normally", {
  expect_no_error({ suppressMessages({ chat <- chat(service = "ollama", model = "llama3.2") }) })
  expect_no_error({ suppressMessages({ chat <- say(chat, "Hello there!") }) })
  expect_no_error({ suppressMessages({ response <- chat |> last_response() }) })
  expect_no_error({ suppressMessages({ response <- chat |> transcript() }) })
}) })
