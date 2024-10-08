vcr::use_cassette("openaiassistant", { test_that("Chats with openaiassistant work normally", {
  expect_no_error({ suppressMessages({ chat <- chat(service = "openaiassistant") }) })
  expect_no_error({ suppressMessages({ chat <- say(chat, "Hello there!") }) })
  expect_no_error({ suppressMessages({ response <- chat |> last_response() }) })
  expect_no_error({ suppressMessages({ response <- chat |> transcript()}) })
}) })
