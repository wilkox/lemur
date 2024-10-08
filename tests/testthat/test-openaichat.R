vcr::use_cassette("openaichat", { test_that("Chats with openaichat work normally", {
  expect_no_error({ suppressMessages({ chat <- chat(service = "openaichat") }) })
  expect_no_error({ suppressMessages({ chat <- say(chat, "Hello there!") }) })
  expect_no_error({ suppressMessages({ response <- chat |> last_response() }) })
  expect_no_error({ suppressMessages({ response <- chat |> transcript()}) })
}) })
