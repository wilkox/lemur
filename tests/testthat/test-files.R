vcr::use_cassette("files", { test_that("Files API bindings", {

  # List files
  expect_no_error({ files_list <- list_files() })
  expect_s3_class(files_list, "data.frame")

}) })
