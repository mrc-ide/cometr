test_that("json deserialises as expected", {
  p <- test_nimue_parameters()
  json <- paste(readLines("nimue-run-example.json"), collapse = "\n")
  expect_equal(p, jsonlite::fromJSON(json))
})
