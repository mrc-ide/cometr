test_that("root", {
  endpoint <- endpoint_root()

  res <- endpoint$target()
  expect_equal(res$name, scalar("cometr"))
  expect_setequal(names(res$version), c("odin", "nimue", "cometr"))

  res <- endpoint$run()
  expect_true(res$validated)

  api <- build_api(validate = TRUE)
  res <- suppressMessages(api$request("GET", "/"))

  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(res$body, as.character(endpoint$run()$body))
})
