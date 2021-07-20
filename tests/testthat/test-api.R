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


test_that("countries", {
  endpoint <- endpoint_countries()

  res <- endpoint$target()
  expect_s3_class(res, "data.frame")
  expect_equal(names(res), c("code", "name", "date", "public"))

  res <- endpoint$run()
  expect_true(res$validated)

  api <- build_api(validate = TRUE)
  res <- suppressMessages(api$request("GET", "/countries"))

  expect_equal(res$status, 200L)
  expect_equal(res$headers[["Content-Type"]], "application/json")
  expect_equal(res$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(res$body, as.character(endpoint$run()$body))
})


test_that("run", {
  endpoint <- endpoint_nimue_run()

  body <- paste(readLines("nimue-run-example.json"), collapse = "\n")

  res_target <- endpoint$target(body)
  expect_s3_class(res_target, "json")

  res_endpoint <- endpoint$run(body)
  expect_true(res_endpoint$validated)
  expect_equal(res_endpoint$data, res_target)

  api <- build_api(validate = TRUE)
  res_api <- suppressMessages(api$request("POST", "/nimue/run", body = body))

  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(res_api$body, res_endpoint$body)
})


test_that("allow empty rt schedule", {
  endpoint <- endpoint_nimue_run()

  body <- paste(readLines("nimue-run-example.json"), collapse = "\n")
  dat <- jsonlite::parse_json(body)
  dat$rt <- list()
  body <- jsonlite::toJSON(dat, auto_unbox = TRUE, null = "null")

  res_target <- endpoint$target(body)
  expect_s3_class(res_target, "json")

  res_endpoint <- endpoint$run(body)
  expect_true(res_endpoint$validated)
  expect_equal(res_endpoint$data, res_target)

  api <- build_api(validate = TRUE)
  res_api <- suppressMessages(api$request("POST", "/nimue/run", body = body))

  expect_equal(res_api$status, 200L)
  expect_equal(res_api$headers[["Content-Type"]], "application/json")
  expect_equal(res_api$headers[["X-Porcelain-Validated"]], "true")
  expect_equal(res_api$body, res_endpoint$body)
})
