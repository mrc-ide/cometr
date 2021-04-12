context("server")


test_that("run server", {
  skip_if_not_installed("mockery")
  port <- 1234
  host <- "127.0.0.1"

  api <- list(run = mockery::mock())
  mock_build_api <- mockery::mock(api)

  msg <- capture_messages(
    with_mock(
      "cometr:::build_api" = mock_build_api,
      server(port, host)))
  expect_match(msg[[1]], "Starting cometr server on port 1234")

  mockery::expect_called(mock_build_api, 1)
  expect_equal(mockery::mock_args(mock_build_api)[[1]], list())

  mockery::expect_called(api$run, 1)
  expect_equal(mockery::mock_args(api$run)[[1]], list(host, port))
})


test_that("pass arguments to server", {
  mock_server <- mockery::mock(NULL)
  with_mock("cometr:::server" = mock_server,
            main(c("--port=1234")))
  expect_equal(
    mockery::mock_args(mock_server)[[1]],
    main_args("--port=1234"))
})
