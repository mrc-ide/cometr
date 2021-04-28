test_that("can run the model over test data", {
  set.seed(1)
  expect_snapshot_value(comet_run(comet_parameters("TEST")), "serialize")
})
