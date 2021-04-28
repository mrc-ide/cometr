test_that("Can't load unknown country data", {
  expect_error(country_data("UNK"), "Invalid country 'UNK'")
})
