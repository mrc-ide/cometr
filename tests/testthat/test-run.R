test_that("run data has correct shape", {
  res <- comet_run(comet_parameters("TEST"))
  expect_type(res, "list")
  expect_equal(names(res),
               c("timeSeries", "fullyVaccinatedPersons",
                 "cumulativePercentPopulationVaccinated"))

  expect_s3_class(res$timeSeries, "data.frame")
  cols <- c("date", "reportedDeaths", "Rt", "Reff",
            "deaths", "counterfactualDeaths",
            "currentInfections", "counterfactualCurrentInfections",
            "hospitalisations", "counterfactualHospitalisations",
            "criticalCare", "counterfactualCriticalCare")
  expect_setequal(names(res$timeSeries), cols)
  expect_type(res$timeSeries$date, "character")
  expect_match(res$timeSeries$date, "^[0-9]{4}-[0-9]{2}-[0-9]{2}$")

  ## All numeric except date:
  expect_true(
    all(vapply(res$timeSeries[setdiff(cols, "date")], is.numeric, TRUE)))

  ## No missing values except reportedDeaths
  expect_false(
    any(vapply(res$timeSeries[setdiff(cols, "reportedDeaths")], anyNA, TRUE)))

  ## not done yet:
  expect_null(res$fullyVaccinatedPersons)
  expect_null(res$cumulativePercentPopulationVaccinated)
})
