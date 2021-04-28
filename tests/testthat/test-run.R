test_that("run data has correct shape", {
  res <- nimue_run(test_nimue_parameters())
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


test_that("Compute vaccine efficacy correctly", {
  expect_equal(
    nimue_run_vaccine_efficacy(0.5, 0.8),
    list(infection = rep(0.5, 17),
         disease = rep(0.6, 17)))
  expect_equal(
    nimue_run_vaccine_efficacy(0.8, 0.5),
    list(infection = rep(0.8, 17),
         disease = rep(0.0, 17)))
})


test_that("Select correct coverage matrix", {
  dat <- country_data("TEST")

  mat_elderly <- nimue::strategy_matrix("Elderly", max_coverage = 1)
  mat_all <- nimue::strategy_matrix("All", max_coverage = 1)

  ## No constraint:
  expect_equal(
    nimue_run_coverage_matrix(dat, "HCW and Elderly", 1, 1),
    dat$vacc_strategy$hcw_elderly)
  expect_equal(
    nimue_run_coverage_matrix(dat, "HCW, Elderly and High-Risk", 1, 1),
    dat$vacc_strategy$hcw_elderly_high_risk)
  expect_equal(
    nimue_run_coverage_matrix(dat, "Elderly", 1, 1),
    mat_elderly)
  expect_equal(
    nimue_run_coverage_matrix(dat, "All", 1, 1),
    mat_all)

  ## Reducing coverage uptake matrix:
  expect_equal(
    nimue_run_coverage_matrix(dat, "HCW and Elderly", 0.5, 1),
    dat$vacc_strategy$hcw_elderly / 2)
  expect_equal(
    nimue_run_coverage_matrix(dat, "HCW, Elderly and High-Risk", 0.5, 1),
    dat$vacc_strategy$hcw_elderly_high_risk / 2)
  expect_equal(
    nimue_run_coverage_matrix(dat, "Elderly", 0.5, 1),
    mat_elderly / 2)
  expect_equal(
    nimue_run_coverage_matrix(dat, "All", 0.5, 1),
    mat_all / 2)

  ## Allow partial coverage
  m <- matrix(0, 17, 17)
  m[c(225, 226, 241, 242, 243, 257, 258, 259, 260, 273, 274, 275, 276, 277)] <-
    1
  m[209] <- 0.234106287912777
  expect_equal(
    nimue_run_coverage_matrix(dat, "Elderly", 1, 0.2), m)

  expect_error(
    nimue_run_coverage_matrix(dat, "Other", 0.5, 1),
    "Incorrect strategy")
})
