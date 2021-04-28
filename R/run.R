nimue_run <- function(pars) {
  dat <- country_data(pars$region)
  beta <- nimue_run_betas(
    dat, pars$rt$start, pars$rt$value)
  vaccines <- nimue_run_vaccines(
    dat, pars$vaccination$future$start, pars$vaccination$future$value)
  coverage_matrix <- nimue_run_coverage_matrix(
    dat, pars$vaccination$strategy, pars$vaccination$uptake,
    pars$vaccination$availability)
  vaccine_efficacy <- nimue_run_vaccine_efficacy(
    pars$vaccination$efficacyInfection, pars$vaccination$efficacyDisease)

  run <- function(with_vaccines) {
    ## Fixed parameters:
    dur_r <- 365
    seeding_cases <- 5
    seeding_age_order <- 6:10

    ## Questions: beta_set and R0, always the same?
    nimue::run(
      dat$country_name,
      dur_R = dur_r,
      use_dde = TRUE,
      beta_set = beta$value,
      seeding_cases = seeding_cases,
      seeding_age_order = seeding_age_order,
      tt_R0 = beta$time,
      R0 = beta$value,
      max_vaccine = vaccines$value * with_vaccines,
      tt_vaccine = vaccines$time,
      time_period = nrow(dat$params) + pars$simulation$forecastDays,
      dur_V = pars$vaccination$durability,
      vaccine_efficacy_infection = vaccine_efficacy$infection,
      vaccine_efficacy_disease = vaccine_efficacy$disease,
      vaccine_coverage_mat = coverage_matrix,
      ## relatively fast parameters
      atol = 0.001,
      rtol = 0.001,
      step_size_min = 0.00001,
      step_size_max = 1,
      step_size_min_allow = TRUE)
  }

  res_vac <- run(TRUE) # 0.4s
  res_cf <- run(FALSE) # 0.3s

  ## TODO: save into core data?
  index <- squire:::odin_index(res_vac$model)
  d_index <- index$D
  inf_cumu_index <- index$infections_cumu
  hosp_demand_index <- index$hospital_demand
  icu_demand_index <- index$ICU_demand
  vacc_cumu_index <- index$vaccines_cumu

  len <- nrow(res_vac$output)

  reff_beta <- stats::approx(beta$time, beta$value, xout = seq_len(len),
                             "constant", rule = 2)$y
  reff <- compute_reff(res_vac, reff_beta, dat$mixing_matrix, index)
  rt <- reff_beta * (last(dat$params$Rt) / last(dat$params$beta))

  incidence <- function(res, index) {
    diff(rowSums(res$output[, index, 1]))
  }
  total <- function(res, index) {
    rowSums(res$output[-1, index, 1])
  }

  date <- dat$params$date[[1]] + seq_len(len - 1)
  time_series <- data.frame(
    date = as.character(date),
    deaths = incidence(res_vac, d_index),
    counterfactualDeaths = incidence(res_cf, d_index),
    currentInfections = incidence(res_vac, inf_cumu_index),
    counterfactualCurrentInfections = incidence(res_cf, inf_cumu_index),
    hospitalisations = total(res_vac, hosp_demand_index),
    counterfactualHospitalisations = total(res_cf, hosp_demand_index),
    criticalCare = total(res_vac, icu_demand_index),
    counterfactualCriticalCare = total(res_cf, icu_demand_index),
    reportedDeaths = dat$params$deaths[match(date, dat$params$date)],
    Rt = rt[-1],
    Reff = reff[-1],
    stringsAsFactors = FALSE)

  ## "age": "80+",
  ## "year_2021": 2.725948444792286,
  ## "year_2022": 0,
  ## "remainder": 0.724667555207714
  fvps <- NULL

  ## "month": "Mar 2021",
  ## "healthcare_workers": 0,
  ## "over_65_years": 0,
  ## "working_age": 0
  vaccinated <- NULL

  list(timeSeries = time_series,
       fullyVaccinatedPersons = fvps,
       cumulativePercentPopulationVaccinated = vaccinated)
}


nimue_run_betas <- function(dat, future_rt_date, future_rt_value) {
  stopifnot(length(future_rt_date) == length(future_rt_value))
  future_rt_date <- as.Date(future_rt_date)
  durs <- nimue:::default_durations()
  probs <- nimue:::default_probs()
  n_groups <- 17

  ## This section feels quite complicated but probably is not
  future_beta <- nimue::beta_est_infectiousness(
    dur_IMild = durs$dur_IMild,
    dur_ICase = durs$dur_ICase,
    prob_hosp = probs$prob_hosp,
    rel_infectiousness = rep(1, n_groups),
    mixing_matrix = dat$mixing_matrix,
    R0 = future_rt_value)

  beta_final <- last(dat$params$beta)
  future_beta_changes <- future_beta / beta_final
  value <- c(dat$params$beta, beta_final * future_beta_changes)

  ## There was calculation if beta min/beta max here but was unused;
  ## later on we might do something with this for uncertainty.
  ##
  ## NOTE: This +1 comes from something deep in nimue that OJ
  ## understands. Perhaps we can move it into the parameters handling
  ## though.

  time <- c(dat$params$date_offset + 1,
            date_offset(dat$params$date[[1]], future_rt_date))

  list(time = time, value = value, rt = future_rt_value)
}


nimue_run_vaccines <- function(dat, future_vaccine_date = NULL,
                               future_vaccine_value = NULL,
                               max_vaccine = NULL) {
  stopifnot(length(future_vaccine_date) == length(future_vaccine_value))
  future_vaccine_date <- as.Date(future_vaccine_date)
  if (is.null(max_vaccine)) {
    ## Otherwise use pop size. The default in covidsim is 2.5% of the
    ## population to recieve per week so divided by 7 here:
    max_vaccine <- as.integer(sum(dat$population$n) * 0.025 / 7)
  }

  value <- dat$params$max_vaccine
  i <- seq(to = length(value), length.out = length(max_vaccine))
  value[i] <- max_vaccine
  value <- c(value, future_vaccine_value)

  time <- c(dat$params$date_offset + 1,
            date_offset(dat$params$date[[1]], future_vaccine_date))

  list(time = time, value = value)
}


nimue_run_coverage_matrix <- function(dat, strategy, uptake, available) {
  if (strategy == "HCW and Elderly") {
    coverage_matrix <- dat$vacc_strategy$hcw_elderly * uptake
  } else if (strategy == "HCW, Elderly and High-Risk") {
    coverage_matrix <- dat$vacc_strategy$hcw_elderly_high_risk * uptake
  } else if (strategy == "Elderly") {
    coverage_matrix <-
      nimue::strategy_matrix("Elderly", max_coverage = uptake, 0)
  } else if (strategy == "All") {
    coverage_matrix <-
      nimue::strategy_matrix("All", max_coverage = uptake, 0)
  } else {
    stop('Incorrect strategy. Must be one of "HCW and Elderly", ',
         '"HCW, Elderly and High-Risk", "Elderly", "All"')
  }
  scale_coverage_matrix(coverage_matrix, available, dat$population$n)
}


nimue_run_vaccine_efficacy <- function(efficacy_infection, efficacy_disease) {
  n_groups <- 17

  if (efficacy_disease < efficacy_infection) {
    efficacy_disease <- efficacy_infection
  }
  efficacy_extra <- (efficacy_disease - efficacy_infection) /
    (1 - efficacy_infection)

  list(
    infection = rep(efficacy_infection, n_groups),
    disease = rep(efficacy_extra, n_groups))
}


## Provided by OJ:
scale_coverage_matrix <- function(coverage_matrix, vaccine_available, pop) {
  tot_vaccines <- sum(pop * vaccine_available)

  ## step 1, find when max allocation exceeds capacity
  step <- 1
  step_found <- FALSE
  tot_vaccs_steps <- 0
  coverage_matrix_dup_ex <- rbind(0, coverage_matrix)

  while (!step_found && step <= nrow(coverage_matrix)) {
    if (nrow(coverage_matrix) == 1) {
      step_found <- TRUE
    }

    vaccs_in_step <- sum((coverage_matrix_dup_ex[step + 1, ] -
                          coverage_matrix_dup_ex[step, ]) * pop)
    tot_vaccs_steps <- tot_vaccs_steps + vaccs_in_step
    if (tot_vaccs_steps > tot_vaccines) {
      step_found <- TRUE
    } else {
      step <- step + 1
    }
  }

  ## if we have enough vaccine return now
  if (step > nrow(coverage_matrix)) {
    return(coverage_matrix)
  }

  ## set steps after max available reached to 0
  if (step < nrow(coverage_matrix)) {
    coverage_matrix[(step + 1):nrow(coverage_matrix), ] <- 0
  }

  ## now set this step to be correct for available
  tots_given <- sum(coverage_matrix[step - 1, ] %*% pop)
  tots_tried <- sum(coverage_matrix[step, ] %*% pop)
  remaining <- tot_vaccines - tots_given

  ## next_group
  next_group <- coverage_matrix[step, ] - coverage_matrix[step - 1, ]
  new_cov <- remaining /
    (next_group[which(next_group > 0)] * pop[which(next_group > 0)])
  coverage_matrix[step, which(next_group > 0)] <- new_cov

  coverage_matrix
}


date_offset <- function(start_date, future_date) {
  if (length(future_date) == 0) {
    return(integer(0))
  }
  ## TODO: I might be off by one here on the conversion from incoming
  ## date for the flexible Rt values
  as.numeric(future_date - start_date + 1)
}


compute_reff <- function(out, beta, mixing_matrix, index) {
  dur_icase <- out$parameters$dur_ICase
  dur_imild <- out$parameters$dur_IMild
  prob_hosp <- out$odin_parameters$prob_hosp
  pop <- out$parameters$population

  # in here we work out each time point the number of individuals in
  # each age category in the S compartment at each time point.
  len <- nrow(out$output)
  susceptible <- array(out$output[, index$S, ], dim = c(len, dim(index$S)))
  # We divide by the total population
  prop_susc <- sweep(susceptible, 2, pop, FUN = "/")
  # We multiply by the effect of vaccines on onward infectiousness
  prop_susc <- sweep(prop_susc, c(2, 3),
                     out$odin_parameters$vaccine_efficacy_infection,
                     FUN = "*")

  # Length 17 with relative R0 in each age category
  rel_r0 <- prob_hosp * dur_icase + (1 - prob_hosp) * dur_imild

  # here we are looping over each time point to calculate the adjusted
  # eigen incorporating the proportion of the susceptible population
  # in each age group
  adjusted_eigens <- vnapply(seq_len(len), function(t)
    eigen1::eigen1(mixing_matrix * rowSums(prop_susc[t, , ] * rel_r0)))

  # multiply beta by the adjusted eigen at each time point to get Reff
  beta * adjusted_eigens
}
