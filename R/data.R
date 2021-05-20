country_data <- function(country) {
  path <- file.path(cometr_file("extdata"), paste0(country, ".rds"))
  if (!file.exists(path)) {
    stop(sprintf("Invalid country '%s'", country))
  }
  readRDS(path)
}
