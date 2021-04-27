## This should give us a reasonable set of parameters, and they
## correspond to the ones that OJ was using
comet_parameters <- function() {
  list(region = "GBR",
       ## version = "v4.20210316",
       healthcare = list(
         generalBeds = 314310,
         criticalBeds = 11350),
       vaccination = list(
         efficacyInfection = 0.90,
         efficacyDisease = 0.96,
         maxDosesPerWeek = NULL,
         doseFactor = 1,
         strategy = "HCW and Elderly",
         uptake = 0.2,
         availability = 0.9,
         durability = 1095,
         riskProportion = 0.1,
         future = NULL),
       rt = data.frame(
         start = as.Date("2021-10-25"),
         value = 2.4),
       simulation = list(
         forecastDays = 720))
}
