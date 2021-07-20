##' Run cometr server
##'
##' @title Run cometr server
##' @param port Port to serve on
##'
##' @param host Optional host (either `0.0.0.0` or `127.0.0.1`)
##'
##' @return Never returns
##' @export
server <- function(port, host = "0.0.0.0") {
  message("Starting cometr server on port ", port)
  build_api()$run(host, port)
}


build_api <- function(validate = NULL) {
  api <- porcelain::porcelain$new(validate = validate)
  api$handle(endpoint_root())
  api$handle(endpoint_countries())
  api$handle(endpoint_nimue_run())

  api$registerHook("preroute", api_preroute)
  api$registerHook("postserialize", api_postserialize)

  api
}


api_preroute <- function(data, req, res, value) {
  api_log_start(data, req, res)
}


api_postserialize <- function(data, req, res, value) {
  api_log_end(data, req, res, value)
}


schema_root <- function() {
  system.file("schema", package = "cometr", mustWork = TRUE)
}


returning_json <- function(schema) {
  porcelain::porcelain_returning_json(schema, schema_root())
}


endpoint_root <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/", target_root,
    returning = returning_json("Root.schema"))
}


target_root <- function() {
  pkgs <- c("odin", "nimue", "cometr")
  version <- lapply(pkgs, function(p)
    scalar(as.character(utils::packageVersion(p))))
  names(version) <- pkgs
  list(
    name = scalar("cometr"),
    version = version)
}


endpoint_countries <- function() {
  porcelain::porcelain_endpoint$new(
    "GET", "/countries", target_countries,
    returning = returning_json("Countries.schema"))
}


target_countries <- function() {
  readRDS(cometr_file("extdata/index.rds"))
}


endpoint_nimue_run <- function() {
  porcelain::porcelain_endpoint$new(
    "POST", "/nimue/run", target_nimue_run,
    porcelain::porcelain_input_body_json("pars", "NimueRunPars.schema",
                                         schema_root()),
    returning = returning_json("NimueRun.schema"))
}


target_nimue_run <- function(pars) {
  pars <- jsonlite::fromJSON(pars)
  if (length(pars$rt) == 0) {
    pars$rt <- data.frame(start = numeric(), value = numeric())
  }
  res <- nimue_run(pars)
  jsonlite::toJSON(res, dataframe = "rows", na = "null", null = "null")
}
