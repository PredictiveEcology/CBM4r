
#' CBM4 results: emissions by timestep
#' @inherit .cbm4_results_by_timestep params return
#' @export
cbm4_results_emissions_by_timestep <- function(cbm4_results, units, timesteps = NULL){

  emiss <- .cbm4_results_by_timestep(cbm4_results, "emissions_annual_process", units, timesteps)

  if ("disturbance_flux" %in% cbm4_results_query(cbm4_results, "SHOW TABLES")$name){
    key <- data.table::key(emiss)
    emiss <- rbind(
      emiss, .cbm4_results_by_timestep(cbm4_results, "emissions_disturbance", units, timesteps))[
        , lapply(.SD, sum), by = key]
    data.table::setkeyv(emiss, key)
  }

  return(emiss)
}

#' CBM4 results: flux by timestep
#' @inherit .cbm4_results_by_timestep params return
#' @export
cbm4_results_flux_by_timestep <- function(cbm4_results, units, timesteps = NULL){

  flux <- .cbm4_results_by_timestep(cbm4_results, "flux_annual_process", units, timesteps)

  if ("disturbance_flux" %in% cbm4_results_query(cbm4_results, "SHOW TABLES")$name){
    flux <- merge(
      flux,
      .cbm4_results_by_timestep(cbm4_results, "flux_disturbance", units, timesteps),
      by = data.table::key(flux), all = TRUE)
    flux[is.na(flux)] <- 0
  }

  return(flux)
}

#' CBM4 results: pools by timestep
#' @inherit .cbm4_results_by_timestep params return
#' @export
cbm4_results_pools_by_timestep <- function(cbm4_results, units, timesteps = NULL){
  .cbm4_results_by_timestep(cbm4_results, "pools", units, timesteps)
}


#' CBM4 results: by timestep
#' @template units
#' @template timesteps
#' @inherit .cbm4_results_query_preset params return
#' @keywords internal
.cbm4_results_by_timestep <- function(cbm4_results, type, units, timesteps = NULL, alias = NULL){

  unitOpts <- c("t", "Mt")
  if (!units %in% unitOpts) stop(
    "invalid unit ", shQuote(units),
    ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

  # Query
  cbm4Summary <- .cbm4_results_query_preset(
    cbm4_results,
    type = type, units = ifelse(units == "Mt", "t", units), by = "timestep",
    where = if (length(timesteps) > 0) paste0(
      if (!is.null(alias)) paste0(alias, "."), "timestep IN (", paste(timesteps, collapse = ", "), ")")
  )

  # Set key
  data.table::setkey(cbm4Summary, timestep)
  data.table::setcolorder(cbm4Summary)

  # Convert
  if (units == "Mt"){
    cols <- setdiff(names(cbm4Summary), data.table::key(cbm4Summary))
    cbm4Summary[, (cols) := lapply(.SD, function(x) x / 10^6), .SDcols = cols]
  }

  return(cbm4Summary)
}


