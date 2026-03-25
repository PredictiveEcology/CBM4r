
#' CBM4 results: flux by timestep
#' @inherit cbm4_results_by_timestep params return
#' @export
cbm4_results_flux_by_timestep <- function(cbm4_results, timesteps = NULL){
  cbm4_results_by_timestep(cbm4_results, "flux_by_timestep", timesteps = timesteps)
}


#' CBM4 results: Pools by timestep
#' @inherit cbm4_results_by_timestep params return
#' @export
cbm4_results_pools_by_timestep <- function(cbm4_results, timesteps = NULL){
  cbm4_results_by_timestep(cbm4_results, "pools_by_timestep", timesteps = timesteps)
}


#' CBM4 results: Products by timestep
#' @inherit cbm4_results_by_timestep params return
#' @export
cbm4_results_products_by_timestep <- function(cbm4_results, timesteps = NULL){
  cbm4_results_by_timestep(cbm4_results, "products_by_timestep", timesteps = timesteps)
}


#' CBM4 results: Emissions by timestep
#' @inherit cbm4_results_by_timestep params return
#' @export
cbm4_results_emissions_by_timestep <- function(cbm4_results, timesteps = NULL){

  cbm4_results <- cbm4_results_processor(cbm4_results)

  cbm4_results_by_timestep(
    cbm4_results,
    ifelse("flux_indicators" %in% names(cbm4_results), "emissions_by_timestep", "emissions_no_disturbance_by_timestep"),
    timesteps = timesteps,
    alias = "a")
}


#' CBM4 results: by timestep
#' @template timesteps
#' @inherit cbm4_results_query_preset params return
#' @keywords internal
cbm4_results_by_timestep <- function(
    cbm4_results,
    queryName,
    timesteps = NULL,
    alias = NULL
){

  cbm4Summary <- cbm4_results_query_preset(
    cbm4_results, queryName,
    where = if (length(timesteps) > 0) paste0(
      if (!is.null(alias)) paste0(alias, "."), "timestep IN (", paste(timesteps, collapse = ", "), ")")
  )

  data.table::setkey(cbm4Summary, timestep)
  data.table::setcolorder(cbm4Summary)
  return(cbm4Summary)
}


