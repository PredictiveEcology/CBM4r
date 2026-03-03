
#' CBM4 results: Emissions by timestep
#' @template cbm4_data
#' @template timestep
#' @return `data.table`
#' @export
cbm4_results_emissions_by_timestep <- function(
    cbm4_data,
    timestep = NULL
){
  cbm4_results_query_preset(
    cbm4_data, "emissions_by_timestep",
    where = if (!is.null(timestep)) paste0("a.timestep IN (", paste(timestep, collapse = ", "), ")")
  )
}


#' CBM4 results: Products by timestep
#' @template cbm4_data
#' @template timestep
#' @return `data.table`
#' @export
cbm4_results_products_by_timestep <- function(
    cbm4_data,
    timestep = NULL
){
  cbm4_results_query_preset(
    cbm4_data, "products_by_timestep",
    where = if (!is.null(timestep)) paste0("timestep IN (", paste(timestep, collapse = ", "), ")")
  )
}

