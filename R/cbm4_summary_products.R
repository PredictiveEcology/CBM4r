
#' CBM4 summary: products
#'
#' @template cbm4_data
#' @template timestep
#' @template simulation_dataset
#' @return `data.table`
#' @export
cbm4_summary_products <- function(
    cbm4_data = NULL,
    timestep  = NULL,
    simulation_dataset = file.path(cbm4_data, "simulation")
){

  paths <- list(
    pools = file.path(simulation_dataset, "simulation")
  )

  stepPools <- arrow::open_dataset(paths$pools)

  if (!is.null(timestep)) stepPools <- dplyr::filter(stepPools, timestep == !!timestep)

  stepPools <- stepPools |>
    dplyr::select(timestep, inventory.area, cohort_proportion, pools.Products) |>
    dplyr::collect() |>
    dplyr::group_by(timestep) |>
    dplyr::summarise(Products = sum(pools.Products * (inventory.area * cohort_proportion)))

  stepPools <- data.table::as.data.table(stepPools)
  data.table::setkey(stepPools, timestep)
  data.table::setcolorder(stepPools, c("timestep", "Products"))

  stepPools
}


