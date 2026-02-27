
#' CBM4 summary: emissions
#' @template cbm4_data
#' @template timestep
#' @template simulation_dataset
#' @return `data.table`
#' @export
cbm4_summary_emissions <- function(
    cbm4_data = NULL,
    timestep  = NULL,
    simulation_dataset = file.path(cbm4_data, "simulation")
){

  paths <- list(
    flux_process = file.path(simulation_dataset, "simulation-table-annual_process_flux"),
    flux_dist    = file.path(simulation_dataset, "simulation-table-disturbance_flux")
  )

  stepFlux <- arrow::open_dataset(paths$flux_process)
  if (!is.null(timestep)) stepFlux <- dplyr::filter(stepFlux, timestep == !!timestep)

  stepFlux <- stepFlux |>
    dplyr::select(timestep, inventory.area, cohort_proportion, DecayDOMCO2Emission) |>
    dplyr::collect() |>
    dplyr::group_by(timestep) |>
    dplyr::summarise(CO2 = sum(DecayDOMCO2Emission * (inventory.area * cohort_proportion)))

  stepFlux <- data.table::as.data.table(stepFlux, key = "timestep")

  if (file.exists(paths$flux_dist)){

    distFlux <- arrow::open_dataset(paths$flux_dist)
    if (!is.null(timestep)) distFlux <- dplyr::filter(distFlux, timestep == !!timestep)

    distFlux <-  distFlux |>
      dplyr::select(timestep, area, cohort_proportion,
                    DisturbanceBioCO2Emission, DisturbanceDOMCO2Emission,
                    DisturbanceBioCH4Emission, DisturbanceDOMCH4Emission,
                    DisturbanceBioCOEmission,  DisturbanceDOMCOEmission) |>
      dplyr::collect() |>
      dplyr::group_by(timestep) |>
      dplyr::summarise(
        CO2 = sum(DisturbanceBioCO2Emission * (area * cohort_proportion),
                  DisturbanceDOMCO2Emission * (area * cohort_proportion)),
        CH4 = sum(DisturbanceBioCH4Emission * (area * cohort_proportion),
                  DisturbanceDOMCH4Emission * (area * cohort_proportion)),
        CO  = sum(DisturbanceBioCOEmission  * (area * cohort_proportion),
                  DisturbanceDOMCOEmission  * (area * cohort_proportion))
      )

    distFlux <- data.table::as.data.table(distFlux, key = "timestep")

  }else distFlux <- data.table::data.table(timestep = stepFlux$timestep, CO2 = 0, CH4 = 0, CO = 0)

  stepFlux <- merge(stepFlux, distFlux, by = "timestep", suffixes = c(".step", ""), all = TRUE)
  stepFlux[is.na(CO2), CO2 := 0]
  stepFlux[is.na(CH4), CH4 := 0]
  stepFlux[is.na(CO),  CO  := 0]
  stepFlux[, CO2 := CO2 + CO2.step][, CO2.step := NULL]
  stepFlux[, Emissions := CO2 + CH4 + CO]
  data.table::setkey(stepFlux, timestep)
  data.table::setcolorder(stepFlux, c("timestep", "Emissions", "CO2", "CH4", "CO"))
  stepFlux
}



