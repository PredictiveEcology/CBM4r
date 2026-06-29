
#' CBM4 step: with cohorts
#'
#' Run an annual step on CBM4 spatial parquet datasets with an alternate set of cohort data.
#'
#' @inherit cbm4_step params return
#' @param ... arguments to \code{\link{cbm4_write_simulation}} or \code{\link{cbm4_step}}
#'
#' @return `NULL`. Updates will be made to CBM4 spatial parquet datasets.
#' @export
cbm4_step_with_cohorts <- function(
    cbm4_data = NULL,
    timestep,
    simulation_dataset = file.path(cbm4_data, "simulation"),
    ...
){

  # Set schema
  schema <- arrow::schema(arrow::open_dataset(file.path(simulation_dataset, "simulation")))

  # Temporarily move existing cohort data
  tempTables <- c("simulation", "simulation-raster_index")
  tempPaths <- data.frame(
    path = file.path(simulation_dataset, tempTables, paste0("timestep=", timestep - 1)),
    temp = file.path(simulation_dataset, paste0("temp_", tempTables))
  )
  file.rename(tempPaths$path, tempPaths$temp)
  on.exit({
    unlink(tempPaths$path, recursive = TRUE)
    file.rename(tempPaths$temp, tempPaths$path)
  })

  # Write temporary cohort data
  cbm4_write_simulation(
    dataset_path = simulation_dataset,
    timestep     = timestep - 1,
    schema       = schema,
    ...
  )

  # Step
  cbm4_step(
    cbm4_data = cbm4_data,
    simulation_dataset = simulation_dataset,
    timestep = timestep,
    ...
  )
}



