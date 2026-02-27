
#' CBM4 step
#'
#' Run an annual step on a CBM4 spatial parquet datasets.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @template timestep
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param write_parameters logical. Write step parameters to file.
#' This will have 1 row for every cohort and timestep (just the increments at the current age).
#' @template max_workers
#' @template step_parameters_dataset
#' @template inventory_dataset
#' @template disturbance_dataset
#' @template simulation_dataset
#'
#' @return `NULL`. Updates will be made to CBM4 spatial parquet datasets.
#' @export
cbm4_step <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    timestep,
    area_unit_conversion    = 0.0001,
    write_parameters        = TRUE,
    max_workers             = NULL,
    step_parameters_dataset = file.path(cbm4_data, "step_parameters"),
    inventory_dataset       = file.path(cbm4_data, "inventory"),
    disturbance_dataset     = file.path(cbm4_data, "disturbance"),
    simulation_dataset      = file.path(cbm4_data, "simulation")
){

  spatial_cbm4_app <- reticulate::import("cbm4.app.spatial.spatial_cbm4.spatial_cbm4_app")

  cbm4_datasets <- list(
    step_parameters = list(
      dataset_name = "step_parameters",
      storage_type = "local_storage",
      path_or_uri  = step_parameters_dataset
    ),
    inventory = list(
      dataset_name = "inventory",
      storage_type = "local_storage",
      path_or_uri  = inventory_dataset
    ),
    disturbance = list(
      dataset_name = "disturbance",
      storage_type = "local_storage",
      path_or_uri  = disturbance_dataset
    ),
    simulation = list(
      dataset_name = "simulation",
      storage_type = "local_storage",
      path_or_uri  = simulation_dataset
    )
  )

  for (dataset in cbm4_datasets){
    if (length(dataset$path_or_uri) == 0) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset path invalid")
    if (dataset$dataset_name != "inventory" && !file.exists(dataset$path_or_uri)) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset not found: ", dataset$path_or_uri)
  }

  cbmspec_config <- reticulate::dict(
    "package_name"       = "cbmspec_cbm3",
    "factory_function"   = "cbmspec_cbm3.models.cbmspec_cbm3_single_matrix.model_create",
    "factory_parameters" = reticulate::dict(
      "cbm_defaults_path" = cbm_defaults_db
    ))

  stepIn <- list(
    "cbmspec_model_config" = cbmspec_config,
    "timestep"             = timestep,
    "parameter_dataset"    = cbm4_datasets$step_parameters,
    "inventory_dataset"    = cbm4_datasets$inventory,
    "disturbance_dataset"  = cbm4_datasets$disturbance,
    "simulation_dataset"   = cbm4_datasets$simulation,
    "area_unit_conversion" = area_unit_conversion,
    "write_parameters"     = write_parameters,
    "max_workers"          = max_workers
  )
  if (is.null(max_workers) || is.na(max_workers)) stepIn[["max_workers"]] <- NULL

  spatial_cbm4_app$step_all(reticulate::dict(stepIn))

  return(invisible())
}

