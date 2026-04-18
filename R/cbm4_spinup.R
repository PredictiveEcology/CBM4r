
#' CBM4 spinup
#'
#' Run spinup initialization on CBM4 spatial parquet datasets.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @template max_workers
#' @template spinup_parameters_dataset
#' @template inventory_dataset
#' @template simulation_dataset
#'
#' @return `NULL`. Updates will be made to CBM4 spatial parquet datasets.
#' @export
cbm4_spinup <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    max_workers = NULL,
    spinup_parameters_dataset = file.path(cbm4_data, "spinup_parameters"),
    inventory_dataset         = file.path(cbm4_data, "inventory"),
    simulation_dataset        = file.path(cbm4_data, "simulation")
  ){

  spatial_cbm4_app <- reticulate::import("cbm4.app.spatial.spatial_cbm4.spatial_cbm4_app")

  cbm4_datasets <- list(
    spinup_parameters = list(
      dataset_name = "spinup_parameters",
      storage_type = "local_storage",
      path_or_uri  = spinup_parameters_dataset
    ),
    inventory = list(
      dataset_name = "inventory",
      storage_type = "local_storage",
      path_or_uri  = inventory_dataset
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
    if (dataset$dataset_name != "simulation" && !file.exists(dataset$path_or_uri)) stop(
      "CBM4 ", shQuote(dataset$dataset_name), " dataset not found: ", dataset$path_or_uri)
    dataset$path_or_uri <- normalizePath(dataset$path_or_uri, winslash = "/", mustWork = FALSE)
  }

  cbmspec_config <- reticulate::dict(
    "package_name"       = "cbmspec_cbm3",
    "factory_function"   = "cbmspec_cbm3.models.cbmspec_cbm3_single_matrix.model_create",
    "factory_parameters" = reticulate::dict(
      "cbm_defaults_path" = cbm_defaults_db
    ))

  spatial_cbm4_app$create_simulation_dataset(reticulate::dict(
    "cbmspec_model_config"   = cbmspec_config,
    "inventory_dataset"      = cbm4_datasets$inventory,
    "out_simulation_dataset" = cbm4_datasets$simulation
  ))

  spinupIn <- list(
    "cbmspec_model_config" = cbmspec_config,
    "parameter_dataset"    = cbm4_datasets$spinup_parameters,
    "inventory_dataset"    = cbm4_datasets$inventory,
    "simulation_dataset"   = cbm4_datasets$simulation,
    "max_workers"          = max_workers
  )
  if (is.null(max_workers) || is.na(max_workers)) spinupIn[["max_workers"]] <- NULL

  spatial_cbm4_app$spinup_all(reticulate::dict(spinupIn))

  # Copy pixels table
  arrow_space_dataset_copy_table(
    dataset_name  = "simulation",
    dataset_path  = simulation_dataset,
    template_name = "inventory",
    template_path = inventory_dataset,
    table_name    = "table-pixels",
    overwrite     = TRUE,
    skip_missing  = TRUE
  )

  return(invisible())
}

