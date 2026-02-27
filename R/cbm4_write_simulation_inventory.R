
#' CBM4 write simulation inventory
#'
#' Write inventory to a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @param inventoryDT data.table. Cohort inventory.
#' @template timestep
#' @inheritParams cbm4_write_geo
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_simulation_inventory <- function(
    cbm4_data = NULL,
    cbm_defaults_db = NULL,
    inventoryDT,
    timestep,
    grid_rast     = NULL,
    grid_chunks   = 1,
    template_name = NULL,
    template_path = file.path(cbm4_data, template_name),
    dataset_name = "simulation",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  # Initiate dataset
  if (!file.exists(dataset_path)) cbm4_write_geo(
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    grid_rast     = grid_rast,
    grid_chunks   = grid_chunks,
    template_name = template_name,
    template_path = template_path,
    partitions    = list("timestep" = "int32", "cohort_index" = "int64", "chunk_index" = "int64"),
    tags          = list(classifier = paste0("classifiers.", classifiers))
  )

  # Format inventory
  inv <- cbm4_format_simulation_inventory(
    cbm_defaults_db = cbm_defaults_db,
    pixelDT = arrow_space_dataset_read_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "table-pixels"
    ),
    inventoryDT,
    timestep,
    ...)

  # Write inventory
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = NULL,
    table_data   = inv$flat,
    partitioning = c("timestep", "cohort_index", "chunk_index")
  )
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "raster_index",
    table_data   = inv$index,
    partitioning = c("timestep", "cohort_index", "chunk_index")
  )

  return(invisible())

}


cbm4_format_simulation_inventory <- function(
    inventoryDT,
    pixelDT,
    timestep,
    cbm_defaults_db = NULL
){

  # Rename columns
  dataFull <- data.table::as.data.table(inventoryDT)
  data.table::setnames(dataFull, "pixelIndex", "pixel_index", skip_absent = TRUE)

  # Check table columns
  colTypes <- list(
    inventory = c(
      "spatial_unit", "eco_boundary", "admin_boundary", "area",
      "age", "cohort_proportion", "land_class", "delay",
      "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"),
    state = c(
      "age", "last_disturbance_type", "last_disturbance_event", "time_since_last_disturbance", "time_since_land_class_change",
      "land_class", "growth_multiplier", "regeneration_delay",
      "growth_enabled", "enabled"
    ),
    pools = c(
      "Input",
      "SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther", "SoftwoodCoarseRoots", "SoftwoodFineRoots",
      "SoftwoodStemSnag", "SoftwoodBranchSnag",
      "HardwoodMerch", "HardwoodFoliage", "HardwoodOther", "HardwoodCoarseRoots", "HardwoodFineRoots",
      "HardwoodStemSnag", "HardwoodBranchSnag",
      "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil", "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil",
      "CO2", "CH4", "CO", "NO2", "Products"
    )
  )
  colNames <- do.call(c, lapply(names(colTypes), function(colType) paste0(colType, ".", colTypes[[colType]])))
  check_table_columns_all("inventoryDT", dataFull, colNames)

  # Join with pixel table
  dataFull <- merge(dataFull, pixelDT[, .(pixel_index, chunk_index, raster_index)],
                    by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Set cohort index
  ## Setting this to chunk_index for all cohorts until otherwise needed
  dataFull[, cohort_index := chunk_index]

  # Set index
  dataFull[, index := .GRP - 1, by = setdiff(names(dataFull), c("raster_index", "inventory.area"))]

  # Set area
  dataFull[, inventory.area := as.numeric(sum(inventory.area)), by = index]

  # Set timestep
  dataFull[, timestep := timestep]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, timestep, cohort_index, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Ensure that area is numeric, not integer
  dataFull[, inventory.area := as.numeric(inventory.area)]

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}


