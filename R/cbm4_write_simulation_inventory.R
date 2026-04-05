
#' CBM4 write simulation inventory
#'
#' Write inventory to a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_format_simulation_inventory
#' @param ... arguments to \code{\link{cbm4_format_simulation_inventory}}
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_simulation_inventory <- function(
    cbm4_data = NULL,
    cohortDT,
    timestep,
    classifiers = NULL,
    template_name = "inventory",
    template_path = file.path(cbm4_data, template_name),
    dataset_name  = "simulation",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){

    arrow_space_dataset_copy_geo(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      partitions    = list("timestep" = "int32", "cohort_index" = "int64", "chunk_index" = "int64"),
      tags          = list(classifier = paste0("classifiers.", classifiers))
    )

    # Copy pixels table
    arrow_space_dataset_copy_table(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      table_name    = "table-pixels",
      overwrite     = TRUE
    )
  }

  # Format inventory
  inv <- cbm4_format_simulation_inventory(
    cohortDT    = cohortDT,
    classifiers = classifiers,
    timestep    = timestep,
    grid_meta   = arrow_space_dataset_read_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "table-pixels",
      col_select   = c(
        "pixel_index", "chunk_index", "raster_index",
        "area", "admin_boundary", "eco_boundary", "spatial_unit"
      )
    ),
    ...)

  # Write inventory
  unlink(file.path(dataset_path, "simulation", paste0("timestep=", timestep), recursive = TRUE))

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


#' CBM4 format simulation inventory
#'
#' @param cohortDT data.table. Cohort inventory.
#' @template timestep
#' @template classifiers
#' @inheritParams cbm4_format_inventory
#' @param ... arguments to \code{\link{cbm4_format_inventory}}
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_simulation_inventory <- function(
    cohortDT,
    grid_meta,
    timestep,
    classifiers = NULL,
    ...
){

  inv <- cbm4_format_inventory(
    cohortDT  = cohortDT,
    grid_meta = grid_meta,
    def_delay = NULL,
    ...)

  inv$flat[,  timestep := timestep]
  inv$index[, timestep := timestep]

  colTypes <- list(
    classifiers = classifiers,
    inventory = c(
      "area", "admin_boundary", "eco_boundary", "spatial_unit"
    ),
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

  for (colType in names(colTypes)[!sapply(colTypes, is.null)]){
    data.table::setnames(inv$flat, colTypes[[colType]], paste0(colType, ".", colTypes[[colType]]), skip_absent = TRUE)
    check_table_columns_all("cohortDT", inv$flat, paste0(colType, ".", colTypes[[colType]]))
  }

  if (is.null(classifiers)){
    classifiers <- setdiff(names(inv$flat), c(
      "index", "cohort_index", "chunk_index", "cohort_proportion", "timestep",
      do.call(c, lapply(names(colTypes), function(colType) paste0(colType, ".", colTypes[[colType]])))
    ))
    data.table::setnames(inv$flat, classifiers, paste0("classifiers.", classifiers), skip_absent = TRUE)
  }

  data.table::setcolorder(inv$flat, unique(c(
    c("index", "timestep", "cohort_index", "chunk_index"),
    names(inv$flat)[!grepl("^(classifiers|inventory|state|pools)\\.", names(inv$flat))],
    names(inv$flat)[grepl("^classifiers\\.", names(inv$flat))],
    names(inv$flat)[grepl("^inventory\\.", names(inv$flat))],
    names(inv$flat)[grepl("^state\\.", names(inv$flat))],
    names(inv$flat)[grepl("^pools\\.", names(inv$flat))]
  )))

  # Return
  inv
}


