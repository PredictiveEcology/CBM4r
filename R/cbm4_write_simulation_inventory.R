
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
    grid_meta,
    cohorts,
    timestep,
    classifiers   = NULL,
    template_name = "inventory",
    template_path = file.path(cbm4_data, template_name),
    dataset_name  = "simulation",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){
    if (!is.null(template_name)){
      arrow_space_dataset_copy_geo(
        dataset_name  = dataset_name,
        dataset_path  = dataset_path,
        template_name = template_name,
        template_path = template_path,
        partitions    = list("timestep" = "int32", "cohort_index" = "int64", "chunk_index" = "int64"),
        tags          = list(classifier = paste0("classifiers.", classifiers))
      )
    }else stop("Use `cbm4_write_geo` to initiate a new dataset or copy dataset attributes by setting `template_name`")
  }

  # Format inventory
  inv <- cbm4_format_simulation_inventory(
    grid_meta   = grid_meta,
    cohorts     = cohorts,
    classifiers = classifiers,
    timestep    = timestep,
    ...)

  # Write inventory
  unlink(file.path(dataset_path, "simulation", paste0("timestep=", timestep), recursive = TRUE))

  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = NULL,
    table_data   = inv$flat,
    partitioning = c("timestep", "cohort_index", "chunk_index"),
    schema       = list(
      inventory.area       = arrow::float32(),
      state.age            = arrow::int32(),
      state.growth_enabled = arrow::int8(),
      state.enabled        = arrow::int8()
    )
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
#' @template grid_meta
#' @param cohorts data.table. Cohort inventory.
#' @template timestep
#' @template classifiers
#' @param col_ignore character. Names of `cohorts` columns to exclude
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
#' @param def_state.enabled integer. TODO
#' @param def_state.growth_enabled integer. TODO
#' @param def_state.growth_multiplier integer. TODO
#' @param def_state.regeneration_delay integer. Regeneration delay.
#' @param def_state.land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @param ... unused
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_simulation_inventory <- function(
    grid_meta,
    cohorts,
    timestep,
    classifiers = NULL,
    col_ignore  = NULL,
    area_unit_conversion = 0.0001,
    def_cohort_proportion        = 1L,
    def_state.enabled            = 1L,
    def_state.growth_enabled     = 1L,
    def_state.growth_multiplier  = 1L,
    def_state.regeneration_delay = 0L,
    def_state.land_class         = "UNFCCC_FL_R_FL",  # "Forest Land remaining Forest Land"
    ...
){

  # Set required columns
  cohortCols <- list(
    classifiers = classifiers,
    inventory = c("area", "admin_boundary", "eco_boundary", "spatial_unit"),
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
  if (is.null(classifiers)){
    cohortCols$classifiers <- setdiff(names(cohorts), c(
      "pixel_index", "index", "cohort_index", "chunk_index", "cohort_proportion", "timestep",
      do.call(c, lapply(names(cohortCols), function(colType) paste0(colType, ".", cohortCols[[colType]]))),
      do.call(c, cohortCols)
    ))
  }

  # Check columns
  check_table_columns_all("cohorts", cohorts, "pixel_index")

  gridCols <- c(
    "pixel_index", "chunk_index", "raster_index",
    "area", "admin_boundary", "eco_boundary", "spatial_unit"
  )
  check_table_columns_all("grid_meta", grid_meta, gridCols)

  # Cast to data.table
  if (!data.table::is.data.table(grid_meta)) grid_meta <- data.table::as.data.table(grid_meta)
  if (!data.table::is.data.table(cohorts))   cohorts   <- data.table::as.data.table(cohorts)

  # Join with pixel table
  dataFull <- merge(
    cohorts,
    grid_meta[, .SD, .SDcols = c("pixel_index", "raster_index", "chunk_index", cohortCols$inventory)],
    by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Rename and check cohort columns
  for (colType in names(cohortCols)){
    data.table::setnames(dataFull, cohortCols[[colType]], paste0(colType, ".", cohortCols[[colType]]), skip_absent = TRUE)
    check_table_columns_all("cohorts", dataFull, paste0(colType, ".", cohortCols[[colType]]))
  }

  # Drop columns
  col_ignore <- intersect(col_ignore, names(cohorts))
  if (length(col_ignore) > 0) dataFull[, eval(col_ignore) := NULL]

  # Set index
  dataFull[, index := .GRP - 1L, by = setdiff(names(dataFull), c("raster_index", "inventory.area"))]

  # Set timestep
  dataFull[, timestep := as.integer(timestep)]

  # Set cohort index
  if (!"cohort_index" %in% names(dataFull)) dataFull[, cohort_index := 0L]

  # Set area
  if (is.integer(dataFull$inventory.area)) dataFull[, inventory.area := as.numeric(inventory.area)]
  dataFull[, inventory.area := sum(as.numeric(inventory.area)) * area_unit_conversion, by = index]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index, timestep)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))

  # Set defaults
  set_table_defaults(dataFull)

  # Return
  data.table::setcolorder(dataFull, unique(c(
    data.table::key(dataFull),
    names(dataFull)[!grepl("^(classifiers|inventory|state|pools)\\.", names(dataFull))],
    names(dataFull)[grepl("^classifiers\\.", names(dataFull))],
    names(dataFull)[grepl("^inventory\\.", names(dataFull))],
    names(dataFull)[grepl("^state\\.", names(dataFull))],
    names(dataFull)[grepl("^pools\\.", names(dataFull))]
  )))

  list(
    index = dataIndex,
    flat  = dataFull
  )
}


