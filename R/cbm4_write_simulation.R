
#' CBM4 write simulation
#'
#' Write inventory and associated carbon pools
#' directly to a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @inheritParams cbm4_write_geo
#' @inheritParams cbm4_format_simulation
#' @param ... arguments to \code{\link{cbm4_format_simulation}}
#' @template template_name
#' @template template_path
#' @param schema internal
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_simulation <- function(
    cbm4_data       = NULL,
    timestep        = 0,
    cohorts         = NULL,
    classifiers     = NULL,
    grid_meta       = NULL,
    grid_rast       = NULL,
    template_name   = "inventory",
    template_path   = file.path(cbm4_data, template_name),
    dataset_name    = "simulation",
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path"),
    schema          = NULL,
    ...
){

  # Initiate simulation dataset from inventory dataset
  if (!file.exists(dataset_path)){
    cbm4_create_simulation_dataset(
      inventory_dataset  = template_path,
      simulation_dataset = dataset_path,
      cbm_defaults_db    = cbm_defaults_db
    )
  }

  if (!is.null(cohorts)){

    # Format inventory
    if (is.null(classifiers)){
      classifiers <- gsub("^classifiers\\.", "", arrow_space_dataset_read_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "tags"
      )[tag == "classifier", layer_name])
    }

    inv <- cbm4_format_simulation(
      grid_meta   = grid_meta,
      cohorts     = cohorts,
      classifiers = classifiers,
      timestep    = timestep,
      ...)

    if (!is.null(schema)){
      inv$flat <- inv$flat[, .SD, .SDcols = intersect(names(schema), names(inv$flat))]
      schema   <- schema[names(inv$flat)]
    }else schema <- cbm4_schema(inv$flat)

    # Write inventory
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = NULL,
      table_data   = inv$flat,
      schema       = schema,
      partitioning = c("timestep", "cohort_index", "chunk_index")
    )
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "raster_index",
      table_data   = inv$index,
      schema       = cbm4_schema(inv$index),
      partitioning = c("timestep", "cohort_index", "chunk_index")
    )
  }

  return(invisible())
}

#' CBM4 format simulation inventory
#'
#' @template timestep
#' @param def_enabled integer. TODO
#' @param def_growth_enabled integer. TODO
#' @param def_growth_multiplier integer. TODO
#' @param def_regeneration_delay integer. Regeneration delay.
#' @inheritParams cbm4_format_inventory
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
#' @keywords internal
cbm4_format_simulation <- function(
    cohorts,
    classifiers,
    timestep,
    grid_meta = NULL,
    def_last_disturbance_type   = NA,
    def_last_disturbance_event  = NA,
    def_time_since_last_disturbance = NA,
    def_time_since_land_class_change = NA,
    def_enabled            = 1L,
    def_growth_enabled     = 1L,
    def_growth_multiplier  = 1L,
    def_regeneration_delay = 0L,
    def_cohort_proportion  = 1L,
    def_land_class         = "UNFCCC_FL_R_FL", # "Forest Land remaining Forest Land"
    area_unit_conversion   = 0.0001,
    ...
){

  # Set required columns
  keyCols <- list(
    grid  = c("chunk_index", "raster_index"),
    other = c("cohort_index", "cohort_proportion")
  )
  cohortCols <- list(
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
      "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil",
      "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil",
      "CO2", "CH4", "CO", "NO2", "Products"
    )
  )

  # Read input
  dataFull <- data.table::as.data.table(cohorts)
  for (colType in names(cohortCols)){
    data.table::setnames(dataFull, paste0(colType, ".", cohortCols[[colType]]), cohortCols[[colType]], skip_absent = TRUE)
  }

  # Join with pixel table
  gridCols <- c(keyCols$grid, cohortCols$inventory)
  if (any(!gridCols %in% names(dataFull))){

    if (is.null(grid_meta)) stop(
      "grid_meta required to set attributes: ",
      paste(shQuote(setdiff(gridCols, names(dataFull))), collapse = ", "))

    # Cast to data.table
    if (!data.table::is.data.table(grid_meta)) grid_meta <- data.table::as.data.table(grid_meta)

    # Check columns
    check_table_columns_all("cohorts",   dataFull,  "pixel_index")
    check_table_columns_all("grid_meta", grid_meta, c("pixel_index", gridCols))

    # Join with pixel table
    dataFull <- merge(
      dataFull,
      grid_meta[, .SD, .SDcols = c("pixel_index", setdiff(gridCols, names(dataFull)))],
      by = "pixel_index", all.x = TRUE)
  }

  # Drop columns
  col_ignore <- setdiff(names(dataFull), c(do.call(c, keyCols), do.call(c, cohortCols)))
  if (length(col_ignore) > 0) dataFull[, eval(col_ignore) := NULL]

  # Set index
  dataFull[, index := .GRP - 1L, by = setdiff(names(dataFull), c("raster_index", "area"))]

  # Set timestep
  dataFull[, timestep := as.integer(timestep)]

  # Set cohort_index
  if (!"cohort_index" %in% names(dataFull)) dataFull[, cohort_index := 0L]

  # Set area
  if (is.integer(dataFull$area)) dataFull[, area := as.numeric(area)]
  dataFull[, area := sum(as.numeric(area)) * area_unit_conversion, by = index]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index, timestep)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))

  # Set defaults
  def_Input <- 1L
  if (timestep == 0){
    def_CO2      <- 0
    def_CH4      <- 0
    def_CO       <- 0
    def_NO2      <- 0
    def_Products <- 0
  }
  set_table_defaults(dataFull)

  # Check, order, and rename columns
  retCols <- setdiff(c(do.call(c, keyCols), do.call(c, cohortCols)), "raster_index")
  check_table_columns_all("cohorts", dataFull, retCols)
  data.table::setcolorder(dataFull, retCols)
  for (colType in names(cohortCols)){
    data.table::setnames(dataFull, cohortCols[[colType]], paste0(colType, ".", cohortCols[[colType]]), skip_absent = TRUE)
  }

  # Format classifiers
  set_table_classifiers(dataFull, classifiers)

  list(
    index = dataIndex,
    flat  = dataFull
  )
}



