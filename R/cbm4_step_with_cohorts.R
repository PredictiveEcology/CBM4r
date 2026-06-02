
#' CBM4 step: with cohorts
#'
#' Run an annual step on CBM4 spatial parquet datasets with an alternate set of cohort data.
#'
#' @inherit cbm4_format_simulation_cohorts params
#' @inherit cbm4_step params return
#' @param ... arguments to \code{\link{cbm4_step}}
#'
#' @return `NULL`. Updates will be made to CBM4 spatial parquet datasets.
#' @export
cbm4_step_with_cohorts <- function(
    cbm4_data = NULL,
    timestep,
    cohorts   = NULL,
    grid_meta = NULL,
    area_unit_conversion   = 0.0001,
    def_cohort_proportion  = 1L,
    def_enabled            = 1L,
    def_growth_enabled     = 1L,
    def_growth_multiplier  = 1L,
    def_regeneration_delay = 0L,
    def_land_class         = "UNFCCC_FL_R_FL",  # "Forest Land remaining Forest Land"
    simulation_dataset = file.path(cbm4_data, "simulation"),
    ...
){

  # Temporarily replace cohort data
  if (!is.null(cohorts)){

    schema <- arrow::schema(arrow::open_dataset(file.path(simulation_dataset, "simulation")))

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

    cbm4_write_simulation_cohorts(
      dataset_path = simulation_dataset,
      cohorts      = cohorts,
      grid_meta    = grid_meta,
      timestep     = timestep - 1,
      def_cohort_proportion  = def_cohort_proportion,
      def_enabled            = def_enabled,
      def_growth_enabled     = def_growth_enabled,
      def_growth_multiplier  = def_growth_multiplier,
      def_regeneration_delay = def_regeneration_delay,
      def_land_class         = def_land_class,
      area_unit_conversion   = area_unit_conversion,
      schema = schema
    )
  }

  # Step
  cbm4_step(
    cbm4_data = cbm4_data,
    simulation_dataset = simulation_dataset,
    timestep = timestep,
    ...
  )
}


#' CBM4 write simulation cohorts
#'
#' Write cohort inventory to a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_format_simulation_cohorts
#' @param ... arguments to \code{\link{cbm4_format_simulation_cohorts}}
#' @template dataset_name
#' @template dataset_path
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @keywords internal
cbm4_write_simulation_cohorts <- function(
    cbm4_data = NULL,
    cohorts,
    grid_meta = NULL,
    timestep,
    dataset_name = "simulation",
    dataset_path = file.path(cbm4_data, dataset_name),
    schema = NULL,
    ...
){

  # Set classifiers
  tags <- arrow_space_dataset_read_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "tags"
  )
  classifiers <- gsub("^classifiers\\.", "", tags$layer_name)

  # Format inventory
  inv <- cbm4_format_simulation_cohorts(
    grid_meta   = grid_meta,
    cohorts     = cohorts,
    classifiers = classifiers,
    timestep    = timestep,
    ...)

  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = NULL,
    table_data   = inv$flat[, .SD, .SDcols = intersect(names(schema), names(inv$flat))],
    partitioning = c("timestep", "cohort_index", "chunk_index"),
    schema       = if (!is.null(schema)) schema[names(inv$flat)]
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
#' @param cohorts data.table. Cohort inventory.
#' @template timestep
#' @template classifiers
#' @template grid_meta_optional
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
#' @param def_enabled integer. TODO
#' @param def_growth_enabled integer. TODO
#' @param def_growth_multiplier integer. TODO
#' @param def_regeneration_delay integer. Regeneration delay.
#' @param def_land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
#' @keywords internal
cbm4_format_simulation_cohorts <- function(
    cohorts,
    timestep,
    classifiers,
    grid_meta = NULL,
    def_cohort_proportion  = 1L,
    def_enabled            = 1L,
    def_growth_enabled     = 1L,
    def_growth_multiplier  = 1L,
    def_regeneration_delay = 0L,
    def_land_class         = "UNFCCC_FL_R_FL",  # "Forest Land remaining Forest Land"
    area_unit_conversion   = 0.0001
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




