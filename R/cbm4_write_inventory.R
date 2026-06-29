
#' CBM4 write inventory
#'
#' Write inventory to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @inheritParams cbm4_write_geo
#' @inheritParams cbm4_format_inventory
#' @param ... arguments to \code{\link{cbm4_format_inventory}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_inventory <- function(
    cbm4_data       = NULL,
    cohorts         = NULL,
    classifiers     = NULL,
    grid_meta       = NULL,
    grid_rast       = NULL,
    dataset_name    = "inventory",
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path"),
    ...
){

  # Initiate dataset
  if (!file.exists(dataset_path)){

    if (length(classifiers) == 0) stop(">=1 classifiers are required.")

    cbm4_write_geo(
      cbm4_data,
      dataset_name    = dataset_name,
      dataset_path    = dataset_path,
      grid_meta       = grid_meta,
      grid_rast       = grid_rast,
      cbm_defaults_db = cbm_defaults_db,
      partitions      = cbm4_schema(c("cohort_index", "chunk_index")),
      tags            = list(classifier = classifiers),
      write_pixels    = TRUE
    )

    # Write CBM defaults database to file
    arrow_space_dataset_write_file_or_dir(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      file_name    = "cbm_defaults",
      file_path    = cbm_defaults_db
    )
  }

  if (!is.null(cohorts)){

    # Format inventory
    if (is.null(classifiers)){
      classifiers <- arrow_space_dataset_read_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "tags"
      )[tag == "classifier", layer_name]
    }

    inv <- cbm4_format_inventory(
      grid_meta   = grid_meta,
      cohorts     = cohorts,
      classifiers = classifiers,
      ...)

    # Write inventory
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = NULL,
      table_data   = inv$flat,
      schema       = cbm4_schema(inv$flat),
      partitioning = c("cohort_index", "chunk_index")
    )
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "raster_index",
      table_data   = inv$index,
      schema       = cbm4_schema(inv$index),
      partitioning = c("cohort_index", "chunk_index")
    )
  }

  return(invisible())
}


#' CBM4 format inventory
#'
#' @template grid_meta
#' @param cohorts data.table. Cohort inventory.
#' @template classifiers
#' @param def_delay integer. Regeneration delay.
#' @param def_land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param col_ignore character. Names of `cohorts` columns to exclude
#' @param ... unused
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_inventory <- function(
    grid_meta,
    cohorts,
    classifiers,
    def_delay             = 0L,
    def_cohort_proportion = 1L,
    def_land_class        = "UNFCCC_FL_R_FL", # "Forest Land remaining Forest Land"
    area_unit_conversion  = 0.0001,
    col_ignore            = NULL,
    ...
){

  # Check classifiers
  if (length(classifiers) == 0) stop(">=1 classifiers are required.")

  # Check table columns
  check_table_columns_all("cohorts", cohorts, c("pixel_index", "age", classifiers))

  gridCols <- c(
    "pixel_index", "chunk_index", "raster_index",
    "area", "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  )
  check_table_columns_all("grid_meta", grid_meta, gridCols)

  # Cast to data.table
  if (!data.table::is.data.table(cohorts))   cohorts   <- data.table::as.data.table(cohorts)
  if (!data.table::is.data.table(grid_meta)) grid_meta <- data.table::as.data.table(grid_meta)

  # Join with pixel table
  dataFull <- merge(cohorts, grid_meta[, .SD, .SDcols = gridCols], by = "pixel_index", all.x = TRUE)

  # Drop columns
  col_ignore <- intersect(col_ignore, names(cohorts))
  if (length(col_ignore) > 0) dataFull[, eval(col_ignore) := NULL]

  # Set index and chunk_index
  dataFull[, index := .GRP - 1L, by = setdiff(names(dataFull), c("pixel_index", "raster_index", "area"))]
  dataFull[, pixel_index := NULL]

  # Set cohort_index to 0
  if (!"cohort_index" %in% names(dataFull)) dataFull[, cohort_index := 0L]

  # Set area
  if (is.integer(dataFull$area)) dataFull[, area := as.numeric(area)]
  dataFull[, area := sum(area) * area_unit_conversion, by = index]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Set defaults
  set_table_defaults(dataFull)

  # Format classifiers
  set_table_classifiers(dataFull, classifiers)

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}



