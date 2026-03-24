
#' CBM4 write inventory
#'
#' Write inventory to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @template classifiers
#' @inheritParams cbm4_write_geo
#' @inheritParams cbm4_format_inventory
#' @param ... arguments to \code{\link{cbm4_write_geo}} or \code{\link{cbm4_format_inventory}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_inventory <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    cohortDT,
    classifiers,
    dataset_name = "inventory",
    dataset_path = file.path(cbm4_data, dataset_name),
    ...
){

  if (length(classifiers) == 0) stop(">=1 'classifiers' are required.")
  if (!all(classifiers %in% names(cohortDT))) stop("cohortDT requires all classifiers")
  if (!all(sapply(cohortDT, function(c) is.integer(c) | is.character(c) | is.factor(c))[classifiers])) stop(
    "classifiers must be integer, character, or factor")

  # Initiate dataset
  if (!file.exists(dataset_path)) cbm4_write_geo(
    cbm4_data,
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    cbm_defaults_db = cbm_defaults_db,
    partitions      = list("cohort_index" = "int64", "chunk_index" = "int64"),
    tags            = list(classifier = classifiers),
    ...)

  # Format inventory
  inv <- cbm4_format_inventory(
    cohortDT = cohortDT,
    pixelDT  = arrow_space_dataset_read_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "table-pixels"
    ),
    ...)

  # Write inventory
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = NULL,
    table_data   = inv$flat,
    partitioning = c("cohort_index", "chunk_index")
  )
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "raster_index",
    table_data   = inv$index,
    partitioning = c("cohort_index", "chunk_index")
  )

  # Write CBM defaults database to file
  arrow_space_dataset_write_file_or_dir(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    file_name    = "cbm_defaults",
    file_path    = cbm_defaults_db
  )

  return(invisible())
}


#' CBM4 format inventory
#'
#' @param cohortDT data.table. Cohort inventory.
#' @template pixelDT
#' @param def_delay integer. Regeneration delay.
#' @param def_land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
#' @param area_unit_conversion numeric. Conversion factor of area to hectares (ha).
#' @param col_ignore character. Names of `cohortDT` columns to exclude
#' @param ... unused
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_inventory <- function(
    cohortDT,
    pixelDT,
    def_delay             = 0L,
    def_land_class        = "UNFCCC_FL_R_FL", # "Forest Land remaining Forest Land"
    def_cohort_proportion = 1L,
    area_unit_conversion  = 0.0001,
    col_ignore            = NULL,
    ...
){

  # Check table columns
  check_table_columns_all("cohortDT", cohortDT, c("pixel_index", "age"))

  pixelCols <- c("pixel_index", "chunk_index", "raster_index", setdiff(c(
    "area", "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ), names(cohortDT)))
  check_table_columns_all("pixelDT", pixelDT, pixelCols)

  # Cast to data.table
  if (!data.table::is.data.table(cohortDT)) cohortDT <- data.table::as.data.table(cohortDT)
  if (!data.table::is.data.table(pixelDT))  pixelDT  <- data.table::as.data.table(pixelDT)

  # Join with pixel table
  pixelDT[, pixel_index := as.integer(pixel_index)]
  dataFull <- merge(cohortDT, pixelDT[, .SD, .SDcols = pixelCols], by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Drop columns
  col_ignore <- intersect(col_ignore, names(cohortDT))
  if (length(col_ignore) > 0) dataFull[, eval(col_ignore) := NULL]

  # Set cohort index
  if (!"cohort_index" %in% names(dataFull)) dataFull[, cohort_index := 0]

  # Set index
  dataFull[, index := as.integer(.GRP - 1L), by = setdiff(names(dataFull), c("raster_index", "area"))]

  # Set area
  ## Ensure that area is numeric, not integer
  dataFull[, area := as.numeric(area)]
  dataFull[, area := sum(area) * area_unit_conversion, by = index]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Set defaults
  for (defArg in names(environment())[grepl("^def\\_", names(environment()))]){
    defCol <- sub("^def\\_", "", defArg)
    if (!is.null(get(defArg))){
      if (!defCol %in% names(dataFull)){
        dataFull[, eval(defCol) := get(defArg)]
      }else{
        dataFull[is.na(eval(defCol)), eval(defCol) := get(defArg)]
      }
    }
  }

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}



