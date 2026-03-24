
#' CBM4 write disturbance
#'
#' Write disturbances to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_write_geo
#' @inheritParams cbm4_format_disturbance
#' @param ... arguments to \code{\link{cbm4_write_geo}} or \code{\link{cbm4_format_disturbance}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_disturbance <- function(
    cbm4_data = NULL,
    distMeta     = NULL,
    distEvents   = NULL,
    classifiers  = NULL,
    dataset_name = "disturbance",
    dataset_path = file.path(cbm4_data, dataset_name),
    ...
){

  # Initiate dataset
  if (!file.exists(dataset_path)) cbm4_write_geo(
    cbm4_data,
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    partitions    = list("disturbance_order" = "int64", "timestep" = "int64", "chunk_index" = "int64"),
    tags          = if (length(classifiers) > 0) list(classifier = paste0("classifiers.", classifiers)),
    ...)

  # Write disturbances
  if (!is.null(distEvents) && nrow(distEvents) > 0){

    # Format disturbances
    dist <- cbm4_format_disturbance(
      distMeta,
      distEvents,
      classifiers,
      pixelDT = arrow_space_dataset_read_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "table-pixels",
        col_select   = c("pixel_index", "chunk_index", "raster_index")
      ),
      ...)

    # Write disturbances
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = NULL,
      table_data   = dist$flat,
      partitioning = c("disturbance_order", "timestep", "chunk_index")
    )
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "raster_index",
      table_data   = dist$index,
      partitioning = c("disturbance_order", "timestep", "chunk_index")
    )
  }

  return(invisible())
}


#' CBM4 format disturbance
#'
#' @param distMeta data.table. Disturbance metadata.
#' @param distEvents data.table. Disturbance events.
#' @template classifiers
#' @template pixelDT
#' @template cbm_defaults_db
#' @param def_proportion integer. TODO
#' @param def_enable_merge integer. TODO
#' @param def_sort_id integer. TODO
#' @param def_filter_id integer. TODO
#' @param def_undisturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param def_disturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param ... unused
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_disturbance <- function(
    distMeta,
    distEvents,
    pixelDT,
    classifiers = NULL,
    cbm_defaults_db = NULL,
    def_proportion                = 1L,
    def_enable_merge              = 0L,
    def_sort_id                   = 0L,
    def_filter_id                 = 0L,
    def_undisturbed_transition_id = 0L,
    def_disturbed_transition_id   = 0L,
    ...
){

  if (length(classifiers) > 0) stop("Disturbances do not yet support classifiers.")

  # Check table columns
  check_table_columns_all("distMeta", distMeta, c("disturbance_id"))
  check_table_columns_any("distMeta", distMeta, c("disturbance_type", "disturbance_type_id"))

  check_table_columns_all("distEvents", distEvents, c("pixel_index", "disturbance_id", "timestep"))

  pixelCols <- c("pixel_index", "chunk_index", "raster_index")
  check_table_columns_all("pixelDT",  pixelDT,  pixelCols)

  # Cast to data.table
  if (!data.table::is.data.table(distEvents)) distEvents <- data.table::as.data.table(distEvents)
  if (!data.table::is.data.table(pixelDT))    pixelDT    <- data.table::as.data.table(pixelDT)

  # Join with pixel table
  dataFull <- merge(distEvents, pixelDT[, .SD, .SDcols = pixelCols], by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Set disturbance_order
  ## This sets no order to the disturbances
  if (!"disturbance_order" %in% names(dataFull)) dataFull[, disturbance_order := 0]

  # Set index
  dataFull[, index := as.integer(.GRP - 1L), by = c("disturbance_order", "timestep", "chunk_index", "disturbance_id")]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, disturbance_order, timestep, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  dataFull <- merge(dataFull, distMeta, by = "disturbance_id", all.x = TRUE)
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Set disturbance_type_id
  if (!"disturbance_type_id" %in% names(dataFull)){

    disturbance_type_tr <- cbmdbReadTable(cbm_defaults_db, "disturbance_type_tr")

    dataFull[, disturbance_type_id := disturbance_type_tr$disturbance_type_id[
      match(disturbance_type, disturbance_type_tr$name)]]
  }

  # Set disturbance_type
  if (!"disturbance_type" %in% names(dataFull)){

    disturbance_type_tr <- cbmdbReadTable(cbm_defaults_db, "disturbance_type_tr")

    dataFull[, disturbance_type := factor(
      disturbance_type_tr$name[match(disturbance_type_id, disturbance_type_tr$disturbance_type_id)],
      levels = disturbance_type_tr$name)]
  }

  # Rename disturbance_type_id column
  data.table::setnames(dataFull, "disturbance_type_id", "default_disturbance_type_id")

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




