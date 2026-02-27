
#' CBM4 write disturbance
#'
#' Write disturbances to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @param ... arguments to \code{\link{cbm4_format_disturbance}}
#' @inheritParams cbm4_format_disturbance
#' @inheritParams cbm4_write_geo
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_disturbance <- function(
    cbm4_data = NULL,
    cbm_defaults_db = NULL,
    distMeta        = NULL,
    distEvents      = NULL,
    classifiers     = NULL,
    grid_rast       = NULL,
    grid_chunks     = 1,
    template_name   = NULL,
    template_path   = file.path(cbm4_data, template_name),
    dataset_name    = "disturbance",
    dataset_path    = file.path(cbm4_data, dataset_name),
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
    partitions    = list("disturbance_order" = "int64", "timestep" = "int64", "chunk_index" = "int64")
  )

  # Write disturbances
  if (!is.null(distEvents) && nrow(distEvents) > 0){

    # Format disturbances
    dist <- cbm4_format_disturbance(
      cbm_defaults_db = cbm_defaults_db,
      pixelDT = arrow_space_dataset_read_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "table-pixels"
      ),
      distMeta,
      distEvents,
      classifiers,
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
#' @param pixelDT TODO
#' @template cbm_defaults_db
#' @param def_proportion integer. TODO
#' @param def_enable_merge integer. TODO
#' @param def_sort_id integer. TODO
#' @param def_filter_id integer. TODO
#' @param def_undisturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param def_disturbed_transition_id integer. Set to 0 to indicate no transitions.
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_disturbance <- function(
    distMeta,
    distEvents,
    classifiers = NULL,
    pixelDT,
    cbm_defaults_db = NULL,
    def_proportion                = 1L,
    def_enable_merge              = 0L,
    def_sort_id                   = 0L,
    def_filter_id                 = 0L,
    def_undisturbed_transition_id = 0L,
    def_disturbed_transition_id   = 0L
){

  if (!is.null(classifiers)) stop("Disturbances do not yet support classifiers.")

  # Rename columns
  dataMeta <- data.table::as.data.table(distMeta)
  data.table::setnames(dataMeta, "eventID", "disturbance_id", skip_absent = TRUE)
  data.table::setnames(dataMeta, "disturbance_type_name", "disturbance_type", skip_absent = TRUE)

  dataFull <- data.table::as.data.table(distEvents)
  data.table::setnames(dataFull, "eventID", "disturbance_id", skip_absent = TRUE)
  data.table::setnames(dataFull, "pixelIndex", "pixel_index", skip_absent = TRUE)

  # Check table columns
  check_table_columns_all("distMeta",   dataMeta, c("disturbance_id"))
  check_table_columns_any("distMeta",   dataMeta, c("disturbance_type", "disturbance_type_id"))
  check_table_columns_all("distEvents", dataFull, c("pixel_index", "disturbance_id", "timestep"))

  # Choose disturbance events by priority
  multiEvents <- dataFull[, .(N = .N, disturbance_id = list(disturbance_id)), by = c("pixel_index", "timestep")][N > 1,]
  if (nrow(multiEvents) > 0){

    if (!"priority" %in% names(distMeta)) stop(
      "Multiple disturbance events found in one or more pixels. ",
      "Use the distMeta \"priority\" column to set event precendence.")

    multiEvents <- multiEvents[, .(disturbance_id = unlist(disturbance_id)), by = c("pixel_index", "timestep")]
    multiEvents <- merge(multiEvents, dataMeta, by = "disturbance_id", all.x = TRUE)

    multiEvents[, pri_highest := priority %in% min(priority), by = c("pixel_index", "timestep")]
    multiEvents <- multiEvents[pri_highest == TRUE, .(N = .N, disturbance_id = first(disturbance_id)), by = c("pixel_index", "timestep")]

    if (any(multiEvents$N > 1)) stop(
      "Multiple disturbance events found in one or more pixels ",
      "and distMeta \"priority\" indicates events have the same priority.")

    dataFull <- rbind(
      dataFull[!multiEvents, on = c("pixel_index", "timestep")],
      dataFull[multiEvents,  on = c("pixel_index", "timestep", "disturbance_id")][, .SD, .SDcols = names(dataFull)]
    )
  }

  # Join with metadata
  dataFull <- merge(dataFull, dataMeta, by = "disturbance_id", all.x = TRUE)

  # Join with pixel table
  dataFull <- merge(dataFull, pixelDT[, .(pixel_index, chunk_index, raster_index)],
                    by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Set disturbance_type
  if (!"disturbance_type" %in% names(dataFull)){

    disturbance_type_tr <- cbmdbReadTable(cbm_defaults_db, "disturbance_type_tr")

    dataFull[, disturbance_type := factor(
      disturbance_type_tr$name[match(disturbance_type_id, disturbance_type_tr$disturbance_type_id)],
      levels = disturbance_type_tr$name)]
  }

  # Set disturbance_type_id
  if (!"disturbance_type_id" %in% names(dataFull)){

    disturbance_type_tr <- cbmdbReadTable(cbm_defaults_db, "disturbance_type_tr")

    dataFull[, disturbance_type_id := disturbance_type_tr$disturbance_type_id[
      match(disturbance_type, disturbance_type_tr$name)]]
  }

  # Set disturbance_order
  ## This sets no order to the disturbances
  if (!"disturbance_order" %in% names(dataFull)) dataFull[, disturbance_order := 0]

  # Set index
  dataFull[, index := .GRP - 1, by = c("disturbance_order", "timestep", "chunk_index", "disturbance_id")]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, disturbance_order, timestep, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Set defaults
  for (defArg in names(environment())[grepl("^def\\_", names(environment()))]){
    defCol <- sub("^def\\_", "", defArg)
    if (!defCol %in% names(dataFull)){
      dataFull[, eval(defCol) := get(defArg)]
    }else{
      data.table::setnafill(dataFull, cols = defCol, fill = get(defArg))
    }
  }

  # Rename disturbance_type_id column
  data.table::setnames(dataFull, "disturbance_type_id", "default_disturbance_type_id")

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}




