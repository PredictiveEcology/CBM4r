
#' CBM4 write disturbance
#'
#' Write disturbances to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_format_disturbance
#' @param ... arguments to \code{\link{cbm4_format_disturbance}}
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#' @template grid_meta_optional
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_disturbance <- function(
    cbm4_data     = NULL,
    dist_meta     = NULL,
    dist_events   = NULL,
    classifiers   = NULL,
    grid_meta     = NULL,
    template_name = "inventory",
    template_path = file.path(cbm4_data, template_name),
    dataset_name  = "disturbance",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){

    if (is.null(template_name)) stop("Use `cbm4_write_geo` to initiate a new dataset or copy dataset attributes by setting `template_name.")

    if (is.null(classifiers)){
      classifiers <- arrow_space_dataset_read_table(
        dataset_name = template_name,
        dataset_path = template_path,
        table_name   = "tags"
      )[tag == "classifier", layer_name]
    }

    arrow_space_dataset_copy_geo(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      partitions    = cbm4_schema(c("disturbance_order", "timestep", "chunk_index")),
      tags          = if (length(classifiers) > 0) list(classifier = paste0("classifiers.", classifiers))
    )
  }

  if (!is.null(dist_events) && nrow(dist_events) > 0){

    # Format disturbances
    if (is.null(classifiers)){
      classifiers <- gsub("^classifiers\\.", "", arrow_space_dataset_read_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "tags"
      )[tag == "classifier", layer_name])
    }

    if (is.null(grid_meta) & !is.null(template_name)) grid_meta <- tryCatch(
      cbm4_results_grid_key(cbm4_data, template_name),
      error = function(e) NULL)
    if (is.null(grid_meta)) grid_meta <- tryCatch(
      cbm4_results_grid_key(cbm4_data, dataset_name),
      error = function(e) stop("grid_meta argument required"))

    dist <- cbm4_format_disturbance(
      grid_meta   = grid_meta,
      dist_meta   = dist_meta,
      dist_events = dist_events,
      classifiers = classifiers,
      ...)

    # Write disturbances
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = NULL,
      table_data   = dist$flat,
      schema       = cbm4_schema(dist$flat),
      partitioning = c("disturbance_order", "timestep", "chunk_index")
    )
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "raster_index",
      table_data   = dist$index,
      schema       = cbm4_schema(dist$index),
      partitioning = c("disturbance_order", "timestep", "chunk_index")
    )

    if (!is.null(dist$cohort_filter)){
      arrow_space_dataset_write_table(
        dataset_name = dataset_name,
        dataset_path = dataset_path,
        table_name   = "table-cohort_filter",
        table_data   = dist$cohort_filter,
        schema       = cbm4_schema(dist$cohort_filter)
      )
    }
  }

  return(invisible())
}


#' CBM4 format disturbance
#'
#' @template grid_meta
#' @param dist_meta data.table. Disturbance metadata.
#' @param dist_events data.table. Disturbance events.
#' @template classifiers
#' @template cbm_defaults_db
#' @param def_proportion integer. TODO
#' @param def_enable_merge integer. TODO
#' @param def_sort_id integer. TODO
#' @param def_undisturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param def_disturbed_transition_id integer. Set to 0 to indicate no transitions.
#' @param ... unused
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_disturbance <- function(
    grid_meta,
    dist_meta,
    dist_events,
    classifiers = NULL,
    def_proportion                = 1L,
    def_enable_merge              = 0L,
    def_sort_id                   = 0L,
    def_undisturbed_transition_id = 0L,
    def_disturbed_transition_id   = 0L,
    cbm_defaults_db = getOption("CBM4r.db.path"),
    ...
){

  # Check table columns
  check_table_columns_all("dist_meta", dist_meta, c("disturbance_id"))
  check_table_columns_any("dist_meta", dist_meta, c("disturbance_type", "disturbance_type_id"))

  check_table_columns_all("dist_events", dist_events, c("pixel_index", "disturbance_id", "timestep"))

  gridCols <- c("pixel_index", "chunk_index", "raster_index")
  check_table_columns_all("grid_meta", grid_meta, gridCols)

  # Cast to data.table
  if (!data.table::is.data.table(dist_events)) dist_events <- data.table::as.data.table(dist_events)
  if (!data.table::is.data.table(grid_meta))   grid_meta   <- data.table::as.data.table(grid_meta)

  # Join with pixel table
  dataFull <- merge(dist_events, grid_meta[, .SD, .SDcols = gridCols], by = "pixel_index")
  dataFull[, pixel_index := NULL]

  # Set disturbance_order
  ## This sets no order to the disturbances
  if (!"disturbance_order" %in% names(dataFull)) dataFull[, disturbance_order := 0L]

  # Set index
  dataFull[, index := .GRP - 1L, by = c("disturbance_order", "timestep", "chunk_index", "disturbance_id")]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, disturbance_order, timestep, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  dataFull <- merge(dataFull, dist_meta, by = "disturbance_id", all.x = TRUE)
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Set disturbance_type_id
  if (!"disturbance_type_id" %in% names(dataFull)){

    disturbance_type_tr <- cbm_defaults_readTable("disturbance_type_tr", cbm_defaults_db)

    dataFull[, disturbance_type_id := disturbance_type_tr$disturbance_type_id[
      match(disturbance_type, disturbance_type_tr$name)]]

    if (anyNA(dataFull$disturbance_type_id)) stop(
      "disturbance_type_id not found for: ",
      paste(shQuote(unique(dataFull[is.na(disturbance_type_id),]$disturbance_type)), collapse = ", "))
  }

  # Set disturbance_type
  if (!"disturbance_type" %in% names(dataFull)){

    disturbance_type_tr <- cbm_defaults_readTable("disturbance_type_tr", cbm_defaults_db)

    dataFull[, disturbance_type := factor(
      disturbance_type_tr$name[match(disturbance_type_id, disturbance_type_tr$disturbance_type_id)],
      levels = disturbance_type_tr$name)]
  }

  # Rename disturbance_type_id column
  data.table::setnames(dataFull, "disturbance_type_id", "default_disturbance_type_id")

  # Set defaults
  dataFull[, filter_id := 0L]
  set_table_defaults(dataFull)

  # Set tables to return
  ret <-  list(
    index = dataIndex,
    flat  = dataFull
  )

  # Set filters
  classifiers <- intersect(classifiers, names(dist_meta))
  if (length(classifiers) > 0){

    cohort_filter <- dataFull[, .SD, .SDcols = classifiers]
    cohort_filter[, filter := rowSums(!is.na(cohort_filter)) > 0]
    cohort_filter[, row_id := .I]
    cohort_filter <- cohort_filter[filter == TRUE]
    cohort_filter[, id := as.integer(.GRP), by = classifiers]

    ret$flat[, (classifiers) := NULL]
    ret$flat$filter_id[cohort_filter$row_id] <- cohort_filter$id

    cohort_filter[, filter := NULL]
    cohort_filter[, row_id := NULL]

    cohort_filter <- unique(cohort_filter)
    data.table::setkey(cohort_filter, id)
    data.table::setcolorder(cohort_filter)

    isChar <- !sapply(cohort_filter, is.numeric)
    isChar <- names(isChar)[isChar]
    cohort_filter[, value := apply(cohort_filter, 1, function(r){

      filt <- r[classifiers]
      filt <- filt[!sapply(filt, is.na)]
      filt[names(filt) %in% isChar] <- shQuote(filt[names(filt) %in% isChar])

      paste0("[[", paste(
        sprintf("[\"classifiers.%s\", \"==\", %s]", names(filt), filt),
        collapse = ", "), "]]")
    })]

    cohort_filter[, (classifiers) := NULL]

    ret$cohort_filter <- rbind(data.table::data.table(id = 0L, value = "[]"), cohort_filter)
  }

  # Return
  ret
}




