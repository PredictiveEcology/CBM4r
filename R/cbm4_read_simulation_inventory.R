
#' CBM4 read simulation inventory
#'
#' Read inventory from a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template timestep
#' @template grid_meta
#'
#' @return `data.table`
#' @export
cbm4_read_simulation_inventory <- function(cbm4_results, timestep, grid_meta = NULL){

  cbm4_results <- cbm4_results_processor(cbm4_results, views = FALSE)

  cols <- cbm4_results$get_columns("simulation")
  classifiers <- gsub("^classifiers\\.", "", cols[grepl("^classifiers\\.", cols)])

  cbm4_inv <- cbm4_results_query(cbm4_results, c(
    "SELECT",
    "a.raster_index, b.chunk_index, b.cohort_index, b.cohort_proportion,",
    sprintf("b.\"classifiers.%1$s\" AS %1$s,", classifiers),
    sprintf("b.\"%1s\",", cols[grepl("^state\\.", cols)]),
    sprintf("b.\"%1s\",", cols[grepl("^pools\\.", cols)]),
    "FROM raster_index a LEFT JOIN simulation b ON a.index = b.index",
    "WHERE a.timestep =", timestep,
      "AND b.timestep =", timestep
  ))

  # Set pixel index
  if (!is.null(grid_meta)){
    cbm4_inv <- cbm4_inv[grid_meta, pixel_index := pixel_index, on = c("raster_index", "chunk_index")]
    data.table::setkey(cbm4_inv, pixel_index)
    data.table::setcolorder(cbm4_inv)
    cbm4_inv[, chunk_index  := NULL]
    cbm4_inv[, raster_index := NULL]
  }

  return(cbm4_inv)
}


