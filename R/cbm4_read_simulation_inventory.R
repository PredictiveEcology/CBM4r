
#' CBM4 read simulation inventory
#'
#' Read inventory from a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template timestep
#'
#' @return `data.table`
#' @export
cbm4_read_simulation_inventory <- function(cbm4_results, timestep){

  cbm4_results <- cbm4_results_processor(cbm4_results, views = FALSE)

  cols <- cbm4_results$get_columns("simulation")
  classifiers <- gsub("^classifiers\\.", "", cols[grepl("^classifiers\\.", cols)])

  cbm4Summary <- cbm4_results_query(cbm4_results, c(
    "SELECT",
    "a.raster_index, b.chunk_index, b.cohort_index, b.cohort_proportion,",
    sprintf("b.\"classifiers.%1$s\" AS %1$s,", classifiers),
    sprintf("b.\"%1s\",", cols[grepl("^state\\.", cols)]),
    sprintf("b.\"%1s\",", cols[grepl("^pools\\.", cols)]),
    "FROM raster_index a LEFT JOIN simulation b ON a.index = b.index",
    "WHERE a.timestep =", timestep,
      "AND b.timestep =", timestep
  ))

  # Merge with pixels table
  results_dataset <- reticulate::py_get_attr(cbm4_results, "_results_dataset")$simulation_dataset
  if ("pixels" %in% results_dataset$list_tables()){

    cbm4Summary <- merge(
      cbm4Summary,
      dplyr::collect(
        results_dataset$read_table_pyarrow(
          "pixels", read_cols = c("pixel_index", "raster_index", "chunk_index"))
      ),
      by = c("raster_index", "chunk_index"))
    cbm4Summary[, c("raster_index", "chunk_index", "cohort_index") := NULL]

    data.table::setkey(cbm4Summary, pixel_index)
    data.table::setcolorder(cbm4Summary)
  }

  return(cbm4Summary)
}

