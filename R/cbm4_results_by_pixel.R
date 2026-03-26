
#' CBM4 results: pools by pixel
#' @inherit .cbm4_results_by_pixel params return
#' @export
cbm4_results_pools_by_pixel <- function(cbm4_results, timestep){
  .cbm4_results_by_pixel(cbm4_results, "pools_by_pixel", timestep = timestep)
}

#' CBM4 results: flux by pixel
#' @inherit .cbm4_results_by_pixel params return
#' @export
cbm4_results_flux_by_pixel <- function(cbm4_results, timestep){
  .cbm4_results_by_pixel(cbm4_results, "flux_by_pixel", timestep = timestep)
}


#' CBM4 results: by pixel
#' @template timestep
#' @inherit .cbm4_results_query_preset params return
#' @keywords internal
.cbm4_results_by_pixel <- function(
    cbm4_results,
    queryName,
    timestep = NULL
){

  cbm4_results <- cbm4_results_processor(cbm4_results)

  cbm4Summary <- .cbm4_results_query_preset(
    cbm4_results, queryName,
    where = if (!is.null(timestep)) paste(
      "a.timestep IN (", paste(timestep, collapse = ", "), ")", "AND",
      "b.timestep IN (", paste(timestep, collapse = ", "), ")")
  )
  if (length(timestep) == 1) cbm4Summary[, timestep := NULL]

  cbm4Summary <- merge(
    cbm4Summary,
    dplyr::collect(
      reticulate::py_get_attr(cbm4_results, "_results_dataset")$simulation_dataset$read_table_pyarrow(
        "pixels", read_cols = c("pixel_index", "raster_index", "chunk_index"))
    ),
    by = c("raster_index", "chunk_index"))
  cbm4Summary[, c("raster_index", "chunk_index") := NULL]

  data.table::setkeyv(cbm4Summary, c("timestep"[length(timestep) > 1], "pixel_index"))
  data.table::setcolorder(cbm4Summary)
  return(cbm4Summary)
}


