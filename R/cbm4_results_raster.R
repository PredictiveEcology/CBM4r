
#' CBM4 results raster
#'
#' Read simulation results into a raster with carbon in tonnes per hectare (t/ha).
#'
#' @template cbm4_results
#' @param view_name character. `SQLResultsProcessor` view name.
#' @param view_column character. `SQLResultsProcessor` view column name.
#' @template timesteps
#' @param list logical. Return a table of options.
#'
#' @return `SpatRaster`
#' @export
cbm4_results_raster <- function(cbm4_results, view_name, view_column, timesteps = NULL, list = FALSE){

  cbm4_results <- cbm4_results_processor(cbm4_results)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & grepl("^spatial_", name)]
  )

  querySQL <- sprintf(paste(
    "SELECT timestep, chunk_index, raster_index,",
    "SUM(\"%1$s\") AS value",
    "FROM %2$s",
    if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
    "GROUP BY timestep, chunk_index, raster_index",
    "ORDER BY timestep, chunk_index, raster_index"
  ), view_column, view_name)

  queryTbl <- tryCatch(
    cbm4_results_query(cbm4_results, querySQL),
    error = function(e) stop(e$message, "\n", "Call cbm_results_raster with list = TRUE to view options")
  )

  if (is.null(timesteps)) timesteps <- unique(queryTbl$timestep)

  cbm4Geo  <- cbm4_read_geo(cbm4_results)
  cbm4Rast <- do.call(c, lapply(timesteps, function(t){
    cbm4Rast_t <- terra::deepcopy(cbm4Geo)
    with(queryTbl[timestep == t,], terra::set.values(cbm4Rast_t, raster_index + 1, value))
    cbm4Rast_t
  }))
  names(cbm4Rast) <- timesteps

  return(cbm4Rast)
}



