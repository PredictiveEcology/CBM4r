
#' CBM4 results raster
#'
#' Read simulation results into a raster with carbon in tonnes per hectare (t/ha).
#'
#' @template cbm4_results
#' @param view_name character. `SQLResultsProcessor` view name.
#' If NULL the function will return an empty study area grid.
#' @param view_column character. `SQLResultsProcessor` view column name.
#' @template timesteps
#' @param list logical. Return a table of options.
#'
#' @return `SpatRaster`
#' @export
cbm4_results_raster <- function(
    cbm4_results,
    view_name   = NULL,
    view_column = NULL,
    timesteps   = NULL,
    grid_meta   = NULL,
    list        = FALSE
){

  cbm4_results <- cbm4_results_processor(cbm4_results)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & grepl("^spatial_", name)]
  )

  cbm4_grid <- cbm4_results_grid(cbm4_results)
  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)

  if (is.null(view_name)) return(cbm4_grid)

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
  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]

  if (is.null(timesteps)) timesteps <- unique(queryTbl$timestep)

  cbm4_rast <- do.call(c, lapply(timesteps, function(t){
    cbm4_rast_t <- terra::deepcopy(cbm4_grid)
    with(queryTbl[timestep == t,], terra::set.values(cbm4_rast_t, pixel_index, value))
    cbm4_rast_t
  }))
  names(cbm4_rast) <- timesteps

  return(cbm4_rast)
}



