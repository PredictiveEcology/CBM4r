
#' CBM4 results pixel
#'
#' Read simulation results by pixel with carbon in tonnes per hectare (t/ha).
#'
#' @inheritParams cbm4_results_view
#' @param view_name character. `SQLResultsProcessor` view name.
#' If NULL the function will return an empty study area grid.
#' @param view_col character. `SQLResultsProcessor` view column name.
#' @template grid_meta_optional
#'
#' @return `SpatRaster`
#' @export
cbm4_results_pixel <- function(
    cbm4_results,
    view_name = NULL,
    view_col  = NULL,
    timesteps = NULL,
    grid_meta = NULL,
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & grepl("^spatial_", name)]
  )

  cbm4_grid <- cbm4_results_grid(cbm4_results)
  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)

  if (is.null(view_name)) return(cbm4_grid)

  if (length(view_col) > 1) stop("view_col must be length 1")

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view_name    = view_name,
    view_col     = setNames(view_col, "value"),
    group_by     = c("timestep", "chunk_index", "raster_index"),
    timesteps    = timesteps
  )

  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]

  if (is.null(timesteps)) timesteps <- unique(queryTbl$timestep)

  cbm4_rast <- do.call(c, lapply(timesteps, function(t){
    t_rast <- terra::rast(cbm4_grid, vals = NA)
    t_vals <- queryTbl[timestep == t,]
    if (nrow(t_vals) > 0){
      terra::set.values(t_rast, t_vals$pixel_index, t_vals$value)
    }
    t_rast
  }))
  if (length(timesteps) > 0) names(cbm4_rast) <- timesteps

  return(cbm4_rast)
}



