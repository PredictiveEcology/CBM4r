
#' CBM4 results pixel
#'
#' Read simulation results by pixel with carbon in tonnes per hectare (t/ha).
#'
#' @inheritParams cbm4_results_view
#' @param rast logical. Return results as a terra `SpatRaster`.
#' @template grid_meta_optional
#'
#' @return `data.table` or `SpatRaster`
#' @export
cbm4_results_pixel <- function(
    cbm4_results,
    view_name,
    view_col  = NULL,
    timesteps = NULL,
    grid_meta = NULL,
    rast      = FALSE,
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & grepl("^spatial_", name)]
  )

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view_name    = view_name,
    view_col     = view_col,
    group_by     = c("timestep", "chunk_index", "raster_index"),
    timesteps    = timesteps
  )

  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)
  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]
  queryTbl[, c("chunk_index", "raster_index") := NULL]
  data.table::setkey(queryTbl, timestep, pixel_index)
  data.table::setcolorder(queryTbl)

  if (!rast){

    return(queryTbl)

  }else{

    cbm4_grid <- cbm4_results_grid(cbm4_results)

    if (length(view_col) != 1) stop("view_col must be length 1 if rast = TRUE")
    data.table::setnames(queryTbl, view_col, "value")

    if (is.null(timesteps)) timesteps <- unique(queryTbl$timestep)

    queryRast <- do.call(c, lapply(timesteps, function(t){
      t_rast <- terra::rast(cbm4_grid, vals = NA)
      t_vals <- queryTbl[timestep == t,]
      if (nrow(t_vals) > 0){
        terra::set.values(t_rast, t_vals$pixel_index, t_vals$value)
      }
      t_rast
    }))
    if (length(timesteps) > 0) names(queryRast) <- timesteps

    return(queryRast)
  }
}



