
#' CBM4 results pixel
#'
#' Read simulation results by pixel.
#'
#' @inheritParams cbm4_results_view
#' @param units character. "t/ha", "t", "Mt".
#' @template grid_meta_optional
#' @param rast logical. Return results as a terra `SpatRaster`.
#'
#' @return `data.table` or `SpatRaster`
#' @export
cbm4_results_pixel <- function(
    cbm4_results,
    view,
    select    = NULL,
    timesteps = NULL,
    units     = "t/ha",
    grid_meta = NULL,
    rast      = FALSE,
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("^spatial_.*_indicators$", name) & !grepl("age_indicators", name)]
  )

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view         = view,
    select       = select,
    group_by     = c("timestep", "chunk_index", "raster_index"),
    timesteps    = timesteps
  )

  if (units != "t/ha"){

    unitOpts <- c("t/ha", "t", "Mt")
    if (!units %in% unitOpts) stop(
      "invalid unit ", shQuote(units),
      ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

    pools <- setdiff(names(queryTbl), data.table::key(queryTbl))

    # Get area
    queryTbl <- queryTbl[cbm4_results_view(cbm4_results, "area"), area := area, on = c("chunk_index", "raster_index")]

    # Convert to t
    queryTbl[, (pools) := lapply(.SD, function(x) x * area), .SDcols = pools]

    # Convert to Mt
    if (units == "Mt"){
      queryTbl[, (pools) := lapply(.SD, function(x) x / 10^6), .SDcols = pools]
    }

    queryTbl[, area := NULL]
  }

  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)
  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]
  queryTbl[, c("chunk_index", "raster_index") := NULL]
  data.table::setkey(queryTbl, timestep, pixel_index)
  data.table::setcolorder(queryTbl)

  if (!rast){

    return(queryTbl)

  }else{

    cbm4_grid <- cbm4_results_grid(cbm4_results)

    if (length(select) != 1) stop("select must be length 1 if rast = TRUE")
    data.table::setnames(queryTbl, select, "value")

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



