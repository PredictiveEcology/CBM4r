
#' CBM4 results cohort
#'
#' Read simulation results by cohort.
#'
#' @inheritParams cbm4_results_view
#' @template grid_meta_optional
#' @param units character. "t/ha", "t", "Mt".
#'
#' @return `data.table`
#' @export
cbm4_results_cohort <- function(
    cbm4_results,
    view_name,
    select    = NULL,
    timesteps = NULL,
    grid_meta = NULL,
    units     = "t/ha",
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & grepl("^spatial_", name)]
  )

  sim_cols <- subset(cbm4_results$views, name == "simulation")$column_names[[1]]
  classifiers <- gsub("^classifiers\\.", "", sim_cols[grepl("^classifiers\\.", sim_cols)])

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view_name    = view_name,
    select       = select,
    group_by     = c("timestep", "chunk_index", "raster_index", classifiers),
    timesteps    = timesteps
  )

  if (units != "t/ha"){

    unitOpts <- c("t/ha", "t", "Mt")
    if (!units %in% unitOpts) stop(
      "invalid unit ", shQuote(units),
      ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

    select <- setdiff(names(queryTbl), data.table::key(queryTbl))

    # Get area
    queryTbl <- queryTbl[cbm4_results_view(cbm4_results, "area"), area := area, on = c("chunk_index", "raster_index")]
    data.table::setcolorder(queryTbl, c(data.table::key(queryTbl), "area"))

    # Convert to t
    queryTbl[, (select) := lapply(.SD, function(x) x * area), .SDcols = select]

    # Convert to Mt
    if (units == "Mt"){
      queryTbl[, (select) := lapply(.SD, function(x) x / 10^6), .SDcols = select]
    }
  }

  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)
  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]
  queryTbl[, c("chunk_index", "raster_index") := NULL]
  data.table::setkeyv(queryTbl, c("timestep", "pixel_index", classifiers))
  data.table::setcolorder(queryTbl)

  return(queryTbl)
}



