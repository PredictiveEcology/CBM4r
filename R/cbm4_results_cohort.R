
#' CBM4 results cohort
#'
#' Read simulation results by cohort.
#'
#' @inheritParams cbm4_results_view
#' @param units character. "t/ha", "t", "Mt".
#' @template grid_meta_optional
#'
#' @return `data.table`
#' @export
cbm4_results_cohort <- function(
    cbm4_results,
    view,
    select    = NULL,
    timesteps = NULL,
    units     = "t/ha",
    grid_meta = NULL,
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("^spatial_.*_indicators$", name) & !grepl("age_indicators", name)]
  )

  sim_cols <- subset(cbm4_results$views, name == "simulation")$column_names[[1]]
  classifiers <- gsub("^classifiers\\.", "", sim_cols[grepl("^classifiers\\.", sim_cols)])

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view         = view,
    select       = if (!is.null(select)) unique(c(
      "timestep", "chunk_index", "raster_index",
      if (units != "t/ha" && "area" %in% subset(cbm4_results$views, name == view)$column_names[[1]]) "area",
      classifiers,
      unlist(unname(select))
    )),
    timesteps    = timesteps
  )

  if (units != "t/ha"){

    unitOpts <- c("t/ha", "t", "Mt")
    if (!units %in% unitOpts) stop(
      "invalid unit ", shQuote(units),
      ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

    pools <- intersect(
      .cbm4_results_view_pool_columns(cbm4_results, view),
      names(queryTbl)
    )

    # Get area
    if (!"area" %in% names(queryTbl)){
      queryTbl <- queryTbl[cbm4_results_view(cbm4_results, "area"), area := area, on = c("chunk_index", "raster_index")]
    }

    # Convert to t
    queryTbl[, (pools) := lapply(.SD, function(x) x * area), .SDcols = pools]

    # Convert to Mt
    if (units == "Mt"){
      queryTbl[, (pools) := lapply(.SD, function(x) x / 10^6), .SDcols = pools]
    }

    if (!is.null(select) && !"area" %in% unlist(unname(select))) queryTbl[, area := NULL]
  }

  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)
  queryTbl <- queryTbl[grid_meta, pixel_index := pixel_index, on = c("chunk_index", "raster_index")]
  queryTbl[, c("chunk_index", "raster_index") := NULL]
  data.table::setkeyv(queryTbl, c("timestep", "pixel_index", classifiers))
  data.table::setcolorder(queryTbl, c(data.table::key(queryTbl), intersect(names(queryTbl), "area")))

  if (!is.null(select) && !is.null(names(select))){
    data.table::setnames(queryTbl, unlist(unname(select)), names(select))
  }

  return(queryTbl)
}



