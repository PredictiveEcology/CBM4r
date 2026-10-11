
#' CBM4 results timestep
#'
#' Read simulation results by timestep.
#'
#' @inheritParams cbm4_results_view
#' @param units character. "t/ha", "t", "Mt".
#'
#' @return `data.table`
#' @export
cbm4_results_timestep <- function(
    cbm4_results,
    view_name,
    view_col  = NULL,
    timesteps = NULL,
    units     = "t",
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & !grepl("^spatial_", name)]
  )

  queryTbl <- cbm4_results_view(
    cbm4_results = cbm4_results,
    view_name    = view_name,
    view_col     = view_col,
    group_by     = "timestep",
    timesteps    = timesteps
  )

  if (units != "t"){

    unitOpts <- c("t/ha", "t", "Mt")
    if (!units %in% unitOpts) stop(
      "invalid unit ", shQuote(units),
      ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

    view_col <- setdiff(names(queryTbl), "timestep")

    if (units == "Mt"){
      queryTbl <- queryTbl[, (view_col) := lapply(.SD, function(x) x / 10^6), .SDcols = view_col]
    }

    if (units == "t/ha"){

      tAreas <- cbm4_results_query(cbm4_results, c(
        "SELECT timestep, SUM(\"inventory.area\" * cohort_proportion) AS area FROM simulation",
        if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
        "GROUP BY timestep"
      ))

      queryTbl <- queryTbl[tAreas, area := area, on = "timestep"]
      data.table::setcolorder(queryTbl, c(data.table::key(queryTbl), "area"))
      queryTbl <- queryTbl[, (view_col) := lapply(.SD, function(x) x / area), .SDcols = view_col]
    }
  }

  data.table::setkey(queryTbl, timestep)
  return(queryTbl)
}



