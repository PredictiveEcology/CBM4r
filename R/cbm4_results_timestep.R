
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
    view,
    select    = NULL,
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
    view         = view,
    select       = select,
    group_by     = "timestep",
    timesteps    = timesteps
  )

  if (units != "t"){

    unitOpts <- c("t/ha", "t", "Mt")
    if (!units %in% unitOpts) stop(
      "invalid unit ", shQuote(units),
      ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

    select <- setdiff(names(queryTbl), "timestep")

    if (units == "Mt"){
      queryTbl <- queryTbl[, (select) := lapply(.SD, function(x) x / 10^6), .SDcols = select]
    }

    if (units == "t/ha"){

      tAreas <- cbm4_results_query(cbm4_results, c(
        "SELECT timestep, SUM(\"inventory.area\" * cohort_proportion) AS area FROM simulation",
        if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
        "GROUP BY timestep"
      ))

      queryTbl <- queryTbl[tAreas, area := area, on = "timestep"]
      data.table::setcolorder(queryTbl, c(data.table::key(queryTbl), "area"))
      queryTbl <- queryTbl[, (select) := lapply(.SD, function(x) x / area), .SDcols = select]
    }
  }

  data.table::setkey(queryTbl, timestep)
  return(queryTbl)
}



