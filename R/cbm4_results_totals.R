
#' CBM4 results totals
#'
#' Read simulation results into a table with carbon totals per timestep.
#'
#' @template cbm4_results
#' @param view_name character. `SQLResultsProcessor` view name.
#' @param view_columns character. `SQLResultsProcessor` view column names.
#' The vector can have names to use as column aliases.
#' @param units character. "t", "Mt", or "t/ha".
#' @template timesteps
#' @param list logical. Return a table of options.
#'
#' @return `data.table`
#' @export
cbm4_results_totals <- function(cbm4_results, view_name, view_columns = NULL, timesteps = NULL,
                                units = "t", list = FALSE){

  unitOpts <- c("t", "Mt", "t/ha")
  if (!units %in% unitOpts) stop(
    "invalid unit ", shQuote(units),
    ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

  cbm4_results <- cbm4_results_processor(cbm4_results)

  if (list) return(
    data.table::as.data.table(cbm4_results$views)[, .(name, column_names)][
      grepl("_indicators$", name) & !grepl("age_indicators", name) & !grepl("^spatial_", name)]
  )

  if (is.null(view_columns)){
    view_columns <- setdiff(
      cbm4_results$views[cbm4_results$views$name == view_name,]$column_names[[1]], c(
        cbm4_results$views[cbm4_results$views$name == "age_indicators",]$column_names[[1]],
        "disturbance_type_id", "disturbance_type"
      ))
  }
  if (is.null(names(view_columns))) names(view_columns) <- view_columns

  querySQL <- paste(
    "SELECT timestep,",
    paste(sprintf("SUM(\"%s\") AS \"%s\"", view_columns, names(view_columns)), collapse = ", "),
    "FROM", view_name,
    if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
    "GROUP BY timestep",
    "ORDER BY timestep"
  )

  queryTbl <- tryCatch(
    cbm4_results_query(cbm4_results, querySQL),
    error = function(e) stop(e$message, "\n", "Call cbm_results_totals with list = TRUE to view options")
  )

  if (units == "Mt"){
    queryTbl <- queryTbl[, lapply(.SD, function(x) x / 10^6), .SDcols = names(view_columns), by = "timestep"]
  }

  if (units == "t/ha"){

    tAreas <- cbm4_results_query(cbm4_results, paste(
      "SELECT timestep, SUM(area) AS area FROM age_indicators",
      if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
      "GROUP BY timestep"
    ))
    queryTbl[tAreas, area := area, on = "timestep"]
    data.table::setcolorder(queryTbl, c("timestep", "area"))

    queryTbl <- queryTbl[, lapply(.SD, function(x) x / area), .SDcols = names(view_columns), by = c("timestep", "area")]
  }

  data.table::setkey(queryTbl, timestep)
  return(queryTbl)
}



