
#' CBM4 results view
#'
#' Get a CBM4 SQLResultsProcessor view.
#'
#' @template cbm4_results
#' @param view_name character. `SQLResultsProcessor` view name.
#' @param select character. `SQLResultsProcessor` view column names.
#' The vector can have names to use as column aliases.
#' @param group_by character. Columns to group by. Pool columns will be aggregated with `SUM`.
#' @template timesteps
#' @param list logical. Return a table of available views.
#' @param ... arguments to \code{\link{SQLResultsProcessor}}
#'
#' @return `data.table`
#' @export
cbm4_results_view <- function(
    cbm4_results,
    view_name,
    select    = NULL,
    group_by  = NULL,
    timesteps = NULL,
    list      = FALSE,
    ...
){

  cbm4_results <- SQLResultsProcessor(cbm4_results, ...)

  if (list) return(data.table::as.data.table(cbm4_results$views)[, .(name, column_names)])

  if (!is.null(group_by)){
    if (is.null(select)) select <- .cbm4_results_view_columns(cbm4_results, view_name)
    if (is.null(names(select))) names(select) <- select
  }

  querySQL <- paste(
    "SELECT",
    if (!is.null(select)){
      c(shQuote(group_by),
        paste(
          sprintf(ifelse(is.null(group_by), "\"%s\"", "SUM(\"%s\")"), select),
          if (!is.null(names(select))) sprintf("AS \"%s\"", names(select))
        )) |> paste(collapse = ", ")
    }else "*",
    "FROM", view_name,
    if (!is.null(timesteps)) sprintf("WHERE timestep IN (%s)", paste(timesteps, collapse = ", ")),
    if (!is.null(group_by)) paste(
      "GROUP BY", paste(group_by, collapse = ", "),
      "ORDER BY", paste(group_by, collapse = ", ")
    )
  )

  queryTbl <- tryCatch(
    cbm4_results_query(cbm4_results, querySQL),
    error = function(e) stop(e$message, "\n", "Use list = TRUE to show a list of available views")
  )

  if (!is.null(group_by)) data.table::setkeyv(queryTbl, group_by)

  return(queryTbl)
}

.cbm4_results_view_columns <- function(cbm4_results, view_name){

  sim_cols       <- cbm4_results$get_columns("simulation")
  cohort_id_cols <- sim_cols[!grepl("^pools\\.", sim_cols)]
  cohort_id_cols <- unique(c(cohort_id_cols, gsub("^(inventory|classifiers|state)\\.", "", cohort_id_cols)))

  setdiff(
    cbm4_results$views[cbm4_results$views$name == view_name,]$column_names[[1]], c(
      cohort_id_cols, "raster_index",
      "disturbance_id", "disturbance_type", "disturbance_type_id", "disturbance_order", "disturbance_number"
    ))
}

