
#' CBM4 results query
#'
#' Query CBM4 results with SQL.
#'
#' @template cbm4_data
#' @param query character. SQL query
#'
#' @export
cbm4_results_query <- function(cbm4_data, query){

  results_processor <- reticulate::import(
    "cbm4.app.spatial.results.sql_results_processor"
  )$SQLResultsProcessor$for_simulation(cbm4_data)

  data.table::as.data.table(
    results_processor$query(paste(query, collapse = " "))
  )
}


# Query results from preset CBM4r SQL file
cbm4_results_query_preset <- function(cbm4_data, queryName, where = NULL){

  queryPath <- file.path("results", paste0(queryName, ".sql"))
  queryPathFull <- system.file(queryPath, package = "CBM4r")
  if (queryPathFull == "") stop("Query file not found: ", queryPath)

  query <- readLines(queryPathFull)

  if (sum(query == "-- WHERE") != 1) stop("WHERE clause not supported for: ", queryPath)
  if (!is.null(where)) query[query == "-- WHERE"] <- paste("WHERE", where)

  query <- query[query != ""]
  query <- query[!grepl("^--", query)]

  cbm4_results_query(cbm4_data, query)
}

