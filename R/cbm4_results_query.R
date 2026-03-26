
#' CBM4 results query
#'
#' Query CBM4 results with SQL.
#'
#' @template cbm4_results
#' @param query character. SQL query
#'
#' @return `data.table`
#' @export
cbm4_results_query <- function(cbm4_results, query = NULL){

  tmpDir <- gsub("\\\\", "/", tempfile("duckdb_temp_storage_"))
  on.exit(unlink(tmpDir, recursive = TRUE))

  data.table::as.data.table(
    cbm4_results_processor(cbm4_results)$query(paste(c(
      sprintf("SET temp_directory = '%s';", tmpDir),
      query
    ), collapse = " "))
  )
}


#' CBM4 results query preset
#'
#' Query results from CBM4r preset SQL query file.
#'
#' @param queryName character. Preset SQL query file name
#' @param where character. SQL WHERE clause
#' @inherit cbm4_results_query params return
#' @keywords internal
.cbm4_results_query_preset <- function(cbm4_results, queryName, where = NULL){

  queryPath <- file.path("SQL", paste0(queryName, ".sql"))
  queryPathFull <- system.file(queryPath, package = "CBM4r")
  if (queryPathFull == "") stop("Query file not found: ", queryPath)

  query <- readLines(queryPathFull)

  if (sum(query == "-- WHERE") != 1) stop("WHERE clause not supported for: ", queryPath)
  if (!is.null(where)) query[query == "-- WHERE"] <- paste("WHERE", where)

  query <- query[query != ""]
  query <- query[!grepl("^--", query)]

  cbm4_results_query(cbm4_results, query)
}

