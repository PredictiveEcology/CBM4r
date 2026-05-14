
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

