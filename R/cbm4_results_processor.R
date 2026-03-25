
#' CBM4 results processor
#'
#' @template cbm4_data
#' @return `SQLResultsProcessor`
#' @export
cbm4_results_processor <- function(cbm4_data){

  if (inherits(cbm4_data, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")){
    return(cbm4_data)
  }

  if (length(cbm4_data) == 0) stop("cbm4_data invalid")
  if (!file.exists(cbm4_data)) stop("cbm4_data not found: ", cbm4_data)

  reticulate::import(
    "cbm4.app.spatial.results.sql_results_processor"
  )$SQLResultsProcessor$for_simulation(cbm4_data)
}

