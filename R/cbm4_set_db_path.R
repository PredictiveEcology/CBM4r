
#' CBM4 set database path
#'
#' Set custom parameters by using a modified version of the CBM defaults RSQLite database.
#' This path can instead be passed directly to functions that use it.
#'
#' @template cbm_defaults_db
#'
#' @export
cbm4_set_db_path <- function(cbm_defaults_db){

  if (length(cbm_defaults_db) == 0)  stop("Database path invalid")
  if (!file.exists(cbm_defaults_db)) stop("Database not found: ", cbm_defaults_db)

  options("CBM4r.db.path" = cbm_defaults_db)

}

