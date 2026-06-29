
#' CBM defaults: read table
#'
#' Read a table from a CBM defaults SQLite database.
#'
#' @param tableName character. Table name.
#' Use `cbm_defaults_listTables` to select a table.
#' @template cbm_defaults_db
#' @param localeID integer. Locale ID.
#' @param ... arguments to `RSQLite::dbReadTable`
#'
#' @export
cbm_defaults_readTable <- function(
    tableName,
    cbm_defaults_db = getOption("CBM4r.db.path"),
    localeID        = getOption("CBM4r.db.localeID"),
    ...
){

  if (length(cbm_defaults_db) == 0)  stop("cbm_defaults_db path invalid")
  if (!file.exists(cbm_defaults_db)) stop("cbm_defaults_db not found: ", cbm_defaults_db)

  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db, flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(cbmDBcon))

  tbl <- data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, tableName, ...))
  if ("locale_id" %in% names(tbl)){
    tbl <- tbl[locale_id == localeID,]
    tbl <- tbl[, locale_id := NULL]
  }
  return(tbl)
}


#' CBM defaults: list tables
#'
#' List tables in a CBM defaults SQLite database.
#'
#' @template cbm_defaults_db
#' @param ... arguments to `RSQLite::dbListTables`
#'
#' @export
cbm_defaults_listTables <- function(cbm_defaults_db = getOption("CBM4r.db.path"), ...){

  if (length(cbm_defaults_db) == 0)  stop("cbm_defaults_db path invalid")
  if (!file.exists(cbm_defaults_db)) stop("cbm_defaults_db not found: ", cbm_defaults_db)

  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db, flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(cbmDBcon))

  RSQLite::dbListTables(cbmDBcon, ...)

}


