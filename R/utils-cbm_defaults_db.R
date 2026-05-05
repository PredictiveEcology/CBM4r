
# Read a CBM defaults SQLite database table
cbm_defaults_db_table <- function(tableName,
                                  cbm_defaults_db = getOption("CBM4r.db.path"),
                                  localeID        = getOption("CBM4r.db.localeID")){

  if (length(cbm_defaults_db) == 0)  stop("cbm_defaults_db path invalid")
  if (!file.exists(cbm_defaults_db)) stop("cbm_defaults_db not found: ", cbm_defaults_db)

  cbmDBcon <- RSQLite::dbConnect(RSQLite::dbDriver("SQLite"), cbm_defaults_db, flags = RSQLite::SQLITE_RO)
  on.exit(RSQLite::dbDisconnect(cbmDBcon))

  tbl <- data.table::as.data.table(RSQLite::dbReadTable(cbmDBcon, tableName))
  if ("locale_id" %in% names(tbl)){
    tbl <- tbl[locale_id == localeID,]
    tbl <- tbl[, locale_id := NULL]
  }
  return(tbl)
}

