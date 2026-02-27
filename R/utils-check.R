
check_table_columns_all <- function(tableName, table, colNames){

  if (is.null(table)) stop(tableName, " object is NULL")

  if (!all(colNames %in% names(table))) stop(
    tableName, " missing column(s): ",
    paste(shQuote(setdiff(colNames, names(table))), collapse = ", "))
}

check_table_columns_any <- function(tableName, table, colNames){

  if (is.null(table)) stop(tableName, " object is NULL")

  if (!any(colNames %in% names(table))) stop(
    tableName, " requires any of column(s): ",
    paste(shQuote(colNames), collapse = ", "))
}

