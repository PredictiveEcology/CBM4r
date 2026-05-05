
# Set table column defaults by reference
# Defaults are set by arguments named as the column name prefixed with 'def_'
set_table_defaults <- function(table, envir = parent.frame()){
  for (defArg in names(envir)[grepl("^def\\_", names(envir))]){
    defCol <- sub("^def\\_", "", defArg)
    defVal <- get(defArg, envir = envir)
    if (!is.null(defVal)){
      if (!defCol %in% names(table)){
        if (is.character(defVal)) defVal <- factor(defVal)
        i <- NULL
      }else{
        i <- which(is.na(table[[defCol]]))
      }
      data.table::set(table, i = i, j = defCol, value = defVal)
    }
  }
  return(table)
}

