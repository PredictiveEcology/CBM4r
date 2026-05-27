
# Format classifier columns
set_table_classifiers <- function(table, classifiers){

  for (classifier in intersect(names(table), c(classifiers, paste0("classifiers.", classifiers)))){

    # Set NA to "?"
    if (anyNA(table[[classifier]])){
      if (!is.character(table[[classifier]])){
        table[, (classifier) := as.character(get(classifier))]
      }
      table[is.na(get(classifier)), (classifier) := "?"]
    }

    # Convert numeric to integer
    if (is.numeric(table[[classifier]])){

      if (any(table[[classifier]] %% 1 > 0)) stop(
        "classifiers can be integer but not numeric. See classifier ", shQuote(classifier))

      table[, (classifier) := as.integer(get(classifier))]
    }
  }
}

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

