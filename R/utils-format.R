
# Default schema
cbm4_schema <- function(columns = NULL, schema = NULL){

  schema_default <- list(

    index             = "int32",
    chunk_index       = "int16",
    raster_index      = "int32",
    cohort_index      = "int16",
    timestep          = "int16",
    disturbance_order = "int8",

    cohort_proportion = "float32",

    age       = "float32",
    state.age = "float32",

    area           = "float32",
    inventory.area = "float32"

  )

  for (column in names(schema)) schema_default[[column]] <- schema[[column]]
  if (!is.null(columns)){
    if (is.data.frame(columns)) columns <- names(columns)
    schema_default <- schema_default[intersect(columns, names(schema_default))]
  }
  if (length(schema_default) > 0) return(schema_default)
}

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

