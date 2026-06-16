
arrow_space_dataset_write_table <- function(
    dataset_dir = NULL,
    dataset_name,
    table_data,
    table_name   = NULL,
    schema       = NULL,
    no_factors   = TRUE,
    existing_data_behavior = "delete_matching",
    dataset_path = file.path(dataset_dir, dataset_name),
    ...
){

  table_path <- file.path(dataset_path, paste0(dataset_name, ifelse(
    is.null(table_name) || table_name == "", "", paste0("-", table_name))))

  if (nrow(table_data) > 0){

    if (no_factors){
      factCols <- names(table_data)[sapply(table_data, is.factor)]
      schema <- as.list(schema)
      for (col in setdiff(factCols, names(schema))) schema[[col]] <- arrow::string()
    }

    if (length(schema) > 0){

      table_data   <- arrow::arrow_table(table_data)
      table_schema <- arrow::schema(table_data)

      for (col in names(schema)){
        if (is.character(schema[[col]])){
          table_schema[[col]] <- do.call(get(schema[[col]], asNamespace("arrow")), list())
        }else{
          table_schema[[col]] <- schema[[col]]
        }
      }

      if (!identical(
        lapply(arrow::schema(table_data), function(x) x$type$ToString()),
        lapply(table_schema, function(x) x$type$ToString)
      )) table_data <- table_data$cast(table_schema)
    }

    arrow::write_dataset(table_data, table_path, existing_data_behavior = existing_data_behavior, ...)

  }else{
    dir.create(table_path, recursive = TRUE, showWarnings = FALSE)
  }
}


