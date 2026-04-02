
arrow_space_dataset_write_table <- function(
    dataset_dir = NULL,
    dataset_name,
    table_data,
    table_name = NULL,
    dataset_path = file.path(dataset_dir, dataset_name),
    existing_data_behavior = "delete_matching",
    no_factors = TRUE,
    ...
){

  table_path <- file.path(dataset_path, paste0(dataset_name, ifelse(
    is.null(table_name) || table_name == "", "", paste0("-", table_name))))

  if (nrow(table_data) > 0){

    # Convert factor columns to string
    if (no_factors){
      factCols <- names(table_data)[sapply(table_data, is.factor)]
      if (length(factCols) > 0) table_data[, (factCols) := lapply(.SD, as.character), .SDcols = factCols]
    }

    arrow::write_dataset(table_data, table_path, existing_data_behavior = existing_data_behavior, ...)

  }else{
    dir.create(table_path, recursive = TRUE)
  }
}


