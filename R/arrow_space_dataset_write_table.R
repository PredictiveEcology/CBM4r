
arrow_space_dataset_write_table <- function(
    dataset_dir = NULL,
    dataset_name,
    table_data,
    table_name = NULL,
    dataset_path = file.path(dataset_dir, dataset_name),
    existing_data_behavior = "delete_matching",
    ...
){

  table_path <- file.path(dataset_path, paste0(dataset_name, ifelse(
    is.null(table_name) || table_name == "", "", paste0("-", table_name))))

  if (nrow(table_data) > 0){
    arrow::write_dataset(table_data, table_path, existing_data_behavior = existing_data_behavior, ...)
  }else{
    dir.create(table_path, recursive = TRUE)
  }
}


