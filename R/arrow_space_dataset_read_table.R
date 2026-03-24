
arrow_space_dataset_read_table <- function(
    dataset_dir = NULL,
    dataset_name,
    table_name = NULL,
    dataset_path = file.path(dataset_dir, dataset_name),
    ...
){

  table_path <- file.path(dataset_path, paste0(dataset_name, if (!is.null(table_name)) paste0("-", table_name)))

  data.table::as.data.table(
    dplyr::collect(arrow::open_dataset(table_path), ...)
  )
}

