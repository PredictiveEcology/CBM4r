
arrow_space_dataset_read_table <- function(
    dataset_dir = NULL,
    dataset_name,
    table_name = NULL,
    col_select = NULL,
    dataset_path = file.path(dataset_dir, dataset_name),
    ...
){

  table_path <- file.path(dataset_path, paste0(dataset_name, if (!is.null(table_name)) paste0("-", table_name)))

  table_dataset <- arrow::open_dataset(table_path, ...)
  if (!is.null(col_select)) table_dataset <- dplyr::select(table_dataset, col_select)
  data.table::as.data.table(dplyr::collect(table_dataset))
}

