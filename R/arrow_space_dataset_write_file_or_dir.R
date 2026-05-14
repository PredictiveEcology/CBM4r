
arrow_space_dataset_write_file_or_dir <- function(
    dataset_dir = NULL,
    dataset_name,
    file_name,
    file_path,
    dataset_path = file.path(dataset_dir, dataset_name)
){

  if (length(dataset_path) == 0) stop("dataset_path is invalid")

  arrow_space <- reticulate::import("arrow_space")

  arrow_space$raster_indexed_dataset$RasterIndexedDataset(
    dataset_name        = dataset_name,
    storage_type        = "local_storage",
    storage_path_or_uri = dataset_path
  )$write_file_or_dir(file_name, file_path)
}

