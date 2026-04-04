
#' arrow_space_dataset_copy_geo
#'
#' @param dataset_dir character. Path to `arrow_space` dataset directory.
#' @param dataset_name character. name of `arrow_space` dataset directory.
#' @param dataset_path character. Path to `arrow_space` dataset.
#' Defaults to `file.path(dataset_dir, dataset_name)`
#' @param template_name character. name of `arrow_space` dataset directory to copy geo metadata from.
#' @param template_path character. Path to `arrow_space` dataset to copy geo metadata from.
#' Defaults to `file.path(dataset_dir, template_name)`
#' @param copy_raster_index_data logical. TODO
#' @param partitions character. TODO
#' @param tags character. TODO
#' @param ... unused
arrow_space_dataset_copy_geo <- function(
    dataset_dir = NULL,
    dataset_name,
    template_name,
    copy_raster_index_data = FALSE,
    partitions = NULL,
    tags       = NULL,
    dataset_path  = file.path(dataset_dir, dataset_name),
    template_path = file.path(dataset_dir, template_name),
    ...
){

  if (length(dataset_path) == 0)   stop("dataset_path is invalid")
  if (length(template_path) == 0)  stop("template_path is invalid")
  if (!file.exists(template_path)) stop(
    "template_path not found: ", template_path,
    "\nUse cbm4_write_geo to initiate a new dataset.")

  arrow_space <- reticulate::import("arrow_space")
  if (!is.null(tags)) pd <- reticulate::import("pandas")

  arrow_space$raster_indexed_dataset$RasterIndexedDataset$create_new(

    arrow_space$raster_indexed_dataset$RasterIndexedDataset(
      dataset_name        = template_name,
      storage_type        = "local_storage",
      storage_path_or_uri = template_path
    ),

    out_dataset_name        = dataset_name,
    out_storage_type        = "local_storage",
    out_storage_path_or_uri = dataset_path,
    copy_raster_index_data  = copy_raster_index_data,

    partitions = if (length(partitions) > 0) partitions else reticulate::dict(),

    tags = if (!is.null(tags)) pd$DataFrame(
      columns = c("layer_name", "tag"),
      data = reticulate::dict(
        layer_name = as.list(do.call(c, unname(tags))),
        tag        = as.list(do.call(c, lapply(1:length(tags), function(i) rep(names(tags)[[i]], length(tags[[i]])))))
      )
    )
  )

  # Write empty partitions
  if (length(partitions) == 0){
    dir.create(file.path(dataset_path, paste0(dataset_name, "-partitions")))
  }

  return(invisible())
}


