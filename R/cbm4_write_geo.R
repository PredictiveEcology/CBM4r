
#' CBM4 write geo
#'
#' Initiate a CBM4 spatial parquet dataset with study area geographic metadata.
#'
#' @template cbm4_data
#' @template dataset_name
#' @template grid_meta
#' @template grid_rast
#' @template dataset_path
#' @param ... arguments to \code{\link{arrow_space_dataset_write_geo}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_geo <- function(
    cbm4_data = NULL,
    dataset_name,
    grid_meta,
    grid_rast,
    dataset_path = file.path(cbm4_data, dataset_name),
    ...
){

  arrow_space_dataset_write_geo(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    grid_rast    = grid_rast,
    grid_chunks  = sum(!is.na(unique(grid_meta$chunk_index))),
    ...
  )
}


