
#' CBM4 copy dataset
#'
#' Copy a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#' @param ... arguments to \code{\link{arrow_space_dataset_copy}}
#'
#' @return `NULL`. Dataset will be copied.
#' @export
cbm4_copy_dataset <- function(
    cbm4_data = NULL,
    dataset_name,
    template_name = dataset_name,
    dataset_path  = file.path(cbm4_data, dataset_name),
    template_path = file.path(cbm4_data, template_name),
    ...
){

  arrow_space_dataset_copy(
    dataset_dir   = cbm4_data,
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    template_name = template_name,
    template_path = template_path,
    ...
  )
}


