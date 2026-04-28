
#' CBM read geo
#'
#' Recreate a study area grid from a CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template dataset_name
#' @template dataset_path
#'
#' @return `SpatRaster`
#' @export
cbm4_read_geo <- function(
    cbm4_results,
    dataset_name = "simulation",
    dataset_path = file.path(cbm4_results, dataset_name)
){

  cbm4_results <- cbm4_results_processor(cbm4_results)

  geo_metadata <- reticulate::py_get_attr(cbm4_results, "_results_dataset")[[
    paste0(dataset_name, "_dataset")]]$get_geo_metadata()

  terra::rast(
    crs  = geo_metadata$projection,
    ncol = geo_metadata$ncols,
    nrow = geo_metadata$nrows,
    res  = geo_metadata$geo_transform_1,
    xmin = geo_metadata$geo_transform_0,
    xmax = geo_metadata$geo_transform_0 + (geo_metadata$geo_transform_1 * geo_metadata$ncols),
    ymin = geo_metadata$geo_transform_3 - (geo_metadata$geo_transform_1 * geo_metadata$nrows),
    ymax = geo_metadata$geo_transform_3
  )
}

