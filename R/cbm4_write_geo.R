
#' CBM4 write geo
#'
#' Initiate a CBM4 spatial parquet dataset with study area geographic metadata.
#' The dataset can contain an additional "pixels" metadata table with
#' mapping grid pixel locations to their parallel processing partition chunks.
#'
#' @template cbm4_data
#' @template dataset_name
#' @template dataset_path
#' @param write_pixels logical. Write pixel table to file. If `TRUE`, grid_meta is required.
#' @inheritParams set_grid_meta
#' @inheritParams arrow_space_dataset_write_geo
#' @param ... arguments to \code{\link{arrow_space_dataset_write_geo}} or \code{\link{set_grid_meta}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_geo <- function(
    cbm4_data = NULL,
    dataset_name,
    grid_rast       = NULL,
    grid_meta       = NULL,
    write_pixels    = TRUE,
    dataset_path    = file.path(cbm4_data, dataset_name),
    ...
){

  if (is.null(grid_rast)) stop("grid_rast is NULL")
  if (!inherits(grid_rast, "SpatRaster")) grid_rast <- tryCatch(
    terra::rast(grid_rast), error = function(e) stop(
      "failed to read grid_rast as SpatRaster: ", e$message))

  if (write_pixels){

    set_grid_meta(grid_meta, ...)

    if (!"area" %in% names(grid_meta)){
      grid_meta[, area := prod(terra::res(chunks$rast) * terra::linearUnits(chunks$rast))]
      data.table::setcolorder(grid_meta, c("pixel_index", "chunk_index", "raster_index", "area"))
    }
  }

  # Create new arrow_space dataset
  arrow_space_dataset_write_geo(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    grid_rast    = grid_rast,
    grid_chunks  = length(unique(grid_meta$chunk_index)),
    ...
  )

  # Write pixel table
  if (write_pixels){
    arrow_space_dataset_write_table(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      table_name    = "table-pixels",
      table_data    = grid_meta,
      schema = list(
        pixel_index       = arrow::int32(),
        chunk_index       = arrow::int8(),
        raster_index      = arrow::int32(),
        spatial_unit      = arrow::int8(),
        admin_boundary_id = arrow::int8(),
        admin_boundary    = arrow::dictionary(arrow::int8()),
        eco_boundary_id   = arrow::int8(),
        eco_boundary      = arrow::dictionary(arrow::int8()),
        afforestation_pre_type     = arrow::dictionary(arrow::int8()),
        historic_disturbance_type  = arrow::dictionary(arrow::int8()),
        last_pass_disturbance_type = arrow::dictionary(arrow::int8())
      )
    )
  }

  return(invisible())
}


