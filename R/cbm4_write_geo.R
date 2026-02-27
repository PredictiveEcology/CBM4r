
#' CBM write geo
#'
#' Initiate a CBM4 spatial parquet dataset with study area geographic metadata.
#' This can be done by providing a raster grid or the name of a dataset to use as a template.
#' The dataset will contain an additional "pixels" table
#' mapping grid pixel locations to their parallel processing partition chunks.
#' This table has columns `pixel_index`, `chunk_index`, `raster_index`, `x`, `y`, `area`.
#'
#' @template cbm4_data
#' @template dataset_name
#' @inheritParams arrow_space_dataset_chunks
#' @template template_name
#' @template template_path
#' @template dataset_path
#' @param ... arguments to \code{\link{arrow_space_dataset_write_geo}}
#' or \code{\link{arrow_space_dataset_copy_geo}}
#' @param write_pixels logical. Write pixel table to file.
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_geo <- function(
    cbm4_data = NULL,
    dataset_name,
    template_name = NULL,
    grid_rast     = NULL,
    grid_chunks   = 1,
    write_pixels  = TRUE,
    dataset_path  = file.path(cbm4_data, dataset_name),
    template_path = file.path(cbm4_data, template_name),
    ...
){

  if (is.null(template_name)){

    # Set metadata from inputs
    chunks <- arrow_space_dataset_chunks(grid_rast, grid_chunks)
    chunks$geo_metadata <- arrow_space_dataset_geo_metadata(grid_rast)

    # Create new arrow_space dataset
    arrow_space_dataset_write_geo(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      geo_metadata = chunks$geo_metadata,
      chunks       = chunks$bounds,
      ...
    )

    # Write pixel table
    if (write_pixels){

      pixelDT <- data.table::as.data.table(terra::values(chunks$rast))
      pixelDT[, pixel_index := 1:terra::ncell(chunks$rast)]
      data.table::setcolorder(pixelDT, c("pixel_index", "chunk_index", "raster_index"))
      pixelDT[, x    := terra::xFromCell(chunks$rast, pixelDT$pixel_index)]
      pixelDT[, y    := terra::yFromCell(chunks$rast, pixelDT$pixel_index)]
      pixelDT[, area := prod(terra::res(chunks$rast) * terra::linearUnits(chunks$rast))]

      arrow_space_dataset_write_table(
        dataset_name  = dataset_name,
        dataset_path  = dataset_path,
        table_name    = "table-pixels",
        table_data    = pixelDT
      )
    }

  }else{

    # Create new arrow_space dataset using another as a template
    arrow_space_dataset_copy_geo(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      ...
    )

    # Copy pixel table
    if (write_pixels){
      arrow_space_dataset_copy_table(
        table_name    = "table-pixels",
        dataset_name  = dataset_name,
        dataset_path  = dataset_path,
        template_name = template_name,
        template_path = template_path,
        overwrite     = TRUE,
        skip_missing  = TRUE
      )
    }
  }

  return(invisible())
}

#' arrow_space_dataset_chunks
#' @param grid_rast terra `SpatRaster`. Grid defining the study area.
#' @param grid_chunks integer.
#' The number of chunks to partition the grid into for parallel processing.
arrow_space_dataset_chunks <- function(grid_rast, grid_chunks = 1){

  if (is.null(grid_rast)) stop("grid_rast is NULL")
  if (grid_chunks != 1) stop(">1 chunks not yet supported")

  if (grid_chunks == 1){

    gridChunks <- terra::rast(grid_rast, vals = 0L, name = "chunk_index")
    gridChunks[["raster_index"]] <- 1L:terra::ncell(grid_rast) - 1L

    gridBounds <- data.table::data.table(
      chunk_index = 0,
      x_off       = 0,
      y_off       = 0,
      x_size      = ncol(grid_rast),
      y_size      = nrow(grid_rast)
    )
  }

  list(
    rast   = gridChunks,
    bounds = gridBounds
  )
}

arrow_space_dataset_geo_metadata <- function(grid_rast){

  if (is.null(grid_rast)) stop("grid_rast is NULL")

  list(
    nrows           = terra::nrow(grid_rast),
    ncols           = terra::ncol(grid_rast),
    projection      = terra::crs(grid_rast),
    geo_transform_0 = terra::xmin(grid_rast),
    geo_transform_1 = terra::res(grid_rast)[[1]],
    geo_transform_2 = 0,
    geo_transform_3 = terra::ymax(grid_rast),
    geo_transform_4 = 0,
    geo_transform_5 = -terra::res(grid_rast)[[2]]
  )
}


