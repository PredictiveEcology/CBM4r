
#' CBM4 results grid
#'
#' Recreate a study area grid from a CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template dataset_name
#'
#' @return `SpatRaster`
#' @export
cbm4_results_grid <- function(
    cbm4_results,
    dataset_name = "simulation"
){

  if (is.character(cbm4_results)){
    geo_metadata <- arrow_space_dataset_read_table(
      cbm4_results,
      dataset_name = dataset_name,
      table_name   = "geo_metadata"
    )
  }else{
    geo_metadata <- reticulate::py_get_attr(cbm4_results, "_results_dataset")[[
      paste0(dataset_name, "_dataset")]]$get_geo_metadata()
  }

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


#' CBM4 results grid key
#'
#' Retrieve a study area grid key for a CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template dataset_name
#' @param coords logical. Return pixel coordinates.
#'
#' @return `data.table` with columns `pixel_index`, `chunk_index`, `raster_index`.
#' @export
cbm4_results_grid_key <- function(
    cbm4_results,
    dataset_name = "simulation",
    coords       = FALSE
){

  cbm4_grid <- cbm4_results_grid(cbm4_results, dataset_name = dataset_name)
  cbm4_grid_info <- list(
    ext = terra::ext(cbm4_grid),
    res = terra::res(cbm4_grid),
    crs = terra::crs(cbm4_grid)
  )

  if (is.character(cbm4_results)){

    grid_chunks <- arrow_space_dataset_read_table(
      cbm4_results,
      dataset_name = dataset_name,
      table_name   = "chunks"
    )

    raster_index <- arrow::open_dataset(file.path(cbm4_results, dataset_name, paste0(dataset_name, "-raster_index")))
    if ("timestep" %in% names(raster_index)) raster_index <- dplyr::filter(raster_index, timestep == 0)
    grid_key <- raster_index |>
      dplyr::select(chunk_index, raster_index) |>
      dplyr::collect() |>
      unique() |>
      data.table::as.data.table()

  }else{

    grid_chunks <- reticulate::py_get_attr(cbm4_results, "_results_dataset")[[
      paste0(dataset_name, "_dataset")]]$chunks
    grid_chunks <- data.table::rbindlist(lapply(1:length(grid_chunks), function(i){
      data.table::data.table(
        chunk_index = i - 1,
        x_off       = grid_chunks[[1]]$x_off,
        x_size      = grid_chunks[[1]]$x_size,
        y_off       = grid_chunks[[1]]$y_off,
        y_size      = grid_chunks[[1]]$y_size
      )
    }))

    grid_key <- cbm4_results_query(cbm4_results, c(
      "SELECT chunk_index, raster_index FROM raster_index WHERE timestep = 0"
    ))
  }

  grid_key <- cbind(grid_key, data.table::rbindlist(lapply(unique(grid_key$chunk_index), function(ch){

    chunk <- grid_chunks[ch + 1,]

    chunk_ext <- list(
      xmin  = cbm4_grid_info$ext$xmin + (cbm4_grid_info$res[[1]] * chunk$x_off),
      ymin  = cbm4_grid_info$ext$ymin + (cbm4_grid_info$res[[2]] * chunk$y_off)
    )
    chunk_ext$xmax <- chunk_ext$xmin + (cbm4_grid_info$res[[1]] * chunk$x_size)
    chunk_ext$ymax <- chunk_ext$ymin + (cbm4_grid_info$res[[2]] * chunk$y_size)

    chunk_rast <- terra::rast(
      xmin  = chunk_ext$xmin,
      ymin  = chunk_ext$ymin,
      xmax  = chunk_ext$xmax,
      ymax  = chunk_ext$ymax,
      ncols = chunk$x_size,
      nrows = chunk$y_size,
      crs   = cbm4_grid_info$crs
    )

    data.table::as.data.table(
      terra::xyFromCell(chunk_rast, grid_key[chunk_index == ch]$raster_index + 1)
    )
  })))

  grid_key <- data.table::as.data.table(terra::crds(cbm4_grid))[
    , pixel_index := 1:.N][grid_key, on = c("x", "y")]
  data.table::setkey(grid_key, pixel_index)

  if (coords){
    grid_key[, .(pixel_index, chunk_index, raster_index, x, y)]
  }else{
    grid_key[, .(pixel_index, chunk_index, raster_index)]
  }
}



