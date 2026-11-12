
#' arrow_space_dataset_write_geo
#'
#' @param dataset_dir character. Path to `arrow_space` dataset directory.
#' @param dataset_name character. name of `arrow_space` dataset directory.
#' @param dataset_path character. Path to `arrow_space` dataset.
#' Defaults to `file.path(dataset_dir, dataset_name)`
#' @template grid_rast
#' @template chunk_size
#' @param partitions character. TODO
#' @param tags character. TODO
#' @param ... unused
#' @keywords internal
arrow_space_dataset_write_geo <- function(
    dataset_dir  = NULL,
    dataset_name,
    grid_rast,
    chunk_size   = NULL,
    partitions   = NULL,
    tags         = NULL,
    dataset_path = file.path(dataset_dir, dataset_name),
    ...
){

  if (length(dataset_path) == 0) stop("dataset_path is invalid")

  arrow_space <- reticulate::import("arrow_space")
  if (!is.null(tags)) pd <- reticulate::import("pandas")

  geo_metadata <- arrow_space_dataset_geo_metadata(grid_rast)
  geo_bounds   <- arrow_space_dataset_bounds(grid_rast, chunk_size)

  arrow_space$dataset_common$create_empty(

    out_dataset_name        = dataset_name,
    out_storage_type        = "local_storage",
    out_storage_path_or_uri = dataset_path,

    partitions = if (length(partitions) > 0) partitions else reticulate::dict(),

    chunks = apply(geo_bounds, 1, function(ch){
      arrow_space$geospatial$raster_bound$RasterBound(
        ch[["x_off"]], ch[["y_off"]], ch[["x_size"]], ch[["y_size"]])
    }),

    geo_metadata = arrow_space$dataset_metadata$GeoMetadata(
      nrows           = geo_metadata$nrows,
      ncols           = geo_metadata$ncols,
      projection      = geo_metadata$projection,
      geo_transform_0 = geo_metadata$geo_transform_0,
      geo_transform_1 = geo_metadata$geo_transform_1,
      geo_transform_2 = geo_metadata$geo_transform_2,
      geo_transform_3 = geo_metadata$geo_transform_3,
      geo_transform_4 = geo_metadata$geo_transform_4,
      geo_transform_5 = geo_metadata$geo_transform_5
    ),

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

arrow_space_dataset_bounds <- function(grid_rast, chunk_size = NULL){

  if (is.null(grid_rast)) stop("grid_rast is NULL")
  if (!inherits(grid_rast, "SpatRaster")) grid_rast <- tryCatch(
    terra::rast(grid_rast), error = function(e) stop(
      "failed to read grid_rast as SpatRaster: ", e$message))

  if (is.null(chunk_size) || is.na(chunk_size)){

    bounds <- data.table::data.table(
      chunk_index = 0,
      x_off       = 0,
      y_off       = 0,
      x_size      = terra::ncol(grid_rast),
      y_size      = terra::nrow(grid_rast)
    )

  }else{

    bounds <- suppressWarnings(terra::getTileExtents(grid_rast, chunk_size)) |>
      data.table::as.data.table()
    bounds[, chunk_index := .I - 1]
    bounds[, x_off  := xmin - terra::xmin(grid_rast)]
    bounds[, y_off  := ymin - terra::ymin(grid_rast)]
    bounds[, x_size := xmax - xmin]
    bounds[, y_size := ymax - ymin]
    bounds[, .(chunk_index, x_off, y_off, x_size, y_size)]
    bounds <- bounds[, .(chunk_index, x_off, y_off, x_size, y_size)]
  }
  bounds[, (names(bounds)) := lapply(.SD, as.integer)]
  bounds
}

arrow_space_dataset_geo_metadata <- function(grid_rast){

  if (is.null(grid_rast)) stop("grid_rast is NULL")
  if (!inherits(grid_rast, "SpatRaster")) grid_rast <- tryCatch(
    terra::rast(grid_rast), error = function(e) stop(
      "failed to read grid_rast as SpatRaster: ", e$message))

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



