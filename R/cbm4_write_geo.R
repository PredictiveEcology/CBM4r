
#' CBM4 write geo
#'
#' Initiate a CBM4 spatial parquet dataset with study area geographic metadata.
#' The dataset can contain an additional "pixels" metadata table with
#' mapping grid pixel locations to their parallel processing partition chunks.
#' This table has columns `pixel_index`, `chunk_index`, `raster_index`, `area`,
#' `admin_boundary`, `eco_boundary`, `spatial_unit`,
#' `afforestation_pre_type`, `historic_disturbance_type`, `last_pass_disturbance_type`.
#'
#' @template cbm4_data
#' @template dataset_name
#' @template dataset_path
#' @param write_pixels logical. Write pixel table to file.
#' @inheritParams arrow_space_dataset_chunks
#' @param grid_meta data.table. Grid metadata. TODO
#' @template cbm_defaults_db
#' @param def_afforestation_pre_type character. Land use before forestation.
#' Defined in CBM defaults database tables 'afforestation_pre_type'
#' @param def_historic_disturbance_type character. Historic disturbance type.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_last_pass_disturbance_type character. Last pass disturbance.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param ... arguments to \code{\link{arrow_space_dataset_write_geo}}
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_geo <- function(
    cbm4_data = NULL,
    dataset_name,
    write_pixels    = TRUE,
    grid_rast       = NULL,
    grid_chunks     = 1,
    grid_meta       = NULL,
    cbm_defaults_db = NULL,
    dataset_path    = file.path(cbm4_data, dataset_name),
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    ...
){

  if (write_pixels){
    if (is.null(grid_meta)) stop("grid_meta required")
    check_table_columns_all("grid_meta", grid_meta, "pixel_index")
    check_table_columns_any("grid_meta", grid_meta, c("admin_boundary_id", "admin_boundary"))
    check_table_columns_any("grid_meta", grid_meta, c("eco_boundary_id",   "eco_boundary"))
  }

  # Set geo metadata from inputs
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
    pixelDT <- merge(pixelDT, grid_meta, by = "pixel_index")

    if (!"area" %in% names(pixelDT)){
      pixelDT[, area := prod(terra::res(chunks$rast) * terra::linearUnits(chunks$rast))]
    }

    if (any(!c("admin_boundary", "eco_boundary", "spatial_unit") %in% names(pixelDT))){

      admin_boundary_tr <- cbmdbReadTable(cbm_defaults_db, "admin_boundary_tr")
      eco_boundary_tr   <- cbmdbReadTable(cbm_defaults_db, "eco_boundary_tr")

      if (!"admin_boundary_id" %in% names(pixelDT)){
        pixelDT[, admin_boundary_id := admin_boundary_tr$admin_boundary_id[
          match(admin_boundary, admin_boundary_tr$name)]]
      }
      if (!"admin_boundary" %in% names(pixelDT)){
        pixelDT[, admin_boundary := factor(
          admin_boundary_tr$name[match(admin_boundary_id, admin_boundary_tr$id)],
          levels = admin_boundary_tr$name)]
      }

      if (!"eco_boundary_id" %in% names(pixelDT)){
        pixelDT[, eco_boundary_id := admin_boundary_tr$eco_boundary_id[
          match(eco_boundary, eco_boundary_tr$name)]]
      }
      if (!"eco_boundary" %in% names(pixelDT)){
        pixelDT[, eco_boundary := factor(
          eco_boundary_tr$name[match(eco_boundary_id, eco_boundary_tr$id)],
          levels = eco_boundary_tr$name)]
      }

      if (!"spatial_unit" %in% names(pixelDT)){

        spatial_unit <- cbmdbReadTable(cbm_defaults_db, "spatial_unit")[
          , .(spatial_unit = id, admin_boundary_id, eco_boundary_id)]

        pixelDT <- merge(
          pixelDT, spatial_unit,
          by = c("admin_boundary_id", "eco_boundary_id"), all.x = TRUE)

        # Check spatial unit IDs
        if (any(is.na(pixelDT$spatial_unit_id))){
          noMatch <- unique(pixelDT[is.na(spatial_unit_id), .(admin_boundary, eco_boundary_id)])
          data.table::setkey(noMatch, admin_boundary, eco_boundary_id)
          if (nrow(noMatch) > 0) stop(
            "spatial_unit_id not found for: ",
            paste(paste(noMatch$admin_boundary, "ecozone", noMatch$eco_boundary_id), collapse = "; "))
        }
      }
    }

    # Set defaults
    for (defArg in names(environment())[grepl("^def\\_", names(environment()))]){
      defCol <- sub("^def\\_", "", defArg)
      if (!defCol %in% names(pixelDT)){
        pixelDT[, eval(defCol) := factor(get(defArg))]
      }else{
        pixelDT[is.na(eval(defCol)), eval(defCol) := factor(get(defArg))]
      }
    }

    data.table::setkey(pixelDT, "pixel_index")
    data.table::setcolorder(pixelDT, intersect(c(
      "pixel_index", "chunk_index", "raster_index",
      "area",
      "spatial_unit", "admin_boundary_id", "admin_boundary", "eco_boundary_id", "eco_boundary",
      "land_class", "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
    ), names(pixelDT)))

    # Strings as factor
    for (col in names(pixelDT)[sapply(pixelDT, is.character)]) pixelDT[[col]] <- factor(pixelDT[[col]])

    arrow_space_dataset_write_table(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      table_name    = "table-pixels",
      table_data    = pixelDT,
      no_factors    = FALSE
    )
  }

  return(invisible())
}

#' arrow_space_dataset_chunks
#' @param grid_rast terra `SpatRaster`. Grid defining the study area.
#' @param grid_chunks integer.
#' The number of chunks to partition the grid into for parallel processing.
arrow_space_dataset_chunks <- function(grid_rast, grid_chunks = 1){

  if (is.null(grid_rast)) stop("grid_rast is NULL")
  if (!inherits(grid_rast, "SpatRaster")) grid_rast <- tryCatch(
    terra::rast(grid_rast), error = function(e) stop(
      "failed to read grid_rast as SpatRaster: ", e$message))

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


