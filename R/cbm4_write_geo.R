
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
#' @inheritParams cbm4_format_grid_meta
#' @inheritParams arrow_space_dataset_write_geo
#' @param ... arguments to \code{\link{arrow_space_dataset_write_geo}} or \code{\link{cbm4_format_grid_meta}}
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

    grid_meta <- cbm4_format_grid_meta(grid_meta, ...)

    if (!"area" %in% names(grid_meta)){
      grid_meta[, area := prod(terra::res(chunks$rast) * terra::linearUnits(chunks$rast))]
      data.table::setcolorder(grid_meta, c("pixel_index", "chunk_index", "raster_index"))
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
      no_factors    = FALSE
    )
  }

  return(invisible())
}


#' cbm4_format_grid_meta
#'
#' @param grid_meta data.table. Grid metadata.
#' Required columns: `pixel_index`,
#' `admin_boundary` or `admin_boundary_id`.
#' `eco_boundary` or `eco_boundary_id`,
#' `spatial_unit`.
#' Optional columns: `chunk_index`, `raster_index`,
#' `afforestation_pre_type`, `historic_disturbance_type`, `last_pass_disturbance_type`.
#' @template cbm_defaults_db
#' @param def_afforestation_pre_type character. Land use before forestation.
#' Defined in CBM defaults database tables 'afforestation_pre_type'
#' @param def_historic_disturbance_type character. Historic disturbance type.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_last_pass_disturbance_type character. Last pass disturbance.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param ... unused
cbm4_format_grid_meta <- function(
    grid_meta,
    cbm_defaults_db = NULL,
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    ...
){

  if (is.null(grid_meta)) stop("grid_meta required")

  check_table_columns_all("grid_meta", grid_meta, "pixel_index")
  check_table_columns_any("grid_meta", grid_meta, c("admin_boundary_id", "admin_boundary"))
  check_table_columns_any("grid_meta", grid_meta, c("eco_boundary_id",   "eco_boundary"))

  if (!"chunk_index"  %in% names(grid_meta)) grid_meta[, chunk_index  := 0]
  if (!"raster_index" %in% names(grid_meta)) grid_meta[, raster_index := pixel_index - 1]

  if (any(!c("admin_boundary", "eco_boundary", "spatial_unit") %in% names(grid_meta))){

    admin_boundary_tr <- cbmdbReadTable(cbm_defaults_db, "admin_boundary_tr")
    eco_boundary_tr   <- cbmdbReadTable(cbm_defaults_db, "eco_boundary_tr")

    if (!"admin_boundary" %in% names(grid_meta)){
      grid_meta[, admin_boundary := factor(
        admin_boundary_tr$name[match(admin_boundary_id, admin_boundary_tr$id)],
        levels = admin_boundary_tr$name)]
    }
    if (!"eco_boundary" %in% names(grid_meta)){
      grid_meta[, eco_boundary := factor(
        eco_boundary_tr$name[match(eco_boundary_id, eco_boundary_tr$id)],
        levels = eco_boundary_tr$name)]
    }

    if (!"spatial_unit" %in% names(grid_meta)){

      if (!"admin_boundary_id" %in% names(grid_meta)){
        grid_meta[, admin_boundary_id := admin_boundary_tr$admin_boundary_id[
          match(admin_boundary, admin_boundary_tr$name)]]
      }
      if (!"eco_boundary_id" %in% names(grid_meta)){
        grid_meta[, eco_boundary_id := admin_boundary_tr$eco_boundary_id[
          match(eco_boundary, eco_boundary_tr$name)]]
      }

      spatial_unit <- cbmdbReadTable(cbm_defaults_db, "spatial_unit")[
        , .(spatial_unit = id, admin_boundary_id, eco_boundary_id)]

      grid_meta <- merge(
        grid_meta, spatial_unit,
        by = c("admin_boundary_id", "eco_boundary_id"), all.x = TRUE)

      # Check spatial unit IDs
      if (any(is.na(grid_meta$spatial_unit_id))){
        noMatch <- unique(grid_meta[is.na(spatial_unit_id), .(admin_boundary, eco_boundary_id)])
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
    if (!defCol %in% names(grid_meta)){
      grid_meta[, eval(defCol) := factor(get(defArg))]
    }else{
      grid_meta[is.na(eval(defCol)), eval(defCol) := factor(get(defArg))]
    }
  }

  data.table::setkey(grid_meta, pixel_index)
  data.table::setcolorder(grid_meta, intersect(c(
    "pixel_index", "chunk_index", "raster_index",
    "area",
    "spatial_unit", "admin_boundary_id", "admin_boundary", "eco_boundary_id", "eco_boundary",
    "land_class", "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ), names(grid_meta)))

  # Strings as factor
  for (col in names(grid_meta)[sapply(grid_meta, is.character)]) grid_meta[[col]] <- factor(grid_meta[[col]])

  return(grid_meta)
}



