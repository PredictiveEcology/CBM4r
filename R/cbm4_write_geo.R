
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
        pixel_index       = arrow::int16(),
        chunk_index       = arrow::int8(),
        raster_index      = arrow::int16(),
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


#' set_grid_meta
#'
#' Format `grid_meta` table by reference.
#'
#' @param grid_meta data.table. Grid metadata.
#' Required columns: `pixel_index`,
#' `admin_boundary`, `admin_abbrev`, or `admin_boundary_id`.
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
#' @keywords internal
set_grid_meta <- function(
    grid_meta,
    cbm_defaults_db = NULL,
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    ...
){

  if (is.null(grid_meta)) stop("grid_meta required")

  check_table_columns_all("grid_meta", grid_meta, "pixel_index")

  if (!"chunk_index"  %in% names(grid_meta)) data.table::set(grid_meta, j = "chunk_index",  value = 0L)
  if (!"raster_index" %in% names(grid_meta)) data.table::set(grid_meta, j = "raster_index", value = grid_meta$pixel_index - 1)

  # Set spatial units
  set_table_spatial_units(grid_meta, "grid_meta", cbm_defaults_db = cbm_defaults_db)

  # Set defaults
  set_table_defaults(grid_meta)

  data.table::setkey(grid_meta, pixel_index)
  data.table::setcolorder(grid_meta, intersect(c(
    "pixel_index", "chunk_index", "raster_index",
    "area",
    "spatial_unit", "admin_boundary_id", "admin_boundary", "admin_abbrev", "eco_boundary_id", "eco_boundary",
    "land_class", "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ), names(grid_meta)))

  return(grid_meta)
}

set_table_spatial_units <- function(table, tableName = "table", naOK = FALSE, cbm_defaults_db = NULL){

  if (nrow(table) == 0){
    if (!"admin_boundary" %in% names(table)) table[, admin_boundary := character(0)]
    if (!"eco_boundary"   %in% names(table)) table[, eco_boundary   := character(0)]
    if (!"spatial_unit"   %in% names(table)) table[, spatial_unit   := integer(0)]
  }

  if (naOK){

    if (!any(c("admin_boundary_id", "admin_boundary", "admin_abbrev") %in% names(table))){
      data.table::set(table, j = "admin_boundary", value = "?")
    }
    if (!any(c("eco_boundary_id", "eco_boundary") %in% names(table))){
      data.table::set(table, j = "eco_boundary", value = "?")
    }

    if ("admin_boundary"    %in% names(table)) table[is.na(admin_boundary),    admin_boundary := "?"]
    if ("eco_boundary"      %in% names(table)) table[is.na(eco_boundary),      eco_boundary   := "?"]
    if ("admin_boundary_id" %in% names(table)) table[is.na(admin_boundary_id), admin_boundary_id := 0]
    if ("eco_boundary_id"   %in% names(table)) table[is.na(eco_boundary_id),   eco_boundary_id   := 0]

    on.exit({
      table[admin_boundary == "?", admin_boundary := NA_character_]
      table[eco_boundary   == "?", eco_boundary   := NA_character_]
      if ("admin_boundary_id" %in% names(table)) table[admin_boundary_id == 0, admin_boundary_id := NA_integer_]
      if ("eco_boundary_id"   %in% names(table)) table[eco_boundary_id   == 0, eco_boundary_id   := NA_integer_]
      table[spatial_unit      == 0, spatial_unit      := NA_integer_]
      data.table::setindex(table, NULL)
    })
  }

  check_table_columns_any(tableName, table, c("admin_boundary_id", "admin_boundary", "admin_abbrev"))
  check_table_columns_any(tableName, table, c("eco_boundary_id",   "eco_boundary"))

  if (!"admin_boundary" %in% names(table) & "admin_abbrev" %in% names(table)){

    if (naOK) adminEquiv <- rbind(adminEquiv, data.frame(admin_abbrev = NA, admin_boundary = "?"))

    data.table::set(table, j = "admin_abbrev", value = factor(
      as.character(table$admin_abbrev), levels = unique(adminEquiv$admin_abbrev)))

    if (anyNA(table$admin_boundary)) stop(
      "admin_abbrev invalid; choose from: ", paste(shQuote(unique(
        adminEquiv$admin_abbrev)), collapse = ", "))

    table[adminEquiv, admin_boundary := admin_boundary, on = "admin_abbrev"]
  }

  if (!all(c("admin_boundary", "eco_boundary", "spatial_unit") %in% names(table))){

    admin_tr <- cbmdbReadTable(cbm_defaults_db, "admin_boundary_tr")[, .(admin_boundary_id, admin_boundary = name)]
    if (naOK) admin_tr <- rbind(admin_tr, data.frame(admin_boundary_id = 0, admin_boundary = "?"))

    eco_tr <- cbmdbReadTable(cbm_defaults_db, "eco_boundary_tr")[, .(eco_boundary_id, eco_boundary = name)]
    if (naOK) eco_tr <- rbind(eco_tr, data.frame(eco_boundary_id = 0, eco_boundary = "?"))

    if (!"admin_boundary" %in% names(table)){
      table[admin_tr, admin_boundary := admin_boundary, on = "admin_boundary_id"]
      if (anyNA(table$admin_boundary)) stop(
        "admin_boundary_id invalid; choose from: ", paste(admin_tr$admin_boundary_id, collapse = ", "))
    }

    if (!"eco_boundary" %in% names(table)){
      table[eco_tr, eco_boundary := eco_boundary, on = "eco_boundary_id"]
      if (anyNA(table$eco_boundary)) stop(
        "eco_boundary_id invalid; choose from: ", paste(eco_tr$eco_boundary_id, collapse = ", "))
    }

    if (!"spatial_unit" %in% names(table)){

      spatial_unit <- cbmdbReadTable(cbm_defaults_db, "spatial_unit")[, .(id, admin_boundary_id, eco_boundary_id)]
      if (naOK) spatial_unit <- rbind(spatial_unit, data.frame(id = 0, admin_boundary_id = 0, eco_boundary_id = 0))

      if (!"admin_boundary_id" %in% names(table)){
        table[admin_tr, admin_boundary_id := admin_boundary_id, on = "admin_boundary"]
        if (anyNA(table$admin_boundary_id)) stop(
          "admin_boundary invalid; choose from: ", paste(shQuote(admin_tr$admin_boundary), collapse = ", "))
      }
      if (!"eco_boundary_id" %in% names(table)){
        table[eco_tr, eco_boundary_id := eco_boundary_id, on = "eco_boundary"]
        if (anyNA(table$eco_boundary_id)) stop(
          "eco_boundary invalid; choose from: ", paste(shQuote(eco_tr$eco_boundary)), collapse = ", ")
      }

      table[spatial_unit, spatial_unit := id, on = .(admin_boundary_id, eco_boundary_id)]

      if (anyNA(table$spatial_unit)){
        noMatch <- unique(table[is.na(spatial_unit), .(admin_boundary, eco_boundary)])
        data.table::setkey(noMatch, admin_boundary, eco_boundary)

        if (nrow(noMatch) > 0) stop(
          "spatial_unit_id(s) not found for:\n- ",
          paste(paste0("admin_boundary: ", shQuote(noMatch$admin_boundary), "; ",
                       "eco_boundary: ",   shQuote(noMatch$eco_boundary)),
                collapse = "\n- "))
      }
    }
  }

  data.table::setindex(table, NULL)
  return(table)
}



