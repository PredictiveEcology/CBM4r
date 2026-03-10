
#' CBM4 write inventory
#'
#' Write inventory to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @template classifiers
#' @param ... arguments to \code{\link{cbm4_format_inventory}}
#' @inheritParams cbm4_format_inventory
#' @inheritParams cbm4_write_geo
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_inventory <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    classifiers,
    inventoryDT,
    grid_rast     = NULL,
    grid_chunks   = 1,
    template_name = NULL,
    template_path = file.path(cbm4_data, template_name),
    dataset_name = "inventory",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  if (length(classifiers) == 0) stop(">=1 'classifiers' are required.")
  if (!all(classifiers %in% names(inventoryDT))) stop("inventoryDT requires all classifiers")
  if (!all(sapply(inventoryDT, function(c) is.integer(c) | is.character(c) | is.factor(c))[classifiers])) stop(
    "classifiers must be integer, character, or factor")

  # Initiate dataset
  cbm4_write_geo(
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    grid_rast     = grid_rast,
    grid_chunks   = grid_chunks,
    template_name = template_name,
    template_path = template_path,
    partitions    = list("cohort_index" = "int64", "chunk_index" = "int64"),
    tags          = list(classifier = classifiers)
  )

  # Format inventory
  inv <- cbm4_format_inventory(
    cbm_defaults_db = cbm_defaults_db,
    pixelDT = arrow_space_dataset_read_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = "table-pixels"
    ),
    inventoryDT,
    ...)

  # Write inventory
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = NULL,
    table_data   = inv$flat,
    partitioning = c("cohort_index", "chunk_index")
  )
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "raster_index",
    table_data   = inv$index,
    partitioning = c("cohort_index", "chunk_index")
  )

  # Write CBM defaults database to file
  arrow_space_dataset_write_file_or_dir(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    file_name    = "cbm_defaults",
    file_path    = cbm_defaults_db
  )

  return(invisible())
}


#' CBM4 format inventory
#'
#' @param inventoryDT data.table. Cohort inventory.
#' @param pixelDT TODO
#' @template cbm_defaults_db
#' @param def_land_class character. Land class code.
#' Defined in CBM defaults database tables 'land_class' and 'land_class_tr'.
#' @param def_afforestation_pre_type character. Land use before forestation.
#' Defined in CBM defaults database tables 'afforestation_pre_type'
#' @param def_historic_disturbance_type character. Historic disturbance type.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_last_pass_disturbance_type character. Last pass disturbance.
#' Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
#' @param def_delay integer. Regeneration delay.
#' @param def_cohort_proportion integer. A value between 0-1.
#' Percentage of the pixel's area that is attributed to the cohort.
#'
#' @return list with items:
#' **index**: `arrow_space` raster indexed `data.table`;
#' **flat**: `arrow_space` flattened dataset `data.table`
cbm4_format_inventory <- function(
    inventoryDT,
    pixelDT,
    cbm_defaults_db = NULL,
    def_land_class                 = "UNFCCC_FL_R_FL", # "Forest Land remaining Forest Land"
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    def_delay                      = 0L,
    def_cohort_proportion          = 1L
){

  # Rename columns
  dataFull <- data.table::as.data.table(inventoryDT)
  data.table::setnames(dataFull, "pixelIndex", "pixel_index",       skip_absent = TRUE)
  data.table::setnames(dataFull, "admin_id",   "admin_boundary_id", skip_absent = TRUE)
  data.table::setnames(dataFull, "admin_name", "admin_boundary",    skip_absent = TRUE)
  data.table::setnames(dataFull, "eco_id",     "eco_boundary_id",   skip_absent = TRUE)
  data.table::setnames(dataFull, "eco_name",   "eco_boundary",      skip_absent = TRUE)
  data.table::setnames(dataFull, "spatial_unit_id", "spatial_unit", skip_absent = TRUE)

  # Check table columns
  check_table_columns_all("inventoryDT", dataFull, c(
    "pixel_index", "area", "admin_boundary", "age"
  ))
  check_table_columns_any("inventoryDT", dataFull, c("eco_boundary_id", "eco_boundary"))

  # Join with pixel table
  dataFull <- merge(dataFull, pixelDT[, .(pixel_index, chunk_index, raster_index)],
                    by = "pixel_index", all.x = TRUE)
  dataFull[, pixel_index := NULL]

  # Set cohort index
  ## Setting this to chunk_index for all cohorts until otherwise needed
  dataFull[, cohort_index := chunk_index]

  # Set index
  dataFull[, index := .GRP - 1, by = setdiff(names(dataFull), c("raster_index", "area"))]

  # Set area
  dataFull[, area := as.numeric(sum(area)), by = index]

  # Split by raster key and unique groups
  dataIndex <- dataFull[, .(index, raster_index, cohort_index, chunk_index)]
  data.table::setkeyv(dataIndex, names(dataIndex))

  dataFull <- unique(dataFull[, .SD, .SDcols = setdiff(names(dataFull), "raster_index")])
  data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
  data.table::setcolorder(dataFull)

  # Ensure that area is numeric, not integer
  dataFull[, area := as.numeric(area)]

  # Set defaults
  for (defArg in names(environment())[grepl("^def\\_", names(environment()))]){
    defCol <- sub("^def\\_", "", defArg)
    if (!defCol %in% names(dataFull)){
      dataFull[, eval(defCol) := get(defArg)]
    }else{
      data.table::setnafill(dataFull, cols = defCol, fill = get(defArg))
    }
  }

  # Set eco_boundary
  if (!"eco_boundary" %in% names(dataFull)){

    eco_boundary_tr <- cbmdbReadTable(cbm_defaults_db, "eco_boundary_tr")

    dataFull[, eco_boundary := factor(
      eco_boundary_tr$name[match(eco_boundary_id, eco_boundary_tr$id)],
      levels = eco_boundary_tr$name)]
  }

  # Set spatial_unit
  if (!"spatial_unit" %in% names(dataFull)){

    spatial_unit <- merge(
      cbmdbReadTable(cbm_defaults_db, "spatial_unit"),
      cbmdbReadTable(cbm_defaults_db, "admin_boundary_tr"),
      by.x = "admin_boundary_id", by.y = "id")[, .(
        admin_boundary = name, eco_boundary_id, spatial_unit = id)]

    dataFull <- merge(
      dataFull, spatial_unit,
      by = c("admin_boundary", "eco_boundary_id"), all.x = TRUE)
    data.table::setkeyv(dataFull, setdiff(names(dataIndex), "raster_index"))
    data.table::setcolorder(dataFull)

    # Check spatial unit IDs
    if (any(is.na(dataFull$spatial_unit_id))){
      noMatch <- unique(dataFull[is.na(spatial_unit_id), .(admin_boundary, eco_boundary_id)])
      data.table::setkey(noMatch, admin_boundary, eco_boundary_id)
      if (nrow(noMatch) > 0) stop(
        "spatial_unit_id not found for: ",
        paste(paste(noMatch$admin_boundary, "ecozone", noMatch$eco_boundary_id), collapse = "; "))
    }
  }

  # Return
  list(
    index = dataIndex,
    flat  = dataFull
  )
}



