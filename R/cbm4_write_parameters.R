
#' CBM4 write spinup parameters
#'
#' Write spinup parameters to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @param ... arguments to \code{\link{cbm4_format_increments}}
#' @inheritParams cbm4_write_geo
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_spinup_parameters <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    grid_rast     = NULL,
    grid_chunks   = 1,
    template_name = NULL,
    template_path = file.path(cbm4_data, template_name),
    dataset_name  = "spinup_parameters",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  cbm4_write_parameters(
    cbm4_data       = cbm4_data,
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    grid_rast       = grid_rast,
    grid_chunks     = grid_chunks,
    template_name   = template_name,
    template_path   = template_path,
    cbm_defaults_db = cbm_defaults_db,
    long            = FALSE,
    ...
  )
}


#' CBM4 write step parameters
#'
#' Write step parameters to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template cbm_defaults_db
#' @param ... arguments to \code{\link{cbm4_format_increments}}
#' @inheritParams cbm4_write_geo
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_step_parameters <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    grid_rast     = NULL,
    grid_chunks   = 1,
    template_name = NULL,
    template_path = file.path(cbm4_data, template_name),
    dataset_name  = "step_parameters",
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  cbm4_write_parameters(
    cbm4_data       = cbm4_data,
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    grid_rast       = grid_rast,
    grid_chunks     = grid_chunks,
    template_name   = template_name,
    template_path   = template_path,
    cbm_defaults_db = cbm_defaults_db,
    long            = TRUE,
    ...
  )
}


# Helper function: write increments long or wide
cbm4_write_parameters <- function(
    cbm4_data = NULL,
    dataset_name,
    cbm_defaults_db,
    long,
    grid_rast     = NULL,
    grid_chunks   = 1,
    template_name = NULL,
    template_path = file.path(cbm4_data, template_name),
    dataset_path  = file.path(cbm4_data, dataset_name),
    ...
){

  # Format increments
  incTable <- cbm4_format_increments(
    cbm_defaults_db = cbm_defaults_db,
    long = long,
    ...
  )

  # Initiate dataset
  cbm4_write_geo(
    dataset_name  = dataset_name,
    dataset_path  = dataset_path,
    grid_rast     = grid_rast,
    grid_chunks   = grid_chunks,
    template_name = template_name,
    template_path = template_path,
    partitions    = list("chunk_index" = "int64"),
    write_pixels  = FALSE
  )

  # Write increments
  incTableName <- ifelse(long, "parameters_increments", "parameters_increments_wide")

  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", incTableName),
    table_data   = incTable
  )

  # Write parameters_ecological
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "table-parameters_ecological",
    table_data   = cbm4_format_parameters_ecological(cbm_defaults_db)
  )

  # Write parameter_table_metadata
  paramTables <- data.table::data.table(table_name = c("parameters_ecological", incTableName))
  paramTablesPath <- file.path(dataset_path, paste0(dataset_name, "-table-parameter_table_metadata"))
  if (file.exists(paramTablesPath)){
    paramTables <- unique(rbind(
      dplyr::collect(arrow::open_dataset(paramTablesPath)),
      paramTables
    ))
  }
  arrow::write_dataset(paramTables, paramTablesPath)

  return(invisible())
}


#' CBM4 format increments
#'
#' @template classifiers
#' @param gcMeta data.table. Growth curve metadata
#' @param gcIncr data.table. Growth curve carbon increments
#' @param long logical. Format table long or wide.
#' @template cbm_defaults_db
#'
#' @return data.table
cbm4_format_increments <- function(classifiers, gcMeta, gcIncr, long = TRUE, cbm_defaults_db = NULL){

  # Read tables
  gcMeta <- data.table::as.data.table(gcMeta)
  gcIncr <- data.table::as.data.table(gcIncr)

  # Rename columns
  data.table::setnames(gcMeta, "admin_name", "admin_boundary",  skip_absent = TRUE)
  data.table::setnames(gcMeta, "eco_id",     "eco_boundary_id", skip_absent = TRUE)

  # Set columns
  if (!"sw" %in% names(gcMeta) & "sw_hw" %in% names(gcMeta)) gcMeta[, sw := sw_hw == "sw"]

  # Check table columns
  check_table_columns_all("gcMeta", gcMeta, c("gcID", classifiers, "sw"))
  check_table_columns_all("gcIncr", gcIncr, c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"))

  # Check for data gaps
  if (any(do.call(c, lapply(split(gcIncr$age, gcIncr[["gcID"]]), diff)) > 1)) stop(
    "gcIncr must have increments for every year")

  # Set spatial_unit
  if (!"spatial_unit" %in% names(gcMeta)){

    check_table_columns_all("gcMeta", gcMeta, c("admin_boundary", "eco_boundary_id"))

    if (is.null(cbm_defaults_db)) stop("cbm_defaults_db required to set spatial_unit")
    spatial_unit <- merge(
      cbmdbReadTable(cbm_defaults_db, "spatial_unit"),
      cbmdbReadTable(cbm_defaults_db, "admin_boundary_tr"),
      by.x = "admin_boundary_id", by.y = "id", all.x = TRUE)

    gcMeta <- data.table::merge.data.table(
      gcMeta,
      data.table::as.data.table(spatial_unit)[, .(admin_boundary = name, eco_boundary_id, spatial_unit = id)],
      by = c("admin_boundary", "eco_boundary_id"), all.x = TRUE, sort = FALSE)

    # Check spatial unit IDs
    if (any(is.na(gcMeta$spatial_unit))){
      noMatch <- unique(gcMeta[is.na(spatial_unit), .(admin_boundary, eco_boundary_id)])
      data.table::setkey(noMatch, admin_boundary, eco_boundary_id)
      if (nrow(noMatch) > 0) stop(
        "spatial_unit_id not found for: ",
        paste(paste(noMatch$admin_boundary, "ecozone", noMatch$eco_boundary_id), collapse = "; "))
    }
  }

  # Format increments
  data.table::setnames(gcIncr, "age", "state.age")
  incrSW <- gcMeta$sw[match(gcIncr[["gcID"]], gcMeta[["gcID"]])]

  gcIncr[ incrSW, increment.SoftwoodMerch   := merch_inc]
  gcIncr[ incrSW, increment.SoftwoodFoliage := foliage_inc]
  gcIncr[ incrSW, increment.SoftwoodOther   := other_inc]
  gcIncr[!incrSW, increment.SoftwoodMerch   := 0]
  gcIncr[!incrSW, increment.SoftwoodFoliage := 0]
  gcIncr[!incrSW, increment.SoftwoodOther   := 0]

  gcIncr[!incrSW, increment.HardwoodMerch   := merch_inc]
  gcIncr[!incrSW, increment.HardwoodFoliage := foliage_inc]
  gcIncr[!incrSW, increment.HardwoodOther   := other_inc]
  gcIncr[ incrSW, increment.HardwoodMerch   := 0]
  gcIncr[ incrSW, increment.HardwoodFoliage := 0]
  gcIncr[ incrSW, increment.HardwoodOther   := 0]

  gcIncr[, merch_inc   := NULL]
  gcIncr[, foliage_inc := NULL]
  gcIncr[, other_inc   := NULL]
  rm(incrSW)

  # Format metadata
  gcMeta <- gcMeta[, .SD, .SDcols = c("gcID", classifiers, "spatial_unit")]
  data.table::setattr(gcMeta, "names", c("gcID", paste0("classifiers.", classifiers), "inventory.spatial_unit"))

  if (long){

    gcIncr <- merge(gcMeta, gcIncr, by = "gcID")
    gcIncr[, eval("gcID") := NULL]

    # Add row for increments above greatest age
    gcIncrWC <- gcIncr[state.age == max(state.age), .SD, by = c(paste0("classifiers.", classifiers), "inventory.spatial_unit")]
    gcIncrWC[["state.age"]] <- "?"
    gcIncr <- rbind(gcIncr, gcIncrWC)
    data.table::setkeyv(gcIncr, c(paste0("classifiers.", classifiers), "inventory.spatial_unit"))

  }else{

    # Format increments wide
    incCols <- paste0(
      "increment.", c(
        paste0("Softwood", c("Merch", "Foliage", "Other")),
        paste0("Hardwood", c("Merch", "Foliage", "Other"))
      ))
    gcIncr <- data.table::mergelist(
      lapply(incCols, function(incCol){
        incWide <- data.table::dcast(gcIncr, gcID ~ state.age, value.var = incCol)
        data.table::setnames(incWide, names(incWide)[-1], paste0(incCol, ".", names(incWide)[-1]))
        incWide
      }),
      on = "gcID", how = "left")

    gcIncr <- merge(gcMeta, gcIncr, by = "gcID")
    gcIncr[, gcID := NULL]
  }

  return(gcIncr)
}


# CBM4 format parameters ecological
cbm4_format_parameters_ecological <- function(cbm_defaults_db){

  # Read tables
  paramTableNames <- c(
    "spatial_unit", "spinup_parameter", "eco_boundary", "turnover_parameter", "root_parameter",
    "decay_parameter", "dom_pool", "pool",
    "slow_mixing_rate", "biomass_to_carbon_rate"
  )
  paramTables <- lapply(paramTableNames, cbmdbReadTable, cbm_defaults_db = cbm_defaults_db)
  names(paramTables) <- paramTableNames

  # Transform tables
  data.table::setnames(paramTables$spatial_unit, "id", "inventory.spatial_unit")

  turnoverRemap <- list(
    id                                     = "id",
    turnover.sw_merch                      = "stem_turnover", # TODO: check
    turnover.sw_foliage                    = "sw_foliage",
    turnover.sw_other                      = "sw_branch", # TODO: check
    turnover.sw_stem_snag                  = "sw_stem_snag",
    turnover.sw_branch_snag                = "sw_branch_snag",
    turnover.sw_other_to_branch_snag_split = "branch_snag_split",  # TODO: check
    turnover.sw_coarse_root                = "coarse_root",
    turnover.sw_coarse_root_ag_split       = "coarse_ag_split",
    turnover.sw_fine_root                  = "fine_root",
    turnover.sw_fine_root_ag_split         = "fine_ag_split",
    turnover.hw_merch                      = "stem_turnover", # TODO: check
    turnover.hw_foliage                    = "hw_foliage",
    turnover.hw_other                      = "hw_branch", # TODO: check
    turnover.hw_stem_snag                  = "hw_stem_snag",
    turnover.hw_branch_snag                = "hw_branch_snag",
    turnover.hw_other_to_branch_snag_split = "branch_snag_split", # TODO: check
    turnover.hw_coarse_root                = "coarse_root",
    turnover.hw_coarse_root_ag_split       = "coarse_ag_split",
    turnover.hw_fine_root                  = "fine_root",
    turnover.hw_fine_root_ag_split         = "fine_ag_split"
  )
  paramTables$turnover_parameter <- data.table::as.data.table(
    lapply(turnoverRemap, function(x) paramTables$turnover_parameter[[x]]))

  rootRemap <- list(
    id         = "id",
    root.hw_a  = "hw_a",
    root.sw_a  = "sw_a",
    root.hw_b  = "hw_b",
    root.frp_a = "frp_a",
    root.frp_b = "frp_b",
    root.frp_c = "frp_c"
  )
  data.table::setnames(paramTables$root_parameter, do.call(c, rootRemap), names(rootRemap))

  decayRemap <- list(
    dom_pool_id               = "dom_pool_id",
    decay.POOL_NAME.base_rate = "base_decay_rate",
    decay.POOL_NAME.tref      = "reference_temp",
    decay.POOL_NAME.q10       = "q10",
    decay.POOL_NAME.p_atm     = "prop_to_atmosphere",
    decay.POOL_NAME.max_rate  = "max_rate"
  )
  domPools <- merge(paramTables$dom_pool, paramTables$pool, by.x = "pool_id", by.y = "id")
  paramTables$decay_parameter <- data.table::cbindlist(
    lapply(split(paramTables$decay_parameter, paramTables$decay_parameter$dom_pool_id), function(r){
      pool_name <- domPools[id == r$dom_pool_id]$code
      data.table::setnames(r, do.call(c, decayRemap), gsub("POOL_NAME", pool_name, names(decayRemap)))
      r[, -c("dom_pool_id")]
    })
  )

  # Merge tables
  params <- paramTables$spatial_unit |>
    merge(paramTables$spinup_parameter,   by.x = "spinup_parameter_id",   by.y = "id") |>
    merge(paramTables$eco_boundary,       by.x = "eco_boundary_id",       by.y = "id") |>
    merge(paramTables$turnover_parameter, by.x = "turnover_parameter_id", by.y = "id") |>
    merge(paramTables$root_parameter,     by.x = "root_parameter_id",     by.y = "id")

  params <- cbind(params, paramTables$decay_parameter)

  params[, turnover.slow_mixing_rate   := paramTables$slow_mixing_rate$rate]
  params[, root.biomass_to_carbon_rate := paramTables$biomass_to_carbon_rate$rate]

  # Set enabled
  params[, enabled := 1]

  # Format and return
  data.table::setkey(params, inventory.spatial_unit)
  data.table::setcolorder(params)
  params
}



