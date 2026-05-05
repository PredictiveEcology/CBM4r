
#' CBM4 write spinup parameters
#'
#' Write spinup parameters to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_format_increments
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#' @template cbm_defaults_db
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_spinup_parameters <- function(
    cbm4_data = NULL,
    gcMeta,
    gcIncr,
    classifiers,
    template_name   = "inventory",
    template_path   = file.path(cbm4_data, template_name),
    dataset_name    = "spinup_parameters",
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){
    if (!is.null(template_name)){
      arrow_space_dataset_copy_geo(
        dataset_name  = dataset_name,
        dataset_path  = dataset_path,
        template_name = template_name,
        template_path = template_path,
        partitions    = list("chunk_index" = "int64")
      )
    }else stop("Use `cbm4_write_geo` to initiate a new dataset or copy dataset attributes by setting `template_name.")
  }

  # Format increments
  incTable <- cbm4_format_increments(
    cbm_defaults_db = cbm_defaults_db,
    gcMeta          = gcMeta,
    gcIncr          = gcIncr,
    classifiers     = classifiers,
    long            = FALSE
  )

  # Write increments
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "table-parameters_increments_wide",
    table_data   = incTable,
    schema       = list(inventory.spatial_unit = arrow::string())
  )

  # Write ecological parameters
  eco_params = list(
    parameters_root     = parameters_root(cbm_defaults_db),
    parameters_decay    = parameters_decay(cbm_defaults_db),
    parameters_turnover = parameters_turnover(cbm_defaults_db),
    parameters_root     = parameters_root(cbm_defaults_db),
    parameters_spinup   = parameters_spinup(cbm_defaults_db)
  )
  for (table_name in names(eco_params)){
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = paste0("table-", table_name),
      table_data   = eco_params[[table_name]]
    )
  }

  # Write parameter table metadata
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "table-parameter_table_metadata",
    table_data   = data.table::data.table(table_name = c("parameters_increments_wide", names(eco_params)))
  )

  return(invisible())
}


#' CBM4 write step parameters
#'
#' Write step parameters to a CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @inheritParams cbm4_format_increments
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#' @template cbm_defaults_db
#'
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
#' @export
cbm4_write_step_parameters <- function(
    cbm4_data = NULL,
    gcMeta,
    gcIncr,
    classifiers,
    template_name   = "inventory",
    template_path   = file.path(cbm4_data, template_name),
    dataset_name    = "step_parameters",
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){
    if (!is.null(template_name)){
      arrow_space_dataset_copy_geo(
        dataset_name  = dataset_name,
        dataset_path  = dataset_path,
        template_name = template_name,
        template_path = template_path,
        partitions    = list("chunk_index" = "int64")
      )
    }else stop("Use `cbm4_write_geo` to initiate a new dataset or copy dataset attributes by setting `template_name.")
  }

  # Format increments
  incTable <- cbm4_format_increments(
    cbm_defaults_db = cbm_defaults_db,
    gcMeta          = gcMeta,
    gcIncr          = gcIncr,
    classifiers     = classifiers,
    long            = TRUE
  )

  # Write increments
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "table-parameters_increments",
    table_data   = incTable,
    schema       = list(inventory.spatial_unit = arrow::string())
  )

  # Write ecological parameters
  eco_params = list(
    parameters_root     = parameters_root(cbm_defaults_db),
    parameters_decay    = parameters_decay(cbm_defaults_db),
    parameters_turnover = parameters_turnover(cbm_defaults_db),
    parameters_root     = parameters_root(cbm_defaults_db),
    parameters_mean_annual_temp = parameters_mean_annual_temp(cbm_defaults_db)
  )
  for (table_name in names(eco_params)){
    arrow_space_dataset_write_table(
      dataset_name = dataset_name,
      dataset_path = dataset_path,
      table_name   = paste0("table-", table_name),
      table_data   = eco_params[[table_name]]
    )
  }

  # Write parameter table metadata
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = "table-parameter_table_metadata",
    table_data   = data.table::data.table(table_name = c("parameters_increments", names(eco_params)))
  )

  return(invisible())
}


#' CBM4 format increments
#'
#' @param gcMeta data.table. Growth curve metadata
#' @param gcIncr data.table. Growth curve carbon increments
#' @template classifiers
#' @param long logical. Format table long or wide.
#' @template cbm_defaults_db
#'
#' @return data.table
#' @keywords internal
cbm4_format_increments <- function(gcMeta, gcIncr, classifiers, long = TRUE,
                                   cbm_defaults_db = getOption("CBM4r.db.path")){

  # Read tables
  gcMeta <- data.table::as.data.table(gcMeta)
  gcIncr <- data.table::as.data.table(gcIncr)

  # Set columns
  if (!"spatial_unit" %in% names(gcMeta)){
    if (any(c("admin_boundary_id", "admin_boundary", "admin_abbrev",
              "eco_boundary_id", "eco_boundary") %in% names(gcMeta))){
      set_table_spatial_units(gcMeta, cbm_defaults_db, naOK = TRUE)
      gcMeta[is.na(spatial_unit), spatial_unit := "?"]
    }else{
      gcMeta[, spatial_unit := "?"]
    }
  }

  # Check table columns
  check_table_columns_all("gcMeta", gcMeta, c("gcID", "spatial_unit", classifiers, "sw"))
  check_table_columns_all("gcIncr", gcIncr, c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"))

  # Check classifiers
  if (length(classifiers) == 0) stop(">=1 'classifiers' are required.")
  if (!all(sapply(gcMeta, function(c) is.integer(c) | is.character(c) | is.factor(c))[classifiers])) stop(
    "classifiers must be integer, character, or factor")

  if (any(is.na(gcIncr[, .(merch_inc, foliage_inc, other_inc)]))) stop("Increments contain NA values")

  # Format increments
  data.table::setnames(gcIncr, "age", "state.age")

  gcIncr[gcMeta, sw := sw, on = "gcID"]

  gcIncr[sw==TRUE,  increment.SoftwoodMerch   := merch_inc]
  gcIncr[sw==TRUE,  increment.SoftwoodFoliage := foliage_inc]
  gcIncr[sw==TRUE,  increment.SoftwoodOther   := other_inc]
  gcIncr[sw==FALSE, increment.HardwoodMerch   := merch_inc]
  gcIncr[sw==FALSE, increment.HardwoodFoliage := foliage_inc]
  gcIncr[sw==FALSE, increment.HardwoodOther   := other_inc]

  gcIncr[is.na(increment.SoftwoodMerch),   increment.SoftwoodMerch   := 0]
  gcIncr[is.na(increment.SoftwoodFoliage), increment.SoftwoodFoliage := 0]
  gcIncr[is.na(increment.SoftwoodOther),   increment.SoftwoodOther   := 0]
  gcIncr[is.na(increment.HardwoodMerch),   increment.HardwoodMerch   := 0]
  gcIncr[is.na(increment.HardwoodFoliage), increment.HardwoodFoliage := 0]
  gcIncr[is.na(increment.HardwoodOther),   increment.HardwoodOther   := 0]

  gcIncr[, sw          := NULL]
  gcIncr[, merch_inc   := NULL]
  gcIncr[, foliage_inc := NULL]
  gcIncr[, other_inc   := NULL]

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


parameters_decay <- function(cbm_defaults_db = getOption("CBM4r.db.path")){

  params <- cbm_defaults_db_table("decay_parameter", cbm_defaults_db)

  domPools <- merge(
    cbm_defaults_db_table("dom_pool", cbm_defaults_db),
    cbm_defaults_db_table("pool",     cbm_defaults_db),
    by.x = "pool_id", by.y = "id")

  decayRemap <- list(
    dom_pool_id               = "dom_pool_id",
    decay.POOL_NAME.base_rate = "base_decay_rate",
    decay.POOL_NAME.tref      = "reference_temp",
    decay.POOL_NAME.q10       = "q10",
    decay.POOL_NAME.p_atm     = "prop_to_atmosphere",
    decay.POOL_NAME.max_rate  = "max_rate"
  )
  params <- data.table::cbindlist(
    lapply(split(params, params$dom_pool_id), function(r){
      pool_name <- domPools[id == r$dom_pool_id]$code
      data.table::setnames(r, do.call(c, decayRemap), gsub("POOL_NAME", pool_name, names(decayRemap)))
      r[, -c("dom_pool_id")]
    })
  )

  return(params)
}

parameters_turnover <- function(cbm_defaults_db = getOption("CBM4r.db.path")){

  params <- merge(
    merge(
      cbm_defaults_db_table("spatial_unit", cbm_defaults_db)[, .(inventory.spatial_unit = id, id = eco_boundary_id)],
      cbm_defaults_db_table("eco_boundary", cbm_defaults_db),
      by = "id")[, .(inventory.spatial_unit, id = turnover_parameter_id)],
    cbm_defaults_db_table("turnover_parameter", cbm_defaults_db)[, .(
      id,
      turnover.sw_merch                      = stem_turnover,     # merch = stem; same for SW or HW
      turnover.sw_foliage                    = sw_foliage,
      turnover.sw_other                      = sw_branch,         # other = branch + bark
      turnover.sw_stem_snag                  = sw_stem_snag,
      turnover.sw_branch_snag                = sw_branch_snag,
      turnover.sw_other_to_branch_snag_split = branch_snag_split, # same for SW or HW
      turnover.sw_coarse_root                = coarse_root,       # same for SW or HW
      turnover.sw_coarse_root_ag_split       = coarse_ag_split,   # same for SW or HW
      turnover.sw_fine_root                  = fine_root,         # same for SW or HW
      turnover.sw_fine_root_ag_split         = fine_ag_split,     # same for SW or HW
      turnover.hw_merch                      = stem_turnover,     # merch = stem; same for SW or HW
      turnover.hw_foliage                    = hw_foliage,
      turnover.hw_other                      = hw_branch,         # other = branch + bark
      turnover.hw_stem_snag                  = hw_stem_snag,
      turnover.hw_branch_snag                = hw_branch_snag,
      turnover.hw_other_to_branch_snag_split = branch_snag_split, # same for SW or HW
      turnover.hw_coarse_root                = coarse_root,       # same for SW or HW
      turnover.hw_coarse_root_ag_split       = coarse_ag_split,   # same for SW or HW
      turnover.hw_fine_root                  = fine_root,         # same for SW or HW
      turnover.hw_fine_root_ag_split         = fine_ag_split      # same for SW or HW
    )],
    by = "id")

  data.table::setkey(params, inventory.spatial_unit)
  params[, id := NULL]
  params[, turnover.slow_mixing_rate := cbm_defaults_db_table("slow_mixing_rate", cbm_defaults_db)$rate]

  return(params)
}

parameters_root <- function(cbm_defaults_db = getOption("CBM4r.db.path")){

  params <- cbind(
    cbm_defaults_db_table("root_parameter", cbm_defaults_db)[, .(
      root.hw_a  = hw_a,
      root.sw_a  = sw_a,
      root.hw_b  = hw_b,
      root.frp_a = frp_a,
      root.frp_b = frp_b,
      root.frp_c = frp_c
    )],
    cbm_defaults_db_table("biomass_to_carbon_rate", cbm_defaults_db)[, .(
      root.biomass_to_carbon_rate = rate
    )]
  )

  return(params)
}

parameters_spinup <- function(cbm_defaults_db = getOption("CBM4r.db.path")){

  params <- merge(
    cbm_defaults_db_table("spatial_unit", cbm_defaults_db)[, .(
      inventory.spatial_unit = id, id = spinup_parameter_id, mean_annual_temperature)],
    cbm_defaults_db_table("spinup_parameter", cbm_defaults_db),
    by = "id", all.x = TRUE)

  data.table::setkey(params, inventory.spatial_unit)
  params[, id := NULL]
  params[, historic_mean_temperature := NULL]

  # Set enabled
  params[, enabled := 1]

  return(params)
}

parameters_mean_annual_temp <- function(cbm_defaults_db = getOption("CBM4r.db.path")){

  params <- cbm_defaults_db_table("spatial_unit", cbm_defaults_db)[, .(
    inventory.spatial_unit = id,
    mean_annual_temperature
  )]

  return(params)
}





