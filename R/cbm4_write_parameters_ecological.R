
cbm4_write_parameters_decay <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Read and format parameters
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

  # Write parameters
  table_name <- "parameters_decay"
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", table_name),
    table_data   = params
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = table_name
  )
}

cbm4_write_parameters_turnover <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Read and format parameters
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

  # Write parameters
  table_name <- "parameters_turnover"
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", table_name),
    table_data   = params
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = table_name
  )
}

cbm4_write_parameters_root <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Read and format parameters
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

  # Write parameters
  table_name <- "parameters_root"
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", table_name),
    table_data   = params
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = table_name
  )
}

cbm4_write_parameters_spinup <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Read and format parameters
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

  # Write parameters
  table_name <- "parameters_spinup"
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", table_name),
    table_data   = params
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = table_name
  )
}

cbm4_write_parameters_mean_annual_temp <- function(
    cbm4_data = NULL,
    dataset_name,
    dataset_path    = file.path(cbm4_data, dataset_name),
    cbm_defaults_db = getOption("CBM4r.db.path")
){

  # Read and format parameters
  params <- cbm_defaults_db_table("spatial_unit", cbm_defaults_db)[, .(
    inventory.spatial_unit = id,
    mean_annual_temperature
  )]

  # Write parameters
  table_name <- "parameters_mean_annual_temp"
  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", table_name),
    table_data   = params
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = table_name
  )
}

cbm4_write_parameter_table_metadata <- function(
    cbm4_data = NULL,
    dataset_name,
    table_name,
    dataset_path = file.path(cbm4_data, dataset_name)
){

  mtdPath <- file.path(dataset_path, paste0(dataset_name, "-table-parameter_table_metadata"))

  mtdTable <- data.table::data.table(table_name = table_name)
  if (file.exists(mtdPath)){
    mtdTable <- unique(rbind(
      dplyr::collect(arrow::open_dataset(mtdPath)),
      mtdTable
    ))
  }

  arrow::write_dataset(mtdTable, mtdPath)
}



