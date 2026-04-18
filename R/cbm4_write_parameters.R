
#' CBM4 write spinup parameters
#'
#' Write spinup parameters to a CBM4 spatial parquet dataset.
#'
#' @param ... arguments to \code{\link{cbm4_write_parameters}}
#' @inherit cbm4_write_parameters return
#' @export
cbm4_write_spinup_parameters <- function(...){
  cbm4_write_parameters(
    dataset_name = "spinup_parameters",
    spinup       = TRUE,
    ...)
}

#' CBM4 write step parameters
#'
#' Write step parameters to a CBM4 spatial parquet dataset.
#'
#' @param ... arguments to \code{\link{cbm4_write_parameters}}
#' @inherit cbm4_write_parameters return
#' @export
cbm4_write_step_parameters  <- function(...){
  cbm4_write_parameters(
    dataset_name = "step_parameters",
    spinup       = FALSE,
    ...)
}


#' cbm4_write_parameters
#' @template cbm4_data
#' @template cbm_defaults_db
#' @param spinup logical. Spinup or step parameters.
#' @inheritParams cbm4_format_increments
#' @template dataset_name
#' @template dataset_path
#' @template template_name
#' @template template_path
#' @return `NULL`. Data will be written to the CBM4 spatial parquet dataset.
cbm4_write_parameters <- function(
    cbm4_data = NULL,
    cbm_defaults_db,
    gcMeta,
    gcIncr,
    classifiers,
    dataset_name,
    spinup,
    template_name = "inventory",
    template_path = file.path(cbm4_data, template_name),
    dataset_path  = file.path(cbm4_data, dataset_name)
){

  # Initiate dataset from template
  if (!file.exists(dataset_path)){
    arrow_space_dataset_copy_geo(
      dataset_name  = dataset_name,
      dataset_path  = dataset_path,
      template_name = template_name,
      template_path = template_path,
      partitions    = list("chunk_index" = "int64")
    )
  }

  # Format increments
  incTable <- cbm4_format_increments(
    cbm_defaults_db = cbm_defaults_db,
    gcMeta          = gcMeta,
    gcIncr          = gcIncr,
    classifiers     = classifiers,
    long            = !spinup
  )

  # Write increments
  incTableName <- ifelse(spinup, "parameters_increments_wide", "parameters_increments")

  arrow_space_dataset_write_table(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = paste0("table-", incTableName),
    table_data   = incTable
  )
  cbm4_write_parameter_table_metadata(
    dataset_name = dataset_name,
    dataset_path = dataset_path,
    table_name   = incTableName
  )

  # Write parameters_ecological
  cbm4_write_parameters_decay(
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    cbm_defaults_db = cbm_defaults_db
  )
  cbm4_write_parameters_turnover(
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    cbm_defaults_db = cbm_defaults_db
  )
  cbm4_write_parameters_root(
    dataset_name    = dataset_name,
    dataset_path    = dataset_path,
    cbm_defaults_db = cbm_defaults_db
  )
  if (spinup){
    cbm4_write_parameters_spinup(
      dataset_name    = dataset_name,
      dataset_path    = dataset_path,
      cbm_defaults_db = cbm_defaults_db
    )
  }else{
    cbm4_write_parameters_mean_annual_temp(
      dataset_name    = dataset_name,
      dataset_path    = dataset_path,
      cbm_defaults_db = cbm_defaults_db
    )
  }

  return(invisible())
}


#' CBM4 format increments
#' @param gcMeta data.table. Growth curve metadata
#' @param gcIncr data.table. Growth curve carbon increments
#'
#' @template classifiers
#' @param long logical. Format table long or wide.
#' @template cbm_defaults_db
#'
#' @return data.table
#' @keywords internal
cbm4_format_increments <- function(gcMeta, gcIncr, classifiers, long = TRUE, cbm_defaults_db = NULL){

  # Read tables
  gcMeta <- data.table::as.data.table(gcMeta)
  gcIncr <- data.table::as.data.table(gcIncr)

  # Set columns
  if (!"spatial_unit" %in% names(gcMeta)){
    if (any(c("admin_boundary_id", "admin_boundary", "admin_abbrev",
              "eco_boundary_id", "eco_boundary") %in% names(gcMeta))){
      set_table_spatial_units(gcMeta, "gcMeta", cbm_defaults_db = cbm_defaults_db, naOK = TRUE)
      gcMeta[is.na(spatial_unit), spatial_unit := "?"]
    }else{
      gcMeta[, spatial_unit := "?"]
    }
  }
  if (!"sw" %in% names(gcMeta) & "sw_hw" %in% names(gcMeta)) gcMeta[, sw := sw_hw == "sw"]

  # Check table columns
  check_table_columns_all("gcMeta", gcMeta, c("gcID", "spatial_unit", classifiers, "sw"))
  check_table_columns_all("gcIncr", gcIncr, c("gcID", "age", "merch_inc", "foliage_inc", "other_inc"))

  # Check classifiers
  if (length(classifiers) == 0) stop(">=1 'classifiers' are required.")
  if (!all(sapply(gcMeta, function(c) is.integer(c) | is.character(c) | is.factor(c))[classifiers])) stop(
    "classifiers must be integer, character, or factor")

  # Check for data gaps
  if (any(do.call(c, lapply(split(gcIncr$age, gcIncr[["gcID"]]), diff)) > 1)) stop(
    "gcIncr must have increments for every year")

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



