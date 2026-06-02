
.onLoad <- function(libname, pkgname){

  # Download CBM defaults SQLite database
  cbm_defaults_db_URL <- "https://raw.githubusercontent.com/cat-cfs/libcbm_py/main/libcbm/resources/cbm_defaults_db/cbm_defaults_v1.2.9300.391.db"
  cbm_defaults_db     <- file.path(tools::R_user_dir("CBM4r"), basename(cbm_defaults_db_URL))
  if (!file.exists(cbm_defaults_db)){
    dir.create(dirname(cbm_defaults_db), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      utils::download.file(cbm_defaults_db_URL, destfile = cbm_defaults_db, mode = "wb", quiet = TRUE),
      error = function(e) warning("CBM4r: CBM defaults database download failed: ", e$message))
  }

  # Set options
  op.CBM4r <- list(
    CBM4r.db.path     = if (file.exists(cbm_defaults_db)) cbm_defaults_db,
    CBM4r.db.localeID = 1L
  )
  toset <- !(names(op.CBM4r) %in% names(options()))
  if (any(toset)) options(op.CBM4r[toset])

  return(invisible())
}


# data.table package common variables
utils::globalVariables(c(".", ":=", ".SD", ".I", ".N", ".BY", ".GRP"))

# CBM defaults database columns
utils::globalVariables(c(
  "id", "locale_id", "name", "description",
  "admin_boundary_id", "eco_boundary_id", "spatial_unit_id",
  "disturbance_type_id"
))

# CBM4 dataset columns
utils::globalVariables(c(
  arrow_space = c("index", "raster_index", "chunk_index"),
  inventory   = c(
    "index", "chunk_index", "cohort_index", "cohort_proportion",
    "age", "area", "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type", "delay", "land_class"
  ),
  disturbance = c(
    "index", "chunk_index", "timestep",
    "disturbance_id", "disturbance_type", "default_disturbance_type_id", "proportion", "enable_merge", "sort_id", "filter_id",
    "undisturbed_transition_id", "disturbed_transition_id", "disturbance_order"
  ),
  simulation = c(
    "index", "chunk_index", "cohort_index", "cohort_proportion", "timestep",
    paste0("inventory.", c(
      "age", "area", "admin_boundary", "eco_boundary", "spatial_unit",
      "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type", "delay", "land_class",
      "cohort_proportion", "historic_disturbance_type_id", "last_pass_disturbance_type_id"
    )),
    paste0("state.", c(
      "last_disturbance_type", "last_disturbance_event", "time_since_last_disturbance", "time_since_land_class_change",
      "growth_enabled", "enabled", "land_class", "age", "growth_multiplier", "regeneration_delay"
    )),
    paste0("pools.", c(
      "Input", "SoftwoodMerch", "SoftwoodFoliage", "SoftwoodOther", "SoftwoodCoarseRoots", "SoftwoodFineRoots",
      "HardwoodMerch", "HardwoodFoliage", "HardwoodOther", "HardwoodCoarseRoots", "HardwoodFineRoots",
      "AboveGroundVeryFastSoil", "BelowGroundVeryFastSoil", "AboveGroundFastSoil", "BelowGroundFastSoil",
      "MediumSoil", "AboveGroundSlowSoil", "BelowGroundSlowSoil", "SoftwoodStemSnag", "SoftwoodBranchSnag",
      "HardwoodStemSnag", "HardwoodBranchSnag", "CO2", "CH4", "CO", "NO2", "Products"
    ))
  )
))

# grid_meta columns
utils::globalVariables(c(
  "pixel_index", "area"
))

