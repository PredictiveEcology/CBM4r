
#' @importFrom data.table ':='
NULL

# data.table package common variables
utils::globalVariables(c(".", ":=", ".BY", ".N", ".SD", ".GRP"))

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

# CBM4 dataset 'pixels' table columns
utils::globalVariables(c(
  "pixel_index", "x", "y", "area"
))


