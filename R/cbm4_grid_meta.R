
#' CBM4 grid metadata
#'
#' Create a study area grid metadata table.
#'
#' @param admin_boundary character. Canada province or territory name.
#' @param eco_boundary character. Canada ecozone name.
#' @param eco_boundary_id integer. Canada ecozone ID. Provide this or `eco_boundary`.
#' @inheritParams cbm4_set_grid_meta
#'
#' @export
cbm4_grid_meta <- function(
    grid_rast,
    admin_boundary,
    eco_boundary    = NULL,
    eco_boundary_id = NULL,
    chunk_size      = NULL,
    chunk_meta      = NULL,
    def_afforestation_pre_type     = "None",
    def_historic_disturbance_type  = "Wildfire",
    def_last_pass_disturbance_type = "Wildfire",
    cbm_defaults_db = getOption("CBM4r.db.path"),
    ...){

  grid_meta <- data.table::data.table(
    pixel_index     = 1:terra::ncell(grid_rast),
    admin_boundary  = admin_boundary,
    eco_boundary    = eco_boundary,
    eco_boundary_id = eco_boundary_id
  )

  cbm4_set_grid_meta(
    grid_meta  = grid_meta,
    grid_rast  = grid_rast,
    chunk_size = chunk_size,
    chunk_meta = chunk_meta,
    def_afforestation_pre_type     = def_afforestation_pre_type,
    def_historic_disturbance_type  = def_historic_disturbance_type,
    def_last_pass_disturbance_type = def_last_pass_disturbance_type,
    cbm_defaults_db  = cbm_defaults_db,
    ...)
}
