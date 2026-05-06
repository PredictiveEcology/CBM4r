
#' CBM4 grid metadata
#'
#' Create a study area grid metadata table.
#'
#' @template grid_rast
#' @param admin_boundary character. TODO
#' @param eco_boundary character. TODO
#' @param eco_boundary_id integer. TODO
#' @param ... arguments to \code{\link{cbm4_set_grid_meta}}
#'
#' @export
cbm4_grid_meta <- function(
    grid_rast,
    admin_boundary  = NULL,
    eco_boundary    = NULL,
    eco_boundary_id = NULL,
    ...){

  grid_meta <- data.table::data.table(
    pixel_index     = 1:terra::ncell(grid_rast),
    admin_boundary  = admin_boundary,
    eco_boundary    = eco_boundary,
    eco_boundary_id = eco_boundary_id
  )

  cbm4_set_grid_meta(grid_meta = grid_meta, grid_rast = grid_rast, ...)
}
