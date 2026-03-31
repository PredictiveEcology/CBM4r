
#' CBM4 results: emissions by pixel
#' @inherit .cbm4_results_by_pixel params return
#' @export
cbm4_results_emissions_by_pixel <- function(cbm4_results, units, timestep){

  emiss <- .cbm4_results_by_pixel(cbm4_results, "emissions_annual_process", units, timestep)

  if ("disturbance_flux" %in% cbm4_results_query(cbm4_results, "SHOW TABLES")$name){
    key <- data.table::key(emiss)
    emiss <- rbind(
      emiss, .cbm4_results_by_pixel(cbm4_results, "emissions_disturbance", units, timestep))[
        , lapply(.SD, sum), by = key]
    data.table::setkeyv(emiss, key)
  }

  return(emiss)
}

#' CBM4 results: flux by pixel
#' @inherit .cbm4_results_by_pixel params return
#' @export
cbm4_results_flux_by_pixel <- function(cbm4_results, units, timestep){

  flux <- .cbm4_results_by_pixel(cbm4_results, "flux_annual_process", units, timestep)

  if ("disturbance_flux" %in% cbm4_results_query(cbm4_results, "SHOW TABLES")$name){
    flux <- merge(
      flux,
      .cbm4_results_by_pixel(cbm4_results, "flux_disturbance", units, timestep),
      by = data.table::key(flux), all = TRUE)
    flux[is.na(flux)] <- 0
  }

  return(flux)
}

#' CBM4 results: pools by pixel
#' @inherit .cbm4_results_by_pixel params return
#' @export
cbm4_results_pools_by_pixel <- function(cbm4_results, units, timestep){
  .cbm4_results_by_pixel(cbm4_results, "pools", units, timestep)
}


#' CBM4 results: by pixel
#' @template units
#' @template timestep
#' @inherit .cbm4_results_query_preset params return
#' @keywords internal
.cbm4_results_by_pixel <- function(cbm4_results, type, units, timestep = NULL){

  unitOpts <- c("t/ha", "t", "Mt")
  if (!units %in% unitOpts) stop(
    "invalid unit ", shQuote(units),
    ". Choose from: ", paste(shQuote(unitOpts), collapse = ", "))

  # Query
  cbm4_results <- cbm4_results_processor(cbm4_results)
  cbm4Summary <- .cbm4_results_query_preset(
    cbm4_results,
    type = type, units = ifelse(units == "Mt", "t", units), by = "pixel",
    where = if (!is.null(timestep)) paste(
      "a.timestep IN (", paste(timestep, collapse = ", "), ")", "AND",
      "b.timestep IN (", paste(timestep, collapse = ", "), ")")
  )
  if (length(timestep) == 1) cbm4Summary[, timestep := NULL]

  # Merge with pixels table
  results_dataset <- reticulate::py_get_attr(cbm4_results, "_results_dataset")$simulation_dataset
  if ("pixels" %in% results_dataset$list_tables()){

    cbm4Summary <- merge(
      cbm4Summary,
      dplyr::collect(
        results_dataset$read_table_pyarrow(
          "pixels", read_cols = c("pixel_index", "raster_index", "chunk_index"))
      ),
      by = c("raster_index", "chunk_index"))
    cbm4Summary[, c("raster_index", "chunk_index") := NULL]
  }

  # Set key
  data.table::setkeyv(cbm4Summary, intersect(
    c("timestep", "pixel_index", "raster_index", "chunk_index"), names(cbm4Summary)))
  data.table::setcolorder(cbm4Summary)

  # Convert
  if (units == "Mt"){
    cols <- setdiff(names(cbm4Summary), data.table::key(cbm4Summary))
    cbm4Summary[, (cols) := lapply(.SD, function(x) x / 10^6), .SDcols = cols]
  }

  return(cbm4Summary)
}


