
#' CBM4 read cohorts
#'
#' Read cohorts from a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_results
#' @template timestep
#' @template grid_meta_optional
#'
#' @return `data.table`
#' @export
cbm4_read_cohorts <- function(cbm4_results, timestep, grid_meta = NULL){

  if (is.null(grid_meta)) grid_meta <- cbm4_results_grid_key(cbm4_results)

  if (is.character(cbm4_results)){

    colNames <- names(arrow::schema(arrow::open_dataset(file.path(cbm4_results, "simulation/simulation"))))

    joinKey <- c("index", "chunk_index", "cohort_index")

    cbm4_inv <- arrow::open_dataset(file.path(cbm4_results, "simulation/simulation")) |>
      dplyr::filter(timestep == !!timestep) |>
      dplyr::select(dplyr::all_of(c(
        joinKey, "cohort_proportion",
        colNames[grepl("^(classifiers|state|pools)\\.", colNames)]
      ))) |>
      dplyr::collect() |> data.table::as.data.table()

    cbm4_inv <- cbm4_inv[
      arrow::open_dataset(file.path(cbm4_results, "simulation/simulation-raster_index")) |>
        dplyr::filter(timestep == !!timestep) |>
        dplyr::select(dplyr::all_of(c(joinKey, "raster_index"))) |>
        dplyr::collect() |> data.table::as.data.table(),
      on = joinKey]
    cbm4_inv[, index := NULL]

  }else{

    colNames <- cbm4_results$get_columns("simulation")

    cbm4_inv <- cbm4_results_query(cbm4_results, c(
      "SELECT",
      "b.chunk_index, a.raster_index, b.cohort_index, b.cohort_proportion,",
      sprintf("b.\"%1s\",", colNames[grepl("^(classifiers|state|pools)\\.", colNames)]),
      "FROM raster_index a LEFT JOIN simulation b",
      "ON a.index = b.index AND a.timestep = b.timestep",
      "AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index",
      "WHERE a.timestep =", timestep,
        "AND b.timestep =", timestep
    ))
  }

  # Set pixel index
  cbm4_inv[grid_meta, pixel_index := pixel_index, on = c("raster_index", "chunk_index")]
  data.table::setkey(cbm4_inv, pixel_index, chunk_index, raster_index, cohort_index)

  data.table::setcolorder(cbm4_inv, c(
    data.table::key(cbm4_inv), "cohort_proportion",
    names(cbm4_inv)[grepl("^classifiers\\.", names(cbm4_inv))],
    "state.age"
  ))

  # Rename columns
  data.table::setnames(cbm4_inv, names(cbm4_inv), gsub("^classifiers\\.", "", names(cbm4_inv)))

  return(cbm4_inv)
}


