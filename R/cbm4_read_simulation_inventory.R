
#' CBM4 read simulation inventory
#'
#' Read inventory from a simulation CBM4 spatial parquet dataset.
#'
#' @template cbm4_data
#' @template timestep
#' @template dataset_name
#' @template dataset_path
#'
#' @return `data.table`
#' @export
cbm4_read_simulation_inventory <- function(
    cbm4_data = NULL,
    timestep,
    dataset_name = "simulation",
    dataset_path = file.path(cbm4_data, dataset_name)
){

  paths <- list(
    pixels = file.path(dataset_path, paste0(dataset_name, "-table-pixels")),
    index  = file.path(dataset_path, paste0(dataset_name, "-raster_index")),
    flat   = file.path(dataset_path, dataset_name)
  )

  cohortDT <- merge(
    arrow::open_dataset(paths$pixels) |>
      dplyr::select(pixel_index, chunk_index, raster_index) |>
      dplyr::collect() |> data.table::as.data.table(),
    arrow::open_dataset(paths$index) |>
      dplyr::filter(timestep == !!timestep) |>
      dplyr::select(-timestep) |>
      dplyr::collect() |> data.table::as.data.table(),
    by = c("chunk_index", "raster_index"))[, .(index, pixel_index)] |>
    merge(
      arrow::open_dataset(paths$flat) |>
        dplyr::filter(timestep == !!timestep) |>
        dplyr::select(-timestep) |>
        dplyr::collect() |> data.table::as.data.table(),
      by = "index")

  cohortDT[, c("index", "cohort_index", "chunk_index") := NULL]

  data.table::setcolorder(cohortDT, unique(c(
    names(cohortDT)[!grepl("^(classifiers|inventory|state|pools)\\.", names(cohortDT))],
    names(cohortDT)[grepl("^classifiers\\.", names(cohortDT))],
    names(cohortDT)[grepl("^inventory\\.", names(cohortDT))],
    names(cohortDT)[grepl("^state\\.", names(cohortDT))],
    names(cohortDT)[grepl("^pools\\.", names(cohortDT))]
  )))

  cohortDT[, names(cohortDT)[grepl("^inventory\\.", names(cohortDT))] := NULL]

  classifierCols <- names(cohortDT)[grepl("^classifiers\\.", names(cohortDT))]
  data.table::setnames(cohortDT, classifierCols, gsub("^classifiers\\.", "", classifierCols))

  stateCols <- names(cohortDT)[grepl("^state\\.", names(cohortDT))]
  data.table::setnames(cohortDT, stateCols, gsub("^state\\.", "", stateCols))

  cohortDT
}

