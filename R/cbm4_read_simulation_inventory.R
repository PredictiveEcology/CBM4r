
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

  inventoryDT <- arrow::open_dataset(paths$pixels) |>
    dplyr::select(pixel_index, chunk_index, raster_index, area) |>
    dplyr::collect() |> data.table::as.data.table() |>
    merge(
      arrow::open_dataset(paths$index) |>
        dplyr::filter(timestep == !!timestep) |>
        dplyr::select(-timestep) |>
        dplyr::collect() |> data.table::as.data.table(),
      by = c("chunk_index", "raster_index")) |>
    merge(
      arrow::open_dataset(paths$flat) |>
        dplyr::filter(timestep == !!timestep) |>
        dplyr::select(-timestep) |>
        dplyr::collect() |> data.table::as.data.table(),
      by = c("index", "cohort_index", "chunk_index"))

  inventoryDT[, inventory.area := NULL]
  data.table::setnames(inventoryDT, "area", "inventory.area")

  data.table::setkey(inventoryDT, index, cohort_index, chunk_index, raster_index)
  data.table::setcolorder(inventoryDT, c(data.table::key(inventoryDT), "pixel_index", "inventory.area"))

  inventoryDT
}

