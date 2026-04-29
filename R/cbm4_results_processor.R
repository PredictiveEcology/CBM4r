
#' CBM4 results processor
#'
#' @template cbm4_data
#' @return `SQLResultsProcessor`
#' @export
cbm4_results_processor <- function(cbm4_data){

  if (inherits(cbm4_data, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")){
    return(cbm4_data)
  }

  if (length(cbm4_data) == 0)  stop("cbm4_data invalid")
  if (!file.exists(cbm4_data)) stop("cbm4_data not found: ", cbm4_data)
  cbm4_data <- normalizePath(cbm4_data, winslash = "/", mustWork = FALSE)

  simulation_dataset_set_disturbance_schema(cbm4_data)

  reticulate::import(
    "cbm4.app.spatial.results.sql_results_processor"
  )$SQLResultsProcessor$for_simulation(cbm4_data)
}

# If simulation data does not contain disturbances:
# add empty tables to the dataset to prevent issues reading results views.
simulation_dataset_set_disturbance_schema <- function(cbm4_data){

  simulation_dataset <- file.path(cbm4_data, "simulation")

  if (!file.exists(file.path(simulation_dataset, "simulation-table-disturbance_flux"))){

    simSchema <- arrow::schema(arrow::open_dataset(file.path(cbm4_data, "simulation", "simulation")))
    distFluxCols <- c(
      "DisturbanceCO2Production", "DisturbanceCH4Production", "DisturbanceCOProduction",
      "DisturbanceBioCO2Emission", "DisturbanceBioCH4Emission", "DisturbanceBioCOEmission",
      "DisturbanceSoftProduction", "DisturbanceHardProduction", "DisturbanceDOMProduction",
      "DisturbanceMerchToAir", "DisturbanceFolToAir", "DisturbanceOthToAir",
      "DisturbanceCoarseToAir", "DisturbanceFineToAir",
      "DisturbanceDOMCO2Emission", "DisturbanceDOMCH4Emission", "DisturbanceDOMCOEmission",
      "DisturbanceMerchLitterInput", "DisturbanceFolLitterInput", "DisturbanceOthLitterInput",
      "DisturbanceCoarseLitterInput", "DisturbanceFineLitterInput",
      "DisturbanceVFastAGToAir", "DisturbanceVFastBGToAir",
      "DisturbanceFastAGToAir", "DisturbanceFastBGToAir", "DisturbanceMediumToAir",
      "DisturbanceSlowAGToAir", "DisturbanceSlowBGToAir",
      "DisturbanceSWStemSnagToAir", "DisturbanceSWBranchSnagToAir",
      "DisturbanceHWStemSnagToAir", "DisturbanceHWBranchSnagToAir"
    )

    pqPath <- file.path(simulation_dataset, "simulation-table-disturbance_flux", "timestep=0", "chunk_index=0", "0.parquet")
    dir.create(dirname(pqPath), recursive = TRUE)
    arrow::write_parquet(
      arrow::arrow_table(schema = arrow::unify_schemas(
        arrow::schema(
          cohort_proportion     = arrow::float(),
          disturbance_type      = arrow::int32(),
          disturbance_type_name = arrow::string(),
          disturbance_number    = arrow::int32(),
          disturbance_id        = arrow::int32(),
          index                 = arrow::int32(),
          area                  = arrow::float()
        ),
        simSchema[grepl("^(classifiers|state)\\.", names(simSchema))],
        {
          sh <- simSchema[grepl("^(classifiers|state)\\.", names(simSchema))]
          names(sh) <- paste0("post_", names(sh))
          sh
        },
        {
          sh <- lapply(distFluxCols, function(x) arrow::float())
          names(sh) <- distFluxCols
          arrow::schema(sh)
        }
      )),
      pqPath)

    pqPath <- file.path(simulation_dataset, "simulation-table-partitions-disturbance_flux", "0.parquet")
    dir.create(dirname(pqPath), recursive = TRUE)
    arrow::write_parquet(
      data.table::data.table(partition_name = c("timestep", "chunk_index"), partition_type = c("int32", "int64")),
      pqPath)
  }

  if (!file.exists(file.path(simulation_dataset, "simulation-table-disturbance_raster_index"))){

    pqPath <- file.path(simulation_dataset, "simulation-table-disturbance_raster_index", "timestep=0", "chunk_index=0", "0.parquet")
    dir.create(dirname(pqPath), recursive = TRUE)
    arrow::write_parquet(
      arrow::arrow_table(schema = arrow::schema(
        raster_index = arrow::int64(),
        index        = arrow::int32()
      )),
      pqPath)

    pqPath <- file.path(simulation_dataset, "simulation-table-partitions-disturbance_raster_index", "0.parquet")
    dir.create(dirname(pqPath), recursive = TRUE)
    arrow::write_parquet(
      data.table::data.table(partition_name = c("timestep", "chunk_index"), partition_type = c("int32", "int64")),
      pqPath)
  }
}

