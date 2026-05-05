
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list()
projects[["SK_w_disturbances"]]  <- testInputs_SK()
projects[["SK_wo_disturbances"]] <- {
  inputs <- testInputs_SK()
  inputs[c("dist_meta", "dist_events")] <- NULL
  inputs
}

for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
}


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_set_grid_meta:", project$test), {

  cbm4_set_grid_meta(project$grid_meta, project$grid_rast)

  expect_true("chunk_index"  %in% names(project$grid_meta))
  expect_true("raster_index" %in% names(project$grid_meta))
  expect_true("area"         %in% names(project$grid_meta))
  expect_true("spatial_unit" %in% names(project$grid_meta))

})

for (project in projects) test_that(paste("cbm4_write_inventory:", project$test), {

  cbm4_data <- project$cbm4_data

  cbm4_write_inventory(
    cbm4_data,
    grid_meta   = project$grid_meta,
    grid_rast   = project$grid_rast,
    cohortDT    = project$cohortDT,
    classifiers = project$classifiers,
    chunk_size  = 2
  )

  expect_true(file.exists(file.path(cbm4_data, "inventory")))

})

for (project in projects) test_that(paste("cbm4_write_disturbance:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_disturbance(
    cbm4_data,
    grid_meta   = project$grid_meta,
    dist_meta   = project$dist_meta,
    dist_events = project$dist_events
  )

  expect_true(file.exists(file.path(cbm4_data, "disturbance")))

})

for (project in projects) test_that(paste("cbm4_write_spinup_parameters:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_spinup_parameters(
    cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(cbm4_data, "spinup_parameters")))

})

for (project in projects) test_that(paste("cbm4_write_step_parameters:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_step_parameters(
    cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(cbm4_data, "step_parameters")))

})

for (project in projects) test_that(paste("cbm4_spinup:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_spinup(cbm4_data)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=0")))

})

for (project in projects) test_that(paste("cbm4_step:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=0")))

  cbm4_step(cbm4_data, timestep = 1)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=1")))

})

for (project in projects) test_that(paste("cbm4_read_simulation_inventory, cbm4_write_simulation_inventory:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=1")))

  cohortDT <- cbm4_read_simulation_inventory(
    cbm4_data,
    grid_meta = project$grid_meta,
    timestep  = 1
  )

  cbm4_write_simulation_inventory(
    cbm4_data,
    grid_meta = project$grid_meta,
    cohortDT  = cohortDT,
    timestep  = 1
  )

  cbm4_step(cbm4_data, timestep = 2)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=2")))

})


## READ RESULTS ----

for (test in names(projects)){

  cbm4_data <- projects[[test]]$cbm4_data

  if (file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=2"))){

    projects[[test]]$cbm4_results <- tryCatch(cbm4_results_processor(cbm4_data), error = function(e) NULL)
    projects[[test]]$grid_area    <- terra::cellSize(projects[[test]]$grid_rast, unit = "ha")[1, 1][[1]]
  }
}

for (project in projects) test_that(paste("cbm4_results_processor:", project$test), {

  expect_s3_class(project$cbm4_results, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")

})

for (project in projects) test_that(paste("cbm4_results_raster:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  # Check listing options
  expect_s3_class(cbm4_results_raster(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_raster(cbm4_results, list = TRUE), "data.table")

  # Check creating empty grid
  expect_true(terra::compareGeom(cbm4_results_raster(cbm4_data),  project$grid_rast))
  expect_true(terra::compareGeom(cbm4_results_grid(cbm4_results), project$grid_rast))

  # Check setting values from views
  rast_view_columns <- list(
    "spatial_pool_indicators"                  = "SoftwoodMerch",
    "spatial_composite_pool_indicators"        = "Softwood Merchantable",
    "spatial_flux_indicators"                  = "DecayDOMCO2Emission",
    "spatial_composite_flux_indicators"        = "Emissions - Emissions By Gas - Total CO2",
    "spatial_disturbance_indicators"           = "DisturbanceSoftProduction",
    "spatial_composite_disturbance_indicators" = "Ecosystem Transfers - Ecosystem to Forest Products - Total Harvest (Biomass + Snags)"
  )

  for (view_name in names(rast_view_columns)){

    resRast <- cbm4_results_raster(
      cbm4_results,
      view_name   = view_name,
      view_column = rast_view_columns[[view_name]],
      timesteps   = 1
    )

    expect_true(terra::compareGeom(resRast, project$grid_rast))
    expect_equal(names(resRast), "1")

    # Check using cbm4_data
    expect_equal(terra::values(resRast), terra::values(
      cbm4_results_raster(
        cbm4_data,
        view_name   = view_name,
        view_column = rast_view_columns[[view_name]],
        timesteps   = 1
      )
    ))
  }
})

for (project in projects) test_that(paste("cbm4_results_totals:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  expect_s3_class(cbm4_results_totals(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_totals(cbm4_results, list = TRUE), "data.table")

  total_view_columns <- list(
    "pool_indicators"                  = "SoftwoodMerch",
    "composite_pool_indicators"        = "Softwood Merchantable",
    "flux_indicators"                  = "DecayDOMCO2Emission",
    "composite_flux_indicators"        = "Emissions - Emissions By Gas - Total CO2",
    "disturbance_indicators"           = "DisturbanceSoftProduction",
    "composite_disturbance_indicators" = "Ecosystem Transfers - Ecosystem to Forest Products - Total Harvest (Biomass + Snags)"
  )
  total_view_timesteps <- list(
    "pool_indicators"                  = 0:2,
    "composite_pool_indicators"        = 0:2,
    "flux_indicators"                  = 1:2,
    "composite_flux_indicators"        = 1:2,
    "disturbance_indicators"           = if (!is.null(project$dist_events)) 1:2 else integer(),
    "composite_disturbance_indicators" = if (!is.null(project$dist_events)) 1:2 else integer()
  )

  for (view_name in names(total_view_columns)){

    resTotals <- cbm4_results_totals(cbm4_results, view_name = view_name)

    expect_s3_class(resTotals, "data.frame")
    expect_gt(ncol(resTotals), 2)
    expect_equal(resTotals$timestep, total_view_timesteps[[view_name]])

    # Check setting units
    expect_equal(
      cbm4_results_totals(cbm4_results, view_name = view_name, units = "Mt")[, -c("timestep")],
      resTotals[, -c("timestep")] / 10^6,
      ignore_attr = TRUE, tolerance = 0.000001)

    expect_equal(
      cbm4_results_totals(cbm4_results, view_name = view_name, units = "t/ha")[, -c("timestep", "area")],
      resTotals[, -c("timestep")] / (project$grid_area * length(unique(project$cohortDT$pixel_index))),
      ignore_attr = TRUE, tolerance = 0.000001)

    # Check subsetting columns and timesteps
    resTotals <- cbm4_results_totals(
      cbm4_results,
      view_name    = view_name,
      view_columns = total_view_columns[[view_name]],
      timesteps    = 1:2
    )

    expect_s3_class(resTotals, "data.frame")
    expect_equal(names(resTotals), c("timestep", total_view_columns[[view_name]]))
    expect_equal(resTotals$timestep, intersect(total_view_timesteps[[view_name]], 1:2))

    # Check using cbm4_data
    expect_equal(resTotals, cbm4_results_totals(
      cbm4_data,
      view_name    = view_name,
      view_columns = total_view_columns[[view_name]],
      timesteps    = 1:2
    ), ignore_attr = TRUE)
  }
})



