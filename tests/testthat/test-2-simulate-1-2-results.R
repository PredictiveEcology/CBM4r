
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_spinup  = readTestInputs("SK", set_grid_meta = TRUE),
  SK_w_dist  = readTestInputs("SK", set_grid_meta = TRUE, disturbances = TRUE),
  SK_wo_dist = readTestInputs("SK", set_grid_meta = TRUE, disturbances = FALSE)
)
for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
}

## RESULTS ----

for (test in names(projects)){
  projects[[test]]$cbm4_results <- tryCatch(cbm4_results_processor(projects[[test]]$cbm4_data), error = function(e) NULL)
  test_that(paste("cbm4_results_processor:", projects[[test]]$test), {
    expect_s3_class(projects[[test]]$cbm4_results, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")
  })
}

for (project in projects) test_that(paste("cbm4_results_grid:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  expect_true(terra::compareGeom(cbm4_results_grid(cbm4_data),    project$grid_rast))
  expect_true(terra::compareGeom(cbm4_results_grid(cbm4_results), project$grid_rast))

})

for (project in projects) test_that(paste("cbm4_results_grid_key:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  grid_key <- cbm4_results_grid_key(cbm4_results)

  expect_equal(names(grid_key), c("pixel_index", "chunk_index", "raster_index"))
  expect_equal(grid_key$pixel_index,  unique(project$cohorts$pixel_index))
  expect_equal(grid_key$raster_index, unique(project$cohorts$pixel_index) - 1)

  expect_equal(grid_key, cbm4_results_grid_key(cbm4_data))

  grid_key_crds <- cbm4_results_grid_key(cbm4_results, coords = TRUE)
  expect_equal(names(grid_key_crds), c("pixel_index", "chunk_index", "raster_index", "x", "y"))
})

for (project in projects[1]) test_that(paste("cbm4_results_raster with list = TRUE:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  # Check listing options
  expect_s3_class(cbm4_results_raster(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_raster(cbm4_results, list = TRUE), "data.table")
})

for (project in projects) test_that(paste("cbm4_results_raster without data:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  # Check creating empty grid
  expect_true(terra::compareGeom(cbm4_results_raster(cbm4_data),    project$grid_rast))
  expect_true(terra::compareGeom(cbm4_results_raster(cbm4_results), project$grid_rast))
})

for (project in projects[2:3]) test_that(paste("cbm4_results_raster:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

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
        project$cbm4_data,
        view_name   = view_name,
        view_column = rast_view_columns[[view_name]],
        timesteps   = 1
      )
    ))
  }
})

for (project in projects[1]) test_that(paste("cbm4_results_totals with list = TRUE:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  expect_s3_class(cbm4_results_totals(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_totals(cbm4_results, list = TRUE), "data.table")
})

for (project in projects[2:3]) test_that(paste("cbm4_results_totals:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

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
      tolerance = 0.000001)

    expect_equal(
      cbm4_results_totals(cbm4_results, view_name = view_name, units = "t/ha")[, -c("timestep", "area")],
      resTotals[, -c("timestep")] / ((project$grid_meta$area[[1]] / 10000) * length(unique(project$cohorts$pixel_index))),
      tolerance = 0.000001)

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
      project$cbm4_data,
      view_name    = view_name,
      view_columns = total_view_columns[[view_name]],
      timesteps    = 1:2
    ))
  }
})
