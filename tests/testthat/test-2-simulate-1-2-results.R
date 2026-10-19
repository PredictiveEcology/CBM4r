
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
  projects[[test]]$cbm4_results <- tryCatch(SQLResultsProcessor(projects[[test]]$cbm4_data), error = function(e) NULL)
  test_that(paste("SQLResultsProcessor:", projects[[test]]$test), {
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

for (project in projects[1]) test_that(paste("cbm4_results_cohort with list = TRUE:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  # Check listing options
  expect_s3_class(cbm4_results_cohort(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_cohort(cbm4_results, list = TRUE), "data.table")
})

for (project in projects[2:3]) test_that(paste("cbm4_results_cohort:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  cohort_views <- cbm4_results_cohort(cbm4_results, list = TRUE)
  for (view in cohort_views$name){

    resByCohort <- cbm4_results_cohort(cbm4_results, view = view)

    expect_s3_class(resByCohort, "data.frame")
    expect_equal(data.table::key(resByCohort), c("timestep", "pixel_index", "species", "prodClass"))
    expect_gt(ncol(resByCohort), 4)

    # Check using cbm4_data
    expect_equal(cbm4_results_cohort(cbm4_results, view = view), resByCohort)

    # Check subset by timestep
    if (nrow(resByCohort) > 0){
      expect_all_equal(cbm4_results_cohort(
        cbm4_results,
        view      = view,
        timesteps = 1
      )$timestep, 1)
    }

    # Check setting units
    pools <- .cbm4_results_view_pool_columns(cbm4_results, view)

    expect_equal(
      cbm4_results_cohort(cbm4_results, view = view, units = "t")[, .SD, .SDcols = pools],
      resByCohort[, .SD, .SDcols = pools] * (project$grid_meta$area[[1]] / 10000),
      tolerance = 0.000001)

    expect_equal(
      cbm4_results_cohort(cbm4_results, view = view, units = "Mt")[, .SD, .SDcols = pools],
      resByCohort[, .SD, .SDcols = pools] * (project$grid_meta$area[[1]] / 10000) / 10^6,
      tolerance = 0.000001)

    # Check selecting columns
    select <- sample(pools, 2)

    resByCohortSelect <- cbm4_results_cohort(
      cbm4_results,
      view   = view,
      select = select
    )
    expect_equal(names(resByCohortSelect),
                 c("timestep", "pixel_index", "species", "prodClass", "age", select))

    resByCohortSelectRename <- cbm4_results_cohort(
      cbm4_results,
      view   = view,
      select = setNames(select, c("colA", "colB"))
    )
    expect_equal(names(resByCohortSelectRename),
                 c("timestep", "pixel_index", "species", "prodClass", "age", "colA", "colB"))

    expect_equal(resByCohortSelect, resByCohortSelectRename, ignore_attr = TRUE)

  }
})

for (project in projects[1]) test_that(paste("cbm4_results_pixel with list = TRUE:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  # Check listing options
  expect_s3_class(cbm4_results_pixel(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_pixel(cbm4_results, list = TRUE), "data.table")
})

for (project in projects[2:3]) test_that(paste("cbm4_results_pixel:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  pixel_views <- cbm4_results_pixel(cbm4_results, list = TRUE)
  for (view in pixel_views$name){

    resByPixel <- cbm4_results_pixel(
      cbm4_results,
      view = view
    )

    expect_s3_class(resByPixel, "data.frame")
    expect_equal(data.table::key(resByPixel), c("timestep", "pixel_index"))
    expect_gt(ncol(resByPixel), 2)

    # Check using cbm4_data
    expect_equal(cbm4_results_pixel(cbm4_results, view = view), resByPixel)

    # Check subset by timestep
    if (nrow(resByPixel) > 0){
      expect_all_equal(cbm4_results_pixel(
        cbm4_results,
        view      = view,
        timesteps = 1
      )$timestep, 1)
    }

    # Check setting units
    pools <- .cbm4_results_view_pool_columns(cbm4_results, view)

    expect_equal(
      cbm4_results_pixel(cbm4_results, view = view, units = "t")[, .SD, .SDcols = pools],
      resByPixel[, .SD, .SDcols = pools] * (project$grid_meta$area[[1]] / 10000),
      tolerance = 0.000001)

    expect_equal(
      cbm4_results_pixel(cbm4_results, view = view, units = "Mt")[, .SD, .SDcols = pools],
      resByPixel[, .SD, .SDcols = pools] * (project$grid_meta$area[[1]] / 10000) / 10^6,
      tolerance = 0.000001)

    # Check selecting columns
    select <- sample(pools, 2)

    resByPixelSelect <- cbm4_results_pixel(
      cbm4_results,
      view   = view,
      select = select
    )
    expect_equal(names(resByPixelSelect), c("timestep", "pixel_index", select))

    resByPixelSelectRename <- cbm4_results_pixel(
      cbm4_results,
      view   = view,
      select = setNames(select, c("colA", "colB"))
    )
    expect_equal(names(resByPixelSelectRename), c("timestep", "pixel_index", "colA", "colB"))

    expect_equal(resByPixelSelect, resByPixelSelectRename, ignore_attr = TRUE)
  }
})

for (project in projects[2:3]) test_that(paste("cbm4_results_pixel with rast = TRUE:", project$test), {

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

  for (view in names(rast_view_columns)){

    resRast <- cbm4_results_pixel(
      cbm4_results,
      view      = view,
      select    = rast_view_columns[[view]],
      timesteps = 1,
      rast      = TRUE
    )

    expect_true(terra::compareGeom(resRast, project$grid_rast))
    expect_equal(names(resRast), "1")
  }
})

for (project in projects[1]) test_that(paste("cbm4_results_timestep with list = TRUE:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(is.null(cbm4_results))

  expect_s3_class(cbm4_results_timestep(cbm4_data,    list = TRUE), "data.table")
  expect_s3_class(cbm4_results_timestep(cbm4_results, list = TRUE), "data.table")
})

for (project in projects[2:3]) test_that(paste("cbm4_results_timestep:", project$test), {

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

  for (view in names(total_view_columns)){

    resByT <- cbm4_results_timestep(cbm4_results, view = view)

    expect_s3_class(resByT, "data.frame")
    expect_equal(data.table::key(resByT), "timestep")
    expect_gt(ncol(resByT), 2)
    expect_equal(resByT$timestep, total_view_timesteps[[view]])

    # Check using cbm4_data
    expect_equal(cbm4_results_timestep(project$cbm4_data, view = view), resByT)

    # Check subset by timestep
    expect_equal(
      cbm4_results_timestep(
        cbm4_results,
        view      = view,
        timesteps = 1:2)$timestep,
      intersect(total_view_timesteps[[view]], 1:2)
    )

    # Check setting units
    expect_equal(
      cbm4_results_timestep(cbm4_results, view = view, units = "Mt")[, -c("timestep")],
      resByT[, -c("timestep")] / 10^6,
      tolerance = 0.000001)

    expect_equal(
      cbm4_results_timestep(cbm4_results, view = view, units = "t/ha")[, -c("timestep", "area")],
      resByT[, -c("timestep")] / ((project$grid_meta$area[[1]] / 10000) * length(unique(project$cohorts$pixel_index))),
      tolerance = 0.000001)

    # Check selecting columns
    pools <- .cbm4_results_view_pool_columns(cbm4_results, view)
    select <- sample(pools, 2)

    resByTSelect <- cbm4_results_timestep(
      cbm4_results,
      view   = view,
      select = select
    )
    expect_equal(names(resByTSelect), c("timestep", select))

    resByTSelectRename <- cbm4_results_timestep(
      cbm4_results,
      view   = view,
      select = setNames(select, c("colA", "colB"))
    )
    expect_equal(names(resByTSelectRename), c("timestep", "colA", "colB"))

    expect_equal(resByTSelect, resByTSelectRename, ignore_attr = TRUE)
  }
})

