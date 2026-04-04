
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  `SK with disturbances` = testInputs_SK()
)
projects$`SK without disturbances` <- projects$`SK`
projects$`SK without disturbances`[c("distMeta", "distEvents")] <- NULL

for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
}


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_write_inventory:", project$test), {

  cbm4_data <- project$cbm4_data

  cbm4_write_inventory(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    cohortDT        = project$cohortDT,
    classifiers     = project$classifiers,
    grid_rast       = project$grid_rast,
    grid_meta       = project$grid_meta
  )

  expect_true(file.exists(file.path(cbm4_data, "inventory")))

})

for (project in projects) test_that(paste("cbm4_write_disturbance:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_disturbance(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    distMeta        = project$distMeta,
    distEvents      = project$distEvents
  )

  expect_true(file.exists(file.path(cbm4_data, "disturbance")))

})

for (project in projects) test_that(paste("cbm4_write_spinup_parameters:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_spinup_parameters(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    classifiers     = project$classifiers,
    gcMeta          = project$gcMeta,
    gcIncr          = project$gcIncr
  )

  expect_true(file.exists(file.path(cbm4_data, "spinup_parameters")))

})

for (project in projects) test_that(paste("cbm4_write_step_parameters:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_write_step_parameters(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    classifiers     = project$classifiers,
    gcMeta          = project$gcMeta,
    gcIncr          = project$gcIncr
  )

  expect_true(file.exists(file.path(cbm4_data, "step_parameters")))

})

for (project in projects) test_that(paste("cbm4_spinup:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "inventory")))

  cbm4_spinup(cbm4_data, cbm_defaults_db = cbm_defaults_db)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=0")))

})

for (project in projects) test_that(paste("cbm4_step:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4_step(cbm4_data, cbm_defaults_db = cbm_defaults_db, timestep = 1)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=1")))

})

for (project in projects) test_that(paste("cbm4_read_simulation_inventory, cbm4_write_simulation_inventory:", project$test), {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4_write_simulation_inventory(
    cbm4_data,
    cohortDT = cbm4_read_simulation_inventory(cbm4_data, timestep = 1),
    timestep = 1)

  cbm4_step(cbm4_data, cbm_defaults_db = cbm_defaults_db, timestep = 2)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=2")))

})


## READ RESULTS ----

test_that("cbm4_results_processor", {

  cbm4_data <- project$cbm4_data
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  results_processor <- cbm4_results_processor(cbm4_data)

  expect_s3_class(results_processor, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")

})

for (test in names(projects)) if (file.exists(file.path(projects[[test]]$cbm4_data, "simulation"))){
  projects[[test]]$cbm4_results <- cbm4_results_processor(projects[[test]]$cbm4_data)
  projects[[test]]$grid_area    <- terra::cellSize(projects[[test]]$grid_rast, unit = "ha")[1, 1][[1]]
}

for (project in projects) test_that(paste("cbm4_read_geo:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  expect_true(terra::compareGeom(project$grid_rast, cbm4_read_geo(cbm4_data)))
  expect_true(terra::compareGeom(project$grid_rast, cbm4_read_geo(cbm4_results)))

})

for (project in projects) test_that(paste("cbm4_results_pools_by_timestep:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_pools_by_timestep(cbm4_data, units)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "timestep")
    expect_equal(cbm4Summary[[units]]$timestep, 0:2)

    cbm4SumSubset <- cbm4_results_pools_by_timestep(cbm4_results, units, timesteps = 1)

    expect_s3_class(cbm4SumSubset, "data.table")
    expect_equal(data.table::key(cbm4SumSubset), "timestep")
    expect_equal(cbm4SumSubset$timestep, 1)
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_pools_by_timestep(cbm4_results, "t"),
    ignore_attr = TRUE)
})

for (project in projects) test_that(paste("cbm4_results_flux_by_timestep:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_flux_by_timestep(cbm4_data, units)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "timestep")
    expect_equal(cbm4Summary[[units]]$timestep, 1:2)

    cbm4SumSubset <- cbm4_results_flux_by_timestep(cbm4_results, units, timesteps = 1)

    expect_s3_class(cbm4SumSubset, "data.table")
    expect_equal(data.table::key(cbm4SumSubset), "timestep")
    expect_equal(cbm4SumSubset$timestep, 1L)
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_flux_by_timestep(cbm4_results, "t"),
    ignore_attr = TRUE)
})

for (project in projects) test_that(paste("cbm4_results_emissions_by_timestep:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_emissions_by_timestep(cbm4_data, units)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "timestep")
    expect_equal(cbm4Summary[[units]]$timestep, 1:2)

    cbm4SumSubset <- cbm4_results_emissions_by_timestep(cbm4_results, units, timesteps = 1)

    expect_s3_class(cbm4SumSubset, "data.table")
    expect_equal(data.table::key(cbm4SumSubset), "timestep")
    expect_equal(cbm4SumSubset$timestep, 1L)
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_emissions_by_timestep(cbm4_results, "t"),
    ignore_attr = TRUE)
})

for (project in projects) test_that(paste("cbm4_results_pools_by_pixel:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t/ha", "t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_pools_by_pixel(cbm4_results, units, timestep = 1)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "pixel_index")
    expect_equal(cbm4Summary[[units]]$pixel_index, c(1, 3, 4))
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)
  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["t/ha"]][, -1] * grid_area, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_pools_by_pixel(cbm4_results, "t", timestep = 1),
    ignore_attr = TRUE)
})

for (project in projects) test_that(paste("cbm4_results_flux_by_pixel:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t/ha", "t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_flux_by_pixel(cbm4_results, units, timestep = 1)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "pixel_index")
    expect_equal(cbm4Summary[[units]]$pixel_index, c(1, 3, 4))
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)
  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["t/ha"]][, -1] * grid_area, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_flux_by_pixel(cbm4_results, "t", timestep = 1),
    ignore_attr = TRUE)
})

for (project in projects) test_that(paste("cbm4_results_emissions_by_pixel:", project$test), {

  cbm4_data    <- project$cbm4_data
  cbm4_results <- project$cbm4_results
  grid_area    <- project$grid_area
  testthat::skip_if(!file.exists(file.path(cbm4_data, "simulation")))

  cbm4Summary <- list()
  for (units in c("t/ha", "t", "Mt")){

    cbm4Summary[[units]] <- cbm4_results_emissions_by_pixel(cbm4_results, units, timestep = 1)

    expect_s3_class(cbm4Summary[[units]], "data.table")
    expect_equal(data.table::key(cbm4Summary[[units]]), "pixel_index")
    expect_equal(cbm4Summary[[units]]$pixel_index, c(1, 3, 4))
  }

  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["Mt"]][, -1] * 10^6, ignore_attr = TRUE)
  expect_equal(cbm4Summary[["t"]][, -1], cbm4Summary[["t/ha"]][, -1] * grid_area, ignore_attr = TRUE)

  expect_equal(
    cbm4Summary[["t"]],
    cbm4_results_emissions_by_pixel(cbm4_results, "t", timestep = 1),
    ignore_attr = TRUE)
})




