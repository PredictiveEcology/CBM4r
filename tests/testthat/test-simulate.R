
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

cbm4_data <- file.path(testDirs$temp$outputs, "simulate")
unlink(cbm4_data, recursive = TRUE)

grid_rast <- terra::rast(ncol = 2, nrow = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "local")
grid_meta <- data.table::data.table(
  pixel_index = 1:4,
  area = 1,
  admin_boundary = "Saskatchewan",
  eco_boundary_id = 6
)

classifiers <- c("species", "prodClass")

gcMeta <- rbind(
  data.table::data.table(
    gcID = 1,
    species = "Something",
    prodClass = "P",
    sw = TRUE
  ),
  data.table::data.table(
    gcID = 2,
    species = "Something else",
    prodClass = "M",
    sw = FALSE
  )
)
gcMeta[, admin_boundary  := "Saskatchewan"]
gcMeta[, eco_boundary_id := 6]

gcIncr <- rbind(
  data.table::data.table(
    gcID        = 1,
    age         = 0:150,
    merch_inc   = seq(0, 1, length.out = 151),
    foliage_inc = seq(0, 1, length.out = 151),
    other_inc   = seq(0, 1, length.out = 151)
  ),
  data.table::data.table(
    gcID        = 2,
    age         = 0:150,
    merch_inc   = seq(0, 1, length.out = 151),
    foliage_inc = seq(0, 1, length.out = 151),
    other_inc   = seq(0, 1, length.out = 151)
  )
)

cohortDT <- rbind(
  data.table::data.table(
    pixel_index = 1,
    species = "Something",
    prodClass = "P",
    age = 100
  ),
  data.table::data.table(
    pixel_index = 3,
    species = "Something",
    prodClass = "P",
    age = 50
  ),
  data.table::data.table(
    pixel_index = 4,
    species = "Something else",
    prodClass = "M",
    age = 100
  )
)

distMeta <- rbind(
  data.table::data.table(
    disturbance_id = 1,
    disturbance_type = "Wildfire"
  ),
  data.table::data.table(
    disturbance_id = 2,
    disturbance_type = "Clearcut harvest without salvage"
  )
)
distEvents <- rbind(
  data.table::data.table(
    pixel_index = 3,
    disturbance_id = 1,
    timestep = 1
  ),
  data.table::data.table(
    pixel_index = 4,
    disturbance_id = 2,
    timestep = 2
  )
)


## SIMULATE ----

test_that("cbm4_write_inventory", {

  cbm4_write_inventory(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    cohortDT = cohortDT,
    classifiers = classifiers,
    grid_rast = grid_rast,
    grid_meta = grid_meta
  )

  expect_true(file.exists(file.path(cbm4_data, "inventory")))

})

test_that("cbm4_write_disturbance", {

  cbm4_write_disturbance(
    cbm4_data, template_name = "inventory",
    cbm_defaults_db = cbm_defaults_db,
    distMeta   = distMeta,
    distEvents = distEvents
  )

  expect_true(file.exists(file.path(cbm4_data, "disturbance")))

})

test_that("cbm4_write_spinup_parameters", {

  cbm4_write_spinup_parameters(
    cbm4_data, template_name = "inventory",
    cbm_defaults_db = cbm_defaults_db,
    classifiers = classifiers,
    gcMeta = gcMeta,
    gcIncr = gcIncr
  )

  expect_true(file.exists(file.path(cbm4_data, "spinup_parameters")))

})

test_that("cbm4_write_step_parameters", {

  cbm4_write_step_parameters(
    cbm4_data, template_name = "inventory",
    cbm_defaults_db = cbm_defaults_db,
    classifiers = classifiers,
    gcMeta = gcMeta,
    gcIncr = gcIncr
  )

  expect_true(file.exists(file.path(cbm4_data, "step_parameters")))

})

test_that("cbm4_spinup", {

  cbm4_spinup(cbm4_data, cbm_defaults_db = cbm_defaults_db)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=0")))

})

test_that("cbm4_step", {

  cbm4_step(cbm4_data, cbm_defaults_db = cbm_defaults_db, timestep = 1)

  expect_true(file.exists(file.path(cbm4_data, "simulation", "simulation", "timestep=1")))

})


## READ RESULTS: BY TIMESTEP ----

cbm4_results <- tryCatch(cbm4_results_processor(cbm4_data), error = function(e) NULL)

test_that("cbm4_results_processor", {

  expect_s3_class(cbm4_results, "cbm4.app.spatial.results.sql_results_processor.SQLResultsProcessor")

})

test_that("cbm4_results_flux_by_timestep", {

  cbm4Summary <- cbm4_results_flux_by_timestep(cbm4_data)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 1)

  expect_equal(cbm4Summary, cbm4_results_flux_by_timestep(cbm4_results), ignore_attr = TRUE)

  cbm4Summary <- cbm4_results_flux_by_timestep(cbm4_data, timesteps = 2)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, integer(0))

})

test_that("cbm4_results_pools_by_timestep", {

  cbm4Summary <- cbm4_results_pools_by_timestep(cbm4_data)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 0:1)

  expect_equal(cbm4Summary, cbm4_results_pools_by_timestep(cbm4_results), ignore_attr = TRUE)

  cbm4Summary <- cbm4_results_pools_by_timestep(cbm4_results, timesteps = 1)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 1)

})

test_that("cbm4_results_products_by_timestep", {

  cbm4Summary <- cbm4_results_products_by_timestep(cbm4_data)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 0:1)

  expect_equal(cbm4Summary, cbm4_results_products_by_timestep(cbm4_results), ignore_attr = TRUE)

  cbm4Summary <- cbm4_results_products_by_timestep(cbm4_results, timesteps = 1)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 1)

})

test_that("cbm4_results_emissions_by_timestep", {

  cbm4Summary <- cbm4_results_emissions_by_timestep(cbm4_data)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, 1)

  expect_equal(cbm4Summary, cbm4_results_emissions_by_timestep(cbm4_results), ignore_attr = TRUE)

  cbm4Summary <- cbm4_results_emissions_by_timestep(cbm4_results, timesteps = 2)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "timestep")
  expect_equal(cbm4Summary$timestep, integer(0))

})


## READ RESULTS: BY PIXEL ----

test_that("cbm4_results_flux_by_pixel", {

  cbm4Summary <- cbm4_results_flux_by_pixel(cbm4_data, timestep = 1)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "pixel_index")
  expect_equal(cbm4Summary$pixel_index, c(1, 3, 4))

  expect_equal(cbm4Summary, cbm4_results_flux_by_pixel(cbm4_results, timestep = 1), ignore_attr = TRUE)

})

test_that("cbm4_results_pools_by_pixel", {

  cbm4Summary <- cbm4_results_pools_by_pixel(cbm4_data, timestep = 1)

  expect_s3_class(cbm4Summary, "data.table")
  expect_equal(data.table::key(cbm4Summary), "pixel_index")
  expect_equal(cbm4Summary$pixel_index, c(1, 3, 4))

  expect_equal(cbm4Summary, cbm4_results_pools_by_pixel(cbm4_results, timestep = 1), ignore_attr = TRUE)

})

