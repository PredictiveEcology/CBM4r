
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

cbm4_data <- file.path(testDirs$temp$outputs, "simulate")

grid_rast <- terra::rast(ncol = 2, nrow = 2, xmin = 0, ymin = 0, crs = "local")

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

inventoryDT <- rbind(
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
inventoryDT[, area := 1]
inventoryDT[, admin_boundary  := "Saskatchewan"]
inventoryDT[, eco_boundary_id := 6]

distMeta <- rbind(
  data.table::data.table(
    eventID = 1,
    disturbance_type = "Wildfire"
  ),
  data.table::data.table(
    eventID = 2,
    disturbance_type = "Clearcut harvest without salvage"
  )
)
distEvents <- rbind(
  data.table::data.table(
    pixel_index = 3,
    eventID = 1,
    timestep = 1
  ),
  data.table::data.table(
    pixel_index = 4,
    eventID = 2,
    timestep = 2
  )
)


## SIMULATE ----

test_that("cbm4_write_inventory", {

  cbm4_write_inventory(
    cbm4_data,
    cbm_defaults_db = cbm_defaults_db,
    inventoryDT = inventoryDT,
    classifiers = classifiers,
    grid_rast   = grid_rast
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


## READ RESULTS ----

test_that("cbm4_results_emissions_by_timestep", {

  cbm4_summary <- cbm4_results_emissions_by_timestep(cbm4_data)

  expect_s3_class(cbm4_summary, "data.table")

})

test_that("cbm4_results_products_by_timestep", {

  cbm4_summary <- cbm4_results_products_by_timestep(cbm4_data)

  expect_s3_class(cbm4_summary, "data.table")

})

