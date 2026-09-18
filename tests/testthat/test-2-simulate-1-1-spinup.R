
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_spinup = readTestInputs("SK", set_grid_meta = TRUE)
)
for (test in names(projects)){
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
}


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_write_inventory:", project$test), {

  cbm4_write_inventory(
    project$cbm4_data,
    grid_meta   = project$grid_meta,
    grid_rast   = project$grid_rast,
    cohorts     = project$cohorts,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(project$cbm4_data, "inventory")))

})

for (project in projects) test_that(paste("cbm4_write_spinup_parameters:", project$test), {

  cbm4_write_spinup_parameters(
    project$cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(project$cbm4_data, "spinup_parameters")))

  # Check that increments can be written without gc_meta
  gc_incr_merge <- merge(project$gc_meta, project$gc_incr, by = "gc_id")
  gc_incr_merge[, gc_id := NULL]
  dataset_path_merge <- file.path(project$cbm4_data, "spinup_parameters_merge")
  withr::defer(unlink(dataset_path_merge, recursive = TRUE))
  cbm4_write_spinup_parameters(
    project$cbm4_data,
    dataset_path = file.path(project$cbm4_data, "spinup_parameters_merge"),
    gc_incr      = gc_incr_merge,
    classifiers  = project$classifiers
  )
  expect_equal_dir(dataset_path_merge, file.path(project$cbm4_data, "spinup_parameters"))

})

for (project in projects) test_that(paste("cbm4_spinup:", project$test), {

  cbm4_spinup(project$cbm4_data)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=0")))

})



