
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_w_dist  = readTestInputs("SK", set_grid_meta = TRUE, disturbances = TRUE),
  SK_wo_dist = readTestInputs("SK", set_grid_meta = TRUE, disturbances = FALSE)
)
for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
  cbm4_data_copy(file.path(testDirs$temp$outputs, "SK_spinup"), projects[[test]]$cbm4_data)
}


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_write_disturbance:", project$test), {

  cbm4_write_disturbance(
    project$cbm4_data,
    dist_meta   = project$dist_meta,
    dist_events = project$dist_events,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(project$cbm4_data, "disturbance")))

})

for (project in projects) test_that(paste("cbm4_write_step_parameters:", project$test), {

  cbm4_write_step_parameters(
    project$cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(project$cbm4_data, "step_parameters")))

})

for (project in projects) test_that(paste("cbm4_step:", project$test), {

  cbm4_step(project$cbm4_data, timestep = 1)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=1")))

  cbm4_step(project$cbm4_data, timestep = 2)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=2")))

})

