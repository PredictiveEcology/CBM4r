
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_wo_dist_gc_class_partial  = {
    project <- readTestInputs("SK", disturbances = FALSE)
    project$gc_meta[, prodClass := NULL]
    project
  },
  SK_wo_dist_gc_class_contains_NA  = {
    project <- readTestInputs("SK", disturbances = FALSE)
    project$gc_meta[species == "species1", prodClass := NA]
    project
  }
)
for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
  cbm4_data_copy(file.path(testDirs$temp$outputs, "SK_wo_dist"), projects[[test]]$cbm4_data,
                 dataset_names = c("inventory", "disturbance"))
}

template_results <- cbm4_results_processor(file.path(testDirs$temp$outputs, "SK_wo_dist"))


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_write_spinup_parameters:", project$test), {

  cbm4_write_spinup_parameters(
    project$cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr,
    classifiers = project$classifiers
  )

  expect_true(file.exists(file.path(project$cbm4_data, "spinup_parameters")))

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

for (project in projects) test_that(paste("cbm4_spinup:", project$test), {

  cbm4_spinup(project$cbm4_data)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=0")))

})

for (project in projects) test_that(paste("cbm4_step:", project$test), {

  cbm4_step(project$cbm4_data, timestep = 1)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=1")))

})


## RESULTS ----

for (project in projects) test_that(paste("Increment classifiers can be partially specified:", project$test), {

  cbm4_results <- cbm4_results_processor(project$cbm4_data)

  view_names <- cbm4_results_totals(template_results, list = TRUE)$name
  for (view_name in view_names){
    expect_equal(
      cbm4_results_totals(cbm4_results,     view_name, timesteps = 1),
      cbm4_results_totals(template_results, view_name, timesteps = 1),
      tolerance = 0.000001
    )
  }
})

