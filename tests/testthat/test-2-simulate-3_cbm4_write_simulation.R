
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_w_dist  = readTestInputs("SK", set_grid_meta = TRUE, disturbances = TRUE),
  SK_wo_dist = readTestInputs("SK", set_grid_meta = TRUE, disturbances = FALSE)
)
for (test in names(projects)) projects[[test]]$template_name <- test
names(projects) <- paste0(names(projects), "_cbm4_write_simulation")

for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
}


## SIMULATE & TEST ----

for (project in projects) test_that(paste("cbm4_write_simulation:", project$test), {

  template_results <- cbm4_results_processor(file.path(testDirs$temp$outputs, project$template_name))

  # Initiate datasets
  cbm4_write_inventory(
    project$cbm4_data,
    grid_meta   = project$grid_meta,
    grid_rast   = project$grid_rast,
    classifiers = project$classifiers
  )
  cbm4_write_step_parameters(
    project$cbm4_data,
    gc_meta     = project$gc_meta,
    gc_incr     = project$gc_incr
  )
  cbm4_write_disturbance(
    project$cbm4_data,
    dist_meta   = project$dist_meta,
    dist_events = project$dist_events,
    grid_meta   = project$grid_meta
  )

  # Expect error: no pools data
  expect_error(
    cbm4_write_simulation(
      project$cbm4_data,
      cohorts   = project$cohorts,
      grid_meta = project$grid_meta
    )
  )

  cbm4_write_simulation(
    project$cbm4_data,
    cohorts   = cbm4_read_cohorts(template_results, timestep = 0),
    grid_meta = project$grid_meta
  )

  expect_true(file.exists(file.path(project$cbm4_data, "simulation")))

  cbm4_step(project$cbm4_data, timestep = 1)
  cbm4_step(project$cbm4_data, timestep = 2)

  cbm4_results <- cbm4_results_processor(project$cbm4_data)
  for (view_name in c("pool_indicators", "flux_indicators", "disturbance_indicators")){
    template_view <- cbm4_results_totals(template_results, view_name)
    expect_equal(
      cbm4_results_totals(cbm4_results, view_name)[, .SD, .SDcols = names(template_view)],
      template_view,
      tolerance = 0.000001
    )
  }
})



