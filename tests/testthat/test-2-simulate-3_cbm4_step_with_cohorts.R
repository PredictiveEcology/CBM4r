
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(
  SK_w_dist  = readTestInputs("SK", set_grid_meta = TRUE, disturbances = TRUE),
  SK_wo_dist = readTestInputs("SK", set_grid_meta = TRUE, disturbances = FALSE)
)
for (test in names(projects)) projects[[test]]$template_name <- test
names(projects) <- paste0(names(projects), "_cbm4_step_with_cohorts")

for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
  cbm4_data_copy(
    file.path(testDirs$temp$outputs, projects[[test]]$template_name), projects[[test]]$cbm4_data,
    dataset_names = c("inventory", "disturbance", "step_parameters", "simulation")
  )
}


## SIMULATE & TEST ----

for (project in projects) test_that(paste("cbm4_step_with_cohorts: no change in cohorts:", project$test), {

  template_results <- cbm4_results_processor(file.path(testDirs$temp$outputs, project$template_name))

  for (timestep in 1:2){

    # Read cohort data from previous timestep
    cohorts <- cbm4_read_cohorts(
      project$cbm4_data,
      timestep = timestep - 1
    )

    # Expect error: stand attributes required
    expect_error(
      cbm4_step_with_cohorts(
        project$cbm4_data,
        cohorts   = cohorts,
        timestep  = timestep
      )
    )

    # Step with cohort data: with grid_meta
    cbm4_step_with_cohorts(
      project$cbm4_data,
      cohorts   = cohorts,
      grid_meta = project$grid_meta,
      timestep  = timestep
    )

    cbm4_results <- cbm4_results_processor(project$cbm4_data)
    for (view_name in c("pool_indicators", "flux_indicators", "disturbance_indicators")){
      expect_equal(
        cbm4_results_totals(cbm4_results,     view_name, timesteps = timestep),
        cbm4_results_totals(template_results, view_name, timesteps = timestep),
        tolerance = 0.000001
      )
    }
  }
})

for (project in projects[2]) test_that(paste("cbm4_step_with_cohorts: 1/2 biomass:", project$test), {

  # Set timestep
  timestep <- 2

  # Read template results
  template_results <- cbm4_results_processor(file.path(testDirs$temp$outputs, project$template_name))
  template_pools   <- cbm4_results_totals(template_results, "pool_indicators", timesteps = timestep)

  # Read cohort data from previous timestep
  cohorts <- cbm4_read_cohorts(
    project$cbm4_data,
    timestep = timestep - 1
  )

  # Step with cohort data:
  # - without grid_meta
  # - 1/2 biomass
  poolCols <- intersect(names(cohorts), paste0("pools.", names(template_pools)))
  cohortsHalf <- cbind(
    cohorts[, .SD, .SDcols = setdiff(names(cohorts), poolCols)],
    cohorts[, .SD, .SDcols = poolCols] / 2
  )
  cohortsHalf <- merge(cohortsHalf, project$grid_meta, by = c("pixel_index", "raster_index", "chunk_index"))

  cbm4_step_with_cohorts(
    project$cbm4_data,
    cohorts  = cohortsHalf,
    timestep = timestep
  )

  # Expect less carbon in pools after the step
  cbm4_results <- cbm4_results_processor(project$cbm4_data)
  expect_true(all(
    cbm4_results_totals(cbm4_results, "pool_indicators", timesteps = timestep)[, -1] <
      template_pools[, -1]
  ))
})

