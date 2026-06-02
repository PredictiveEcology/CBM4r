
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

projects <- list(

  # Clearcut occurs in all pixels when timestep==2 but only affects species=="species2"
  SK_w_dist_dist_partial_species = {
    project <- readTestInputs("SK", disturbances = TRUE)
    project$dist_meta$species <- NA_character_
    project$dist_meta[disturbance_id == 2, species := "species2"]
    project$dist_events <- rbind(project$dist_events, data.table::data.table(
      pixel_index = 1:3, disturbance_id = 2, timestep = 2
    ))
    project
  },

  # Clearcut occurs in all pixels when timestep==2 but only affects prodClass=="M"
  SK_w_dist_dist_partial_prodClass = {
    project <- readTestInputs("SK", disturbances = TRUE)
    project$dist_meta$species   <- NA_character_
    project$dist_meta$prodClass <- NA_character_
    project$dist_meta[disturbance_id == 2, prodClass := "M"]
    project$dist_events <- rbind(project$dist_events, data.table::data.table(
      pixel_index = 1:3, disturbance_id = 2, timestep = 2
    ))
    project
  }
)
for (test in names(projects)){
  projects[[test]]$test      <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
}

# Copy data
template_data <- file.path(testDirs$temp$outputs, "SK_w_dist")
if (!file.exists(template_data)) stop("Run test-simulate-1")
for (project in projects) cbm4_data_copy(
  template_data, project$cbm4_data,
  dataset_names = c("inventory", "step_parameters", "simulation")
)


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

for (project in projects) test_that(paste("cbm4_step:", project$test), {

  cbm4_step(project$cbm4_data, timestep = 1)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=1")))

  cbm4_step(project$cbm4_data, timestep = 2)

  expect_true(file.exists(file.path(project$cbm4_data, "simulation", "simulation", "timestep=2")))

})


## RESULTS ----

template_results <- cbm4_results_processor(template_data)

for (project in projects) test_that(paste("Disturbance filters can exclude cohorts:", project$test), {

  cbm4_results <- cbm4_results_processor(project$cbm4_data)

  # No softwood harvested
  expect_equal(
    sum(cbm4_results_totals(cbm4_results, "disturbance_indicators", "DisturbanceSoftProduction")$DisturbanceSoftProduction),
    0)

  for (view_name in c("pool_indicators", "flux_indicators", "disturbance_indicators")){
    expect_equal(
      cbm4_results_totals(cbm4_results,     view_name, timesteps = 1:2),
      cbm4_results_totals(template_results, view_name, timesteps = 1:2),
      ignore_attr = TRUE
    )
  }
})



