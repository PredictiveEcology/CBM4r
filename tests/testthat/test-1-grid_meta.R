
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

## SET UP ----

# Set projects
projects <- list(
  SK = readTestInputs("SK")
)
for (test in names(projects)){
  projects[[test]]$test <- test
  projects[[test]]$cbm4_data <- file.path(testDirs$temp$outputs, test)
  unlink(projects[[test]]$cbm4_data, recursive = TRUE)
  projects[[test]]$grid_area <- terra::cellSize(projects[[test]]$grid_rast, unit = "m")[1, 1][[1]]
}


## SIMULATE ----

for (project in projects) test_that(paste("cbm4_grid_meta:", project$test), {

  grid_meta <- cbm4_grid_meta(
    grid_rast       = project$grid_rast,
    admin_boundary  = project$grid_meta$admin_boundary,
    eco_boundary_id = project$grid_meta$eco_boundary_id,
    chunk_meta      = NULL,
    chunk_size      = NULL
  )

  expect_equal(grid_meta$pixel_index,  1:4)
  expect_equal(grid_meta$chunk_index,  rep(0, 4))
  expect_equal(grid_meta$raster_index, 0:3)
  expect_equal(grid_meta$area,         rep(project$grid_area, 4))
  expect_true(all(c(
    "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ) %in% names(grid_meta)))

  grid_meta_chunks <- cbm4_grid_meta(
    grid_rast       = project$grid_rast,
    admin_boundary  = project$grid_meta$admin_boundary,
    eco_boundary_id = project$grid_meta$eco_boundary_id,
    chunk_meta      = NULL,
    chunk_size      = 2
  )
  expect_equal(grid_meta[,        .SD, .SDcols = setdiff(names(grid_meta), "chunk_index")],
               grid_meta_chunks[, .SD, .SDcols = setdiff(names(grid_meta), "chunk_index")])
  expect_equal(grid_meta_chunks$chunk_index, c(0, 0, 1, 1))

  grid_meta_chunks <- cbm4_grid_meta(
    grid_rast       = project$grid_rast,
    admin_boundary  = project$grid_meta$admin_boundary,
    eco_boundary_id = project$grid_meta$eco_boundary_id,
    chunk_meta      = project$cohorts,
    chunk_size      = 1
  )
  expect_equal(grid_meta[,        .SD, .SDcols = setdiff(names(grid_meta), "chunk_index")],
               grid_meta_chunks[, .SD, .SDcols = setdiff(names(grid_meta), "chunk_index")])
  expect_equal(grid_meta_chunks$chunk_index, c(0, NA, 0, 1))

})

for (project in projects) test_that(paste("cbm4_set_grid_meta:", project$test), {

  cbm4_set_grid_meta(
    project$grid_meta,
    project$grid_rast,
    chunk_meta = project$cohorts,
    chunk_size = 1
  )

  grid_meta <- project$grid_meta
  expect_equal(grid_meta$pixel_index,  1:4)
  expect_equal(grid_meta$chunk_index, c(0, NA, 0, 1))
  expect_equal(grid_meta$raster_index, 0:3)
  expect_equal(grid_meta$area,         rep(project$grid_area, 4))
  expect_true(all(c(
    "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ) %in% names(grid_meta)))
})



