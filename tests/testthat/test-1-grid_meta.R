
if (!testthat::is_testing()) source(testthat::test_path("setup.R"))

test_that("cbm4_grid_meta and cbm4_set_grid_meta", {

  grid_rast <- terra::rast(
    xmin = 0, xmax = 8,
    ymin = 0, ymax = 8,
    res = 1,
    crs = "local"
  )

  # cbm4_grid_meta
  grid_meta <- cbm4_grid_meta(
    grid_rast       = grid_rast,
    admin_boundary  = "Saskatchewan",
    eco_boundary_id = 6,
    chunk_size      = NULL
  )

  expect_equal(grid_meta$pixel_index,  1:terra::ncell(grid_rast))
  expect_equal(grid_meta$chunk_index,  rep(0, terra::ncell(grid_rast)))
  expect_equal(grid_meta$raster_index, (1:terra::ncell(grid_rast)) - 1)
  expect_equal(grid_meta$area,         rep(1, terra::ncell(grid_rast)))
  expect_true(all(c(
    "admin_boundary", "eco_boundary", "spatial_unit",
    "afforestation_pre_type", "historic_disturbance_type", "last_pass_disturbance_type"
  ) %in% names(grid_meta)))

  # cbm4_set_grid_meta
  expect_equal({

    grid_meta_set <- grid_meta[, .(pixel_index, admin_boundary, eco_boundary_id)]

    cbm4_set_grid_meta(
      grid_meta  = grid_meta_set,
      grid_rast  = grid_rast,
      chunk_size = NULL
    )

    grid_meta_set

  }, grid_meta)

  # cbm4_grid_meta with chunk_size
  grid_meta_chunks <- cbm4_grid_meta(
    grid_rast       = grid_rast,
    admin_boundary  = "Saskatchewan",
    eco_boundary_id = 6,
    chunk_size      = 2
  )
  expect_equal(grid_meta[,        .SD, .SDcols = setdiff(names(grid_meta), c("chunk_index", "raster_index"))],
               grid_meta_chunks[, .SD, .SDcols = setdiff(names(grid_meta), c("chunk_index", "raster_index"))])

  expect_equal(grid_meta_chunks$chunk_index, c(
     0,  0,  1,  1,  2,  2,  3,  3,
     0,  0,  1,  1,  2,  2,  3,  3,
     4,  4,  5,  5,  6,  6,  7,  7,
     4,  4,  5,  5,  6,  6,  7,  7,
     8,  8,  9,  9, 10, 10, 11, 11,
     8,  8,  9,  9, 10, 10, 11, 11,
    12, 12, 13, 13, 14, 14, 15, 15,
    12, 12, 13, 13, 14, 14, 15, 15
  ))
  expect_equal(grid_meta_chunks$raster_index, c(
    0, 1, 0, 1, 0, 1, 0, 1,
    2, 3, 2, 3, 2, 3, 2, 3,
    0, 1, 0, 1, 0, 1, 0, 1,
    2, 3, 2, 3, 2, 3, 2, 3,
    0, 1, 0, 1, 0, 1, 0, 1,
    2, 3, 2, 3, 2, 3, 2, 3,
    0, 1, 0, 1, 0, 1, 0, 1,
    2, 3, 2, 3, 2, 3, 2, 3
  ))
})

test_that("cbm4_set_grid_meta with admin_abbrev", {

  grid_meta <- data.table::data.table(
    pixel_index   = 1,
    admin_abbrev  = "BC",
    eco_boundary  = "Pacific Maritime",
    area          = 1
  )
  cbm4_set_grid_meta(grid_meta)
  expect_equal(as.character(grid_meta$admin_boundary), "British Columbia")

  # admin_boundary cannot be set with admin_abbrev = \"NL\"
  grid_meta <- data.table::data.table(
    pixel_index     = 1,
    admin_abbrev    = "NL",
    eco_boundary_id = 17,
    area            = 1
  )
  expect_error(cbm4_set_grid_meta(grid_meta), "admin_abbrev")

})


