
testInputs_SK <- function(){

  admin_boundary  <- "Saskatchewan"
  eco_boundary_id <- 6

  list(
    grid_rast = terra::rast(ncol = 2, nrow = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "local"),
    grid_meta = data.table::data.table(
      pixel_index = 1:4,
      area = 1,
      admin_boundary = admin_boundary,
      eco_boundary_id = eco_boundary_id
    ),

    classifiers = c("species", "prodClass"),

    gcMeta = cbind(rbind(
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
    ), admin_boundary = admin_boundary, eco_boundary_id = eco_boundary_id),

    gcIncr = rbind(
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
    ),

    cohortDT = rbind(
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
    ),

    distMeta = rbind(
      data.table::data.table(
        disturbance_id = 1,
        disturbance_type = "Wildfire"
      ),
      data.table::data.table(
        disturbance_id = 2,
        disturbance_type = "Clearcut harvest without salvage"
      )
    ),
    distEvents = rbind(
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
  )
}
