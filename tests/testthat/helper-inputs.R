
testInputs_SK <- function(){

  list(

    grid_meta = data.table::data.table(
      pixel_index = 1:4,
      area = 1,
      admin_boundary  = "Saskatchewan",
      eco_boundary_id = 6
    ),
    grid_rast = terra::rast(ncol = 2, nrow = 2, xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "local"),

    classifiers = c("species", "prodClass"),

    gc_meta = cbind(rbind(
      data.table::data.table(
        gcID = 1,
        species = "species1", prodClass = "P",
        sw = TRUE
      ),
      data.table::data.table(
        gcID = 2,
        species = "species2", prodClass = "M",
        sw = FALSE
      )
    )),
    gc_incr = rbind(
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
        species = "species1", prodClass = "P",
        age = 100
      ),
      data.table::data.table(
        pixel_index = 3,
        species = "species1", prodClass = "P",
        age = 100
      ),
      data.table::data.table(
        pixel_index = 4,
        species = "species2", prodClass = "M",
        age = 50
      )
    ),

    dist_meta = rbind(
      data.table::data.table(
        disturbance_id = 1,
        disturbance_type = "Wildfire"
      ),
      data.table::data.table(
        disturbance_id = 2,
        disturbance_type = "Clearcut harvesting without salvage"
      )
    ),
    dist_events = rbind(
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
