
readTestInputs <- function(testName, disturbances = TRUE, set_grid_meta = FALSE){

  testdata <- system.file(file.path("testdata", testName), package = "CBM4r")

  list(
    grid_meta   = data.table::fread(file.path(testdata, ifelse(set_grid_meta, "grid_meta_set.csv", "grid_meta.csv"))),
    grid_rast   = do.call(terra::rast, data.table::fread(file.path(testdata, "grid_rast.csv"))),
    cohorts     = data.table::fread(file.path(testdata, "cohorts.csv")),
    classifiers = readLines(file.path(testdata, "classifiers.txt")),
    gc_meta     = data.table::fread(file.path(testdata, "gc_meta.csv")),
    gc_incr     = data.table::fread(file.path(testdata, "gc_incr.csv")),
    dist_meta   = if (disturbances) data.table::fread(file.path(testdata, "dist_meta.csv")),
    dist_events = if (disturbances) data.table::fread(file.path(testdata, "dist_events.csv"))
  )
}

