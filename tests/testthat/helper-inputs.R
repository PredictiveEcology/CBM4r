
readTestInputs <- function(testName, disturbances = TRUE){

  testdata <- system.file(file.path("testdata", testName), package = "CBM4r")

  testInputs <- list(
    grid_meta   = data.table::fread(file.path(testdata, "grid_meta.csv")),
    grid_rast   = do.call(terra::rast, data.table::fread(file.path(testdata, "grid_rast.csv"))),
    cohorts     = data.table::fread(file.path(testdata, "cohorts.csv")),
    classifiers = readLines(file.path(testdata, "classifiers.txt")),
    gc_meta     = data.table::fread(file.path(testdata, "gc_meta.csv")),
    gc_incr     = data.table::fread(file.path(testdata, "gc_incr.csv"))
  )

  if (disturbances){
    testInputs <- c(testInputs, list(
      dist_meta   = data.table::fread(file.path(testdata, "dist_meta.csv")),
      dist_events = data.table::fread(file.path(testdata, "dist_events.csv"))
    ))
  }

  return(testInputs)
}

