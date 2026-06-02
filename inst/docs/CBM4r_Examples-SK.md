CBM4r Example: Saskatchewan
================

``` r
library(CBM4r)
```

## Set inputs

``` r
# Set input test data directory
testdata <- system.file(file.path("testdata", "SK"), package = "CBM4r")

# Read study area grid
grid_rast <- do.call(terra::rast, data.table::fread(file.path(testdata, "grid_rast.csv")))

# Read cohort data and cohort classifier columns
cohorts     <- data.table::fread(file.path(testdata, "cohorts.csv"))
classifiers <- readLines(file.path(testdata, "classifiers.txt"))

# Read growth curve metadata and carbon increments
gc_meta <- data.table::fread(file.path(testdata, "gc_meta.csv"))
gc_incr <- data.table::fread(file.path(testdata, "gc_incr.csv"))

# Read disturbance metadata and events
dist_meta   <- data.table::fread(file.path(testdata, "dist_meta.csv"))
dist_events <- data.table::fread(file.path(testdata, "dist_events.csv"))
```

## Set up Python virtual environment

``` r
# Create CBM4 virtual environment
cbm4_virtualenv_create("r-CBM4")

# Use CBM4 virtual environment
reticulate::use_virtualenv("r-CBM4")
```

## Simulate with CBM4

### Set paths

``` r
# Set CBM4 output data directory path
cbm4_data <- "cbm4_data"

#' Optional: Set custom parameters by using a modified version of the CBM defaults RSQLite database.
#' This path can instead be passed directly to functions that use it.
#' Here the path is set again to the default.
cbm4_set_db_path(getOption("CBM4r.db.path"))
```

### Set study area metadata

``` r
# Set study area metadata
grid_meta <- cbm4_grid_meta(
  grid_rast       =  grid_rast,
  admin_boundary  = "Saskatchewan",
  eco_boundary_id = 6
)

# Alternatively: set grid metadata in an existing pixels table
grid_meta <- data.table::data.table(
  pixel_index     = 1:terra::ncell(grid_rast),
  admin_boundary  = "Saskatchewan",
  eco_boundary_id = 6
)
cbm4_set_grid_meta(grid_meta, grid_rast = grid_rast)
```

### Run spinup

``` r
# Write inventory to CBM4 data directory
cbm4_write_inventory(
  cbm4_data,
  grid_meta   = grid_meta,
  grid_rast   = grid_rast,
  cohorts     = cohorts,
  classifiers = classifiers
)

# Write spinup parameters to CBM4 data directory
cbm4_write_spinup_parameters(
  cbm4_data,
  gc_meta     = gc_meta,
  gc_incr     = gc_incr,
  classifiers = classifiers
)

# Run CBM4 spinup
cbm4_spinup(cbm4_data)
```

### Run annual steps

``` r
# Write annual step parameters to CBM4 data directory
cbm4_write_step_parameters(
  cbm4_data,
  gc_meta     = gc_meta,
  gc_incr     = gc_incr,
  classifiers = classifiers
)

# Write disturbances to CBM4 data directory
cbm4_write_disturbance(
  cbm4_data,
  dist_meta   = dist_meta,
  dist_events = dist_events
)

# Run annual step for year 1
cbm4_step(cbm4_data, timestep = 1)

# Run annual step for year 2
cbm4_step(cbm4_data, timestep = 2)
```

## Read results

All results functions can accept `cbm4_data` or `cbm4_results` as input.
Most functions will read `cbm4_data` into a `SQLResultsProcessor` object
as the first step, so calling `cbm4_results_processor` directly is more
efficient when reading more than one result.

``` r
# Get CBM4 SQLResultsProcessor
cbm4_results <- cbm4_results_processor(cbm4_data)
```

### Study area grid

``` r
# Get study area grid
cbm4_results_grid(cbm4_results)

# Get study area chunk processing key
cbm4_results_grid_key(cbm4_results)
```

### Results by timestep

``` r
# Read pools totals by timestep
cbm4_results_totals(
  cbm4_results,
  view_name = "composite_pool_indicators",
  units     = "t"
)
cbm4_results_totals(
  cbm4_results,
  view_name = "composite_pool_indicators",
  units     = "Mt"
)

# Read flux totals by timestep
cbm4_results_totals(
  cbm4_results,
  view_name = "composite_flux_indicators",
  units     = "t"
)
cbm4_results_totals(
  cbm4_results,
  view_name = "composite_flux_indicators",
  units     = "Mt"
)

# View a list of results that can be read into a summary table
totalOpts <- cbm4_results_totals(cbm4_results, list = TRUE)
```

### Results by pixel

``` r
# Read Net Primary Productivity (NPP) raster
rastNPP <- cbm4_results_raster(
  cbm4_results, 
  view_name   = "spatial_composite_flux_indicators",
  view_column = "Ecosystem Indicators - Productivity - Net Primary Productivity (NPP)",
  timesteps   = 1:2
)
terra::plot(
  rastNPP,
  main       = c("timestep 1: NPP (tC/ha)", "timestep 2: NPP (tC/ha)"),
  type       = "continuous",
  col        = colorRampPalette(c("lightgreen", "darkgreen"))(10),
  breaks     = seq(0, ceiling(max(terra::values(rastNPP), na.rm = TRUE)), length.out = 10),
  background = "darkgrey",
  axes       = FALSE
)

# View a list of results that can be read into a raster
rastOpts <- cbm4_results_raster(cbm4_results, list = TRUE)
```
