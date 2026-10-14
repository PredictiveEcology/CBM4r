    library(CBM4r)

## Getting started

### Initialize the CBM4 virtual environment

    # Use the CBM4 virtual environment
    # See `vignette("Getting-started", package = "CBM4r")`
    reticulate::use_virtualenv("r-CBM4")

### Set CBM4 simulation data source

    # Set CBM4 output data directory path
    cbm4_data <- "cbm4_data"

All results functions can accept `cbm4_data` or `cbm4_results` as input.
Most functions will read `cbm4_data` into a `SQLResultsProcessor` object
as the first step, so calling `SQLResultsProcessor` directly is more
efficient when reading multiple results.

    # Get CBM4 SQLResultsProcessor
    cbm4_results <- SQLResultsProcessor(cbm4_data)

## Read results

See the [CBM4 SQL Results Processor
documentation](https://github.com/cat-cfs/tech_docs/blob/main/dev/cbm4/results/sql_results_processor.md)
for more information about the `SQLResultsProcessor`.

### Direct query the SQL Results Processor

    # See list of available views to query
    data.table::as.data.table(cbm4_results$views)[, c("name", "column_names")]

    # Example: get cohort pool totals in the first and last timesteps
    cbm4_results_query(cbm4_results, "SELECT * FROM spatial_pool_indicators WHERE timestep == 1 ORDER BY raster_index")
    cbm4_results_query(cbm4_results, "SELECT * FROM spatial_pool_indicators WHERE timestep == (SELECT MAX(timestep) FROM simulation) ORDER BY raster_index")

    # Example: get cohort pool summaries in the first and last timesteps
    cbm4_results_query(cbm4_results, "SELECT * FROM spatial_composite_pool_indicators WHERE timestep == 1 ORDER BY raster_index")
    cbm4_results_query(cbm4_results, "SELECT * FROM spatial_composite_pool_indicators WHERE timestep == (SELECT MAX(timestep) FROM simulation) ORDER BY raster_index")

### Read SQL Results Processor views

    # See list of available views to query
    cbm4_results_view(cbm4_results, list = TRUE)

    # Example: get cohort pool totals in the first timestep
    cbm4_results_view(
      cbm4_results,
      view      = "spatial_pool_indicators",
      timesteps = 1
    )

    # Example: get cohort pool summaries in the first timestep
    cbm4_results_view(
      cbm4_results,
      view      = "spatial_composite_pool_indicators",
      timesteps = 1
    )

### Results by timestep

    # See list of views that can be read into a timestep summary
    cbm4_results_timestep(cbm4_results, list = TRUE)

    # Read pools totals by timestep
    cbm4_results_timestep(
      cbm4_results,
      view      = "composite_pool_indicators",
      units     = "t"
    )
    cbm4_results_timestep(
      cbm4_results,
      view      = "composite_pool_indicators",
      units     = "Mt"
    )

    # Read flux totals by timestep
    cbm4_results_timestep(
      cbm4_results,
      view      = "composite_flux_indicators",
      units     = "t"
    )
    cbm4_results_timestep(
      cbm4_results,
      view      = "composite_flux_indicators",
      units     = "Mt"
    )

### Results by cohort

    # See list of views with that can be read into a cohort summary
    cbm4_results_cohort(cbm4_results, list = TRUE)

    # Read pools totals by cohort
    cbm4_results_cohort(
      cbm4_results,
      view = "spatial_pool_indicators"
    )

    # Read pools summaries by cohort
    cbm4_results_cohort(
      cbm4_results,
      view = "spatial_composite_pool_indicators"
    )

    # Read flux summaries by cohort
    cbm4_results_cohort(
      cbm4_results,
      view = "spatial_composite_flux_indicators"
    )

### Results by pixel

    # See list of views with that can be read into a pixel summary
    cbm4_results_pixel(cbm4_results, list = TRUE)

    # Read pools totals by cohort
    cbm4_results_pixel(
      cbm4_results,
      view = "spatial_pool_indicators"
    )

    # Read pools summaries by pixel
    cbm4_results_pixel(
      cbm4_results,
      view = "spatial_composite_pool_indicators"
    )

    # Read flux summaries by pixel
    cbm4_results_pixel(
      cbm4_results,
      view = "spatial_composite_flux_indicators"
    )

Pixel results can be read as a terra `SpatRaster`.

    # Read Net Primary Productivity (NPP) raster
    rastNPP <- cbm4_results_pixel(
      cbm4_results, 
      view      = "spatial_composite_flux_indicators",
      select    = "Ecosystem Indicators - Productivity - Net Primary Productivity (NPP)",
      timesteps = 1:2,
      rast      = TRUE
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

### Study area grid

    # Get study area grid
    cbm4_results_grid(cbm4_results)

    # Get study area chunk processing key
    cbm4_results_grid_key(cbm4_results)
