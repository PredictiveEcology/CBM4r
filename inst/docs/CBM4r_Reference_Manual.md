# DESCRIPTION

```
Package: CBM4r
Title: CBM4 in R
Version: 1.0.0
Authors@R: 
    person("Susan", "Murray", email = "murray.e.susan@gmail.com", role = c("aut", "cre"))
Description: R interface to CBM4 Python applications. 
License: GPL-3
Depends: R (>= 4.1.0)
Encoding: UTF-8
Roxygen: list(markdown = TRUE)
Imports:
  arrow,
  data.table (>= 1.18.0),
  dplyr,
  reticulate,
  RSQLite,
  terra
Suggests:
  covr,
  devtools,
  gert,
  knitr,
  rmarkdown,
  testthat (>= 3.0.0),
  withr
Config/testthat/edition: 3
VignetteBuilder: knitr
Config/roxygen2/version: 8.0.0
RoxygenNote: 8.0.0
```

# `cbm_defaults_listTables`: CBM defaults: list tables

## Description

List tables in a CBM defaults SQLite database.

## Usage

```r
cbm_defaults_listTables(cbm_defaults_db = getOption("CBM4r.db.path"))
```

## Arguments

* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `...`: arguments to `RSQLite::dbListTables`

# `cbm_defaults_readTable`: CBM defaults: read table

## Description

Read a table from a CBM defaults SQLite database.

## Usage

```r
cbm_defaults_readTable(
  tableName,
  cbm_defaults_db = getOption("CBM4r.db.path"),
  localeID = getOption("CBM4r.db.localeID"),
  ...
)
```

## Arguments

* `tableName`: character. Table name.
Use `cbm_defaults_listTables` to select a table.
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `localeID`: integer. Locale ID.
* `...`: arguments to `RSQLite::dbReadTable`

# `cbm4_data_copy`: CBM4 data copy

## Description

Copy a CBM4 spatial parquet datasets directory.

## Usage

```r
cbm4_data_copy(
  template_data,
  cbm4_data,
  dataset_names = NULL,
  overwrite = FALSE
)
```

## Arguments

* `template_data`: character.
Path to template CBM4 spatial parquet datasets directory.
May be omitted if full paths to template datasets are provided.
* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `dataset_names`: character. Names of CBM4 spatial parquet datasets to copy.
* `overwrite`: logical. Overwrite existing files.

## Value

`NULL`. Data directory will be copied.

# `cbm4_data_copy_dataset`: CBM4 data copy dataset

## Description

Copy a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_data_copy_dataset(
  cbm4_data = NULL,
  dataset_name = NULL,
  dataset_path = file.path(cbm4_data, dataset_name),
  template_data = cbm4_data,
  template_name = dataset_name,
  template_path = file.path(template_data, template_name),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `template_data`: character.
Path to template CBM4 spatial parquet datasets directory.
May be omitted if full paths to template datasets are provided.
* `template_name`: character.
Name of a CBM4 spatial parquet dataset to use as a template for the new dataset.
* `template_path`: character.
Path to CBM4 spatial parquet dataset to use as a template for the new dataset.
Defaults to `file.path(cbm4_data, template_name)`
* `...`: arguments to `[arrow_space_dataset_copy](arrow_space_dataset_copy)`

## Value

`NULL`. Dataset will be copied.

# `cbm4_grid_meta`: CBM4 grid metadata

## Description

Create a study area grid metadata table.

## Usage

```r
cbm4_grid_meta(
  grid_rast,
  admin_boundary,
  eco_boundary = NULL,
  eco_boundary_id = NULL,
  chunk_size = NULL,
  chunk_meta = NULL,
  def_afforestation_pre_type = "None",
  def_historic_disturbance_type = "Wildfire",
  def_last_pass_disturbance_type = "Wildfire",
  cbm_defaults_db = getOption("CBM4r.db.path"),
  ...
)
```

## Arguments

* `grid_rast`: terra `SpatRaster`. Grid defining the study area.
* `admin_boundary`: character. Canada province or territory name.
* `eco_boundary`: character. Canada ecozone name.
* `eco_boundary_id`: integer. Canada ecozone ID. Provide this or `eco_boundary`.
* `chunk_size`: integer. Number of pixels or `chunk_meta` groups in each processing chunk.
* `chunk_meta`: data.table. Table to use to group pixels by shared characteristics.
Required columns: `pixel_index` and at least one other column.
* `def_afforestation_pre_type`: character. Land use before forestation.
Defined in CBM defaults database tables 'afforestation_pre_type'
* `def_historic_disturbance_type`: character. Historic disturbance type.
Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
* `def_last_pass_disturbance_type`: character. Last pass disturbance.
Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `...`: unused

# `cbm4_read_cohorts`: CBM4 read cohorts

## Description

Read cohorts from a simulation CBM4 spatial parquet dataset.

## Usage

```r
cbm4_read_cohorts(cbm4_results, timestep, grid_meta = NULL)
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `timestep`: integer. Simulation timestep with 1 representing the first year.
* `grid_meta`: data.table. Grid metadata.
May not be required but can be provided for efficiency.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.

## Value

`data.table`

# `cbm4_results_grid`: CBM4 results grid

## Description

Recreate a study area grid from a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_results_grid(cbm4_results, dataset_name = "simulation")
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.

## Value

`SpatRaster`

# `cbm4_results_grid_key`: CBM4 results grid key

## Description

Retrieve a study area grid key for a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_results_grid_key(
  cbm4_results,
  dataset_name = "simulation",
  coords = FALSE
)
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `coords`: logical. Return pixel coordinates.

## Value

`data.table` with columns `pixel_index`, `chunk_index`, `raster_index`.

# `cbm4_results_processor`: CBM4 results processor

## Description

CBM4 results processor

## Usage

```r
cbm4_results_processor(cbm4_data, max_workers = NULL, views = TRUE)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `max_workers`: integer.
Number of parallel processes to use.
NULL or NA will bypass this and and cap processing at the system's available resources.
A positive integer will spawn multiple processes assigned to each `chunk_index`.
* `views`: logical. Require views to be available

## Value

`SQLResultsProcessor`

# `cbm4_results_query`: CBM4 results query

## Description

Query CBM4 results with SQL.

## Usage

```r
cbm4_results_query(cbm4_results, query = NULL)
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `query`: character. SQL query

## Value

`data.table`

# `cbm4_results_raster`: CBM4 results raster

## Description

Read simulation results into a raster with carbon in tonnes per hectare (t/ha).

## Usage

```r
cbm4_results_raster(
  cbm4_results,
  view_name = NULL,
  view_column = NULL,
  timesteps = NULL,
  grid_meta = NULL,
  list = FALSE
)
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `view_name`: character. `SQLResultsProcessor` view name.
If NULL the function will return an empty study area grid.
* `view_column`: character. `SQLResultsProcessor` view column name.
* `timesteps`: integer. Simulation timesteps with 1 representing the first year.
* `grid_meta`: data.table. Grid metadata.
May not be required but can be provided for efficiency.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.
* `list`: logical. Return a table of options.

## Value

`SpatRaster`

# `cbm4_results_totals`: CBM4 results totals

## Description

Read simulation results into a table with carbon totals per timestep.

## Usage

```r
cbm4_results_totals(
  cbm4_results,
  view_name,
  view_columns = NULL,
  timesteps = NULL,
  units = "t",
  list = FALSE
)
```

## Arguments

* `cbm4_results`: character or `SQLResultsProcessor`.
Path to CBM4 spatial datasets directory
or a `SQLResultsProcessor` object created with `[cbm4_results_processor](cbm4_results_processor)`.
* `view_name`: character. `SQLResultsProcessor` view name.
* `view_columns`: character. `SQLResultsProcessor` view column names.
The vector can have names to use as column aliases.
* `timesteps`: integer. Simulation timesteps with 1 representing the first year.
* `units`: character. "t", "Mt", or "t/ha".
* `list`: logical. Return a table of options.

## Value

`data.table`

# `cbm4_set_grid_meta`: CBM4 set grid metadata

## Description

Format `grid_meta` table by reference.

## Usage

```r
cbm4_set_grid_meta(
  grid_meta,
  grid_rast = NULL,
  chunk_size = NULL,
  chunk_meta = NULL,
  def_afforestation_pre_type = "None",
  def_historic_disturbance_type = "Wildfire",
  def_last_pass_disturbance_type = "Wildfire",
  cbm_defaults_db = getOption("CBM4r.db.path"),
  ...
)
```

## Arguments

* `grid_meta`: data.table. Grid metadata.
Required columns: `pixel_index`,
`admin_boundary`, `admin_abbrev`, or `admin_boundary_id`.
`eco_boundary` or `eco_boundary_id`,
`spatial_unit`.
Optional columns: `chunk_index`, `raster_index`,
`afforestation_pre_type`, `historic_disturbance_type`, `last_pass_disturbance_type`.
* `grid_rast`: terra `SpatRaster`. Grid defining the study area.
* `chunk_size`: integer. Number of pixels or `chunk_meta` groups in each processing chunk.
* `chunk_meta`: data.table. Table to use to group pixels by shared characteristics.
Required columns: `pixel_index` and at least one other column.
* `def_afforestation_pre_type`: character. Land use before forestation.
Defined in CBM defaults database tables 'afforestation_pre_type'
* `def_historic_disturbance_type`: character. Historic disturbance type.
Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
* `def_last_pass_disturbance_type`: character. Last pass disturbance.
Defined in CBM defaults database tables 'disturbance_type' and 'disturbance_type_tr'.
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `...`: unused

## Value

`data.table` updated by reference.

# `cbm4_spinup`: CBM4 spinup

## Description

Run spinup initialization on CBM4 spatial parquet datasets.

## Usage

```r
cbm4_spinup(
  cbm4_data = NULL,
  max_workers = NULL,
  cbm_defaults_db = getOption("CBM4r.db.path"),
  spinup_parameters_dataset = file.path(cbm4_data, "spinup_parameters"),
  inventory_dataset = file.path(cbm4_data, "inventory"),
  simulation_dataset = file.path(cbm4_data, "simulation"),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `max_workers`: integer.
Number of parallel processes to use.
NULL or NA will bypass this and and cap processing at the system's available resources.
A positive integer will spawn multiple processes assigned to each `chunk_index`.
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `spinup_parameters_dataset`: character.
Path to spinup parameters CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "spinup_parameters")`
* `inventory_dataset`: character.
Path to inventory CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "inventory")`
* `simulation_dataset`: character.
Path to simulation CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "simulation")`
* `...`: unused

## Value

`NULL`. Updates will be made to CBM4 spatial parquet datasets.

# `cbm4_step`: CBM4 step

## Description

Run an annual step on CBM4 spatial parquet datasets.

## Usage

```r
cbm4_step(
  cbm4_data = NULL,
  timestep,
  area_unit_conversion = 0.0001,
  write_parameters = FALSE,
  max_workers = NULL,
  cbm_defaults_db = getOption("CBM4r.db.path"),
  step_parameters_dataset = file.path(cbm4_data, "step_parameters"),
  inventory_dataset = file.path(cbm4_data, "inventory"),
  disturbance_dataset = file.path(cbm4_data, "disturbance"),
  simulation_dataset = file.path(cbm4_data, "simulation"),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `timestep`: integer. Simulation timestep with 1 representing the first year.
* `area_unit_conversion`: numeric. Conversion factor of area to hectares (ha).
* `write_parameters`: logical. Write cohort step parameters to file.
* `max_workers`: integer.
Number of parallel processes to use.
NULL or NA will bypass this and and cap processing at the system's available resources.
A positive integer will spawn multiple processes assigned to each `chunk_index`.
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `step_parameters_dataset`: character.
Path to step parameters CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "step_parameters")`
* `inventory_dataset`: character.
Path to inventory CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "inventory")`
* `disturbance_dataset`: character.
Path to disturbance CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "disturbance")`
* `simulation_dataset`: character.
Path to simulation CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "simulation")`
* `...`: unused

## Value

`NULL`. Updates will be made to CBM4 spatial parquet datasets.

# `cbm4_step_with_cohorts`: CBM4 step: with cohorts

## Description

Run an annual step on CBM4 spatial parquet datasets with an alternate set of cohort data.

## Usage

```r
cbm4_step_with_cohorts(
  cbm4_data = NULL,
  timestep,
  simulation_dataset = file.path(cbm4_data, "simulation"),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `timestep`: integer. Simulation timestep with 1 representing the first year.
* `simulation_dataset`: character.
Path to simulation CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, "simulation")`
* `...`: arguments to `[cbm4_write_simulation](cbm4_write_simulation)` or `[cbm4_step](cbm4_step)`

## Value

`NULL`. Updates will be made to CBM4 spatial parquet datasets.

# `cbm4_virtualenv_create`: cbm4_virtualenv_create

## Description

Create a Python virtual environment to run CBM4.

## Usage

```r
cbm4_virtualenv_create(
  envname = "r-CBM4",
  version = NULL,
  upgrade = FALSE,
  quiet = FALSE,
  ...
)
```

## Arguments

* `envname`: character. Virtual environment name.
* `version`: character. CBM4 version.
* `upgrade`: logical. Upgrade Python packages.
* `quiet`: logical. Silence pip output.
* `...`: arguments to `[virtualenv_create](https://rdrr.io/pkg/reticulate/man/virtualenv-tools.html)`

# `cbm4_write_disturbance`: CBM4 write disturbance

## Description

Write disturbances to a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_write_disturbance(
  cbm4_data = NULL,
  dist_meta = NULL,
  dist_events = NULL,
  classifiers = NULL,
  grid_meta = NULL,
  template_name = "inventory",
  template_path = file.path(cbm4_data, template_name),
  dataset_name = "disturbance",
  dataset_path = file.path(cbm4_data, dataset_name),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `dist_meta`: data.table. Disturbance metadata.
* `dist_events`: data.table. Disturbance events.
* `classifiers`: character.
Column names of cohort inventory identifiers.
* `grid_meta`: data.table. Grid metadata.
May not be required but can be provided for efficiency.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.
* `template_name`: character.
Name of a CBM4 spatial parquet dataset to use as a template for the new dataset.
* `template_path`: character.
Path to CBM4 spatial parquet dataset to use as a template for the new dataset.
Defaults to `file.path(cbm4_data, template_name)`
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `...`: arguments to `[cbm4_format_disturbance](cbm4_format_disturbance)`

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

# `cbm4_write_geo`: CBM4 write geo

## Description

Initiate a CBM4 spatial parquet dataset with study area geographic metadata.

## Usage

```r
cbm4_write_geo(
  cbm4_data = NULL,
  dataset_name,
  grid_meta,
  grid_rast,
  dataset_path = file.path(cbm4_data, dataset_name),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `grid_meta`: data.table. Grid metadata.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.
* `grid_rast`: terra `SpatRaster`. Grid defining the study area.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `...`: arguments to `[arrow_space_dataset_write_geo](arrow_space_dataset_write_geo)`

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

# `cbm4_write_inventory`: CBM4 write inventory

## Description

Write inventory to a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_write_inventory(
  cbm4_data = NULL,
  cohorts = NULL,
  classifiers = NULL,
  grid_meta = NULL,
  grid_rast = NULL,
  dataset_name = "inventory",
  dataset_path = file.path(cbm4_data, dataset_name),
  cbm_defaults_db = getOption("CBM4r.db.path"),
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `cohorts`: data.table. Cohort inventory.
* `classifiers`: character.
Column names of cohort inventory identifiers.
* `grid_meta`: data.table. Grid metadata.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.
* `grid_rast`: terra `SpatRaster`. Grid defining the study area.
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `...`: arguments to `[cbm4_format_inventory](cbm4_format_inventory)`

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

# `cbm4_write_simulation`: CBM4 write simulation

## Description

Write inventory and associated carbon pools
directly to a simulation CBM4 spatial parquet dataset.

## Usage

```r
cbm4_write_simulation(
  cbm4_data = NULL,
  timestep = 0,
  cohorts = NULL,
  classifiers = NULL,
  grid_meta = NULL,
  grid_rast = NULL,
  template_name = "inventory",
  template_path = file.path(cbm4_data, template_name),
  dataset_name = "simulation",
  dataset_path = file.path(cbm4_data, dataset_name),
  cbm_defaults_db = getOption("CBM4r.db.path"),
  schema = NULL,
  ...
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `timestep`: integer. Simulation timestep with 1 representing the first year.
* `cohorts`: data.table. Cohort inventory.
* `classifiers`: character.
Column names of cohort inventory identifiers.
* `grid_meta`: data.table. Grid metadata.
This table can be created with `cbm4_grid_meta` or `cbm4_set_grid_meta`.
* `grid_rast`: terra `SpatRaster`. Grid defining the study area.
* `template_name`: character.
Name of a CBM4 spatial parquet dataset to use as a template for the new dataset.
* `template_path`: character.
Path to CBM4 spatial parquet dataset to use as a template for the new dataset.
Defaults to `file.path(cbm4_data, template_name)`
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.
* `schema`: internal
* `...`: arguments to `[cbm4_format_simulation](cbm4_format_simulation)`

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

# `cbm4_write_spinup_parameters`: CBM4 write spinup parameters

## Description

Write spinup parameters to a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_write_spinup_parameters(
  cbm4_data = NULL,
  gc_meta,
  gc_incr,
  classifiers = NULL,
  template_name = "inventory",
  template_path = file.path(cbm4_data, template_name),
  dataset_name = "spinup_parameters",
  dataset_path = file.path(cbm4_data, dataset_name),
  cbm_defaults_db = getOption("CBM4r.db.path")
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `gc_meta`: data.table. Growth curve metadata
* `gc_incr`: data.table. Growth curve carbon increments
* `classifiers`: character.
Column names of cohort inventory identifiers.
* `template_name`: character.
Name of a CBM4 spatial parquet dataset to use as a template for the new dataset.
* `template_path`: character.
Path to CBM4 spatial parquet dataset to use as a template for the new dataset.
Defaults to `file.path(cbm4_data, template_name)`
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

# `cbm4_write_step_parameters`: CBM4 write step parameters

## Description

Write step parameters to a CBM4 spatial parquet dataset.

## Usage

```r
cbm4_write_step_parameters(
  cbm4_data = NULL,
  gc_meta,
  gc_incr,
  classifiers = NULL,
  template_name = "inventory",
  template_path = file.path(cbm4_data, template_name),
  dataset_name = "step_parameters",
  dataset_path = file.path(cbm4_data, dataset_name),
  cbm_defaults_db = getOption("CBM4r.db.path")
)
```

## Arguments

* `cbm4_data`: character.
Path to CBM4 spatial parquet datasets directory.
May be omitted if full paths to datasets are provided.
* `gc_meta`: data.table. Growth curve metadata
* `gc_incr`: data.table. Growth curve carbon increments
* `classifiers`: character.
Column names of cohort inventory identifiers.
* `template_name`: character.
Name of a CBM4 spatial parquet dataset to use as a template for the new dataset.
* `template_path`: character.
Path to CBM4 spatial parquet dataset to use as a template for the new dataset.
Defaults to `file.path(cbm4_data, template_name)`
* `dataset_name`: character.
Name of the CBM4 spatial parquet dataset.
* `dataset_path`: character.
Path to the CBM4 spatial parquet dataset.
Defaults to `file.path(cbm4_data, dataset_name)`
* `cbm_defaults_db`: character.
Path to CBM defaults SQLite database.

## Value

`NULL`. Data will be written to the CBM4 spatial parquet dataset.

