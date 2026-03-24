# CBM4r

## Description

`CBM4r` is an R package interface to [CBM4](https://github.com/cat-cfs/cbm4) Python applications. This is provided by writing to and reading from [CBM4 spatial datasets](https://github.com/cat-cfs/tech_docs/tree/main/dev/cbm4/structure#cbm4-spatial-datasets).

Spatial datasets are created with an additional "pixels" table with metadata about the study area pixels. 
This table has columns `pixel_index`, `chunk_index`, `raster_index`, `x`, `y`, `area`, `admin_boundary`, `eco_boundary`, `spatial_unit`, `afforestation_pre_type`, `historic_disturbance_type`, `last_pass_disturbance_type`.
