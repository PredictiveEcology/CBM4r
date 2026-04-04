
-- Carbon density for each pixel
-- Units: tonnes per hectare (t/ha)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM("pools.Input"                   * cohort_proportion) AS Input,
  SUM("pools.SoftwoodMerch"           * cohort_proportion) +
  SUM("pools.HardwoodMerch"           * cohort_proportion) AS Merch,
  SUM("pools.SoftwoodFoliage"         * cohort_proportion) +
  SUM("pools.HardwoodFoliage"         * cohort_proportion) AS Foliage,
  SUM("pools.SoftwoodOther"           * cohort_proportion) +
  SUM("pools.HardwoodOther"           * cohort_proportion) AS Other,
  SUM("pools.SoftwoodCoarseRoots"     * cohort_proportion) +
  SUM("pools.HardwoodCoarseRoots"     * cohort_proportion) AS CoarseRoots,
  SUM("pools.SoftwoodFineRoots"       * cohort_proportion) +
  SUM("pools.HardwoodFineRoots"       * cohort_proportion) AS FineRoots,
  SUM("pools.AboveGroundVeryFastSoil" * cohort_proportion) AS AboveGroundVeryFastSoil,
  SUM("pools.BelowGroundVeryFastSoil" * cohort_proportion) AS BelowGroundVeryFastSoil,
  SUM("pools.AboveGroundFastSoil"     * cohort_proportion) AS AboveGroundFastSoil,
  SUM("pools.BelowGroundFastSoil"     * cohort_proportion) AS BelowGroundFastSoil,
  SUM("pools.MediumSoil"              * cohort_proportion) AS MediumSoil,
  SUM("pools.AboveGroundSlowSoil"     * cohort_proportion) AS AboveGroundSlowSoil,
  SUM("pools.BelowGroundSlowSoil"     * cohort_proportion) AS BelowGroundSlowSoil,
  SUM("pools.SoftwoodStemSnag"        * cohort_proportion) +
  SUM("pools.HardwoodStemSnag"        * cohort_proportion) AS StemSnag,
  SUM("pools.SoftwoodBranchSnag"      * cohort_proportion) +
  SUM("pools.HardwoodBranchSnag"      * cohort_proportion) AS BranchSnag,
  SUM("pools.CO2"                     * cohort_proportion) AS CO2,
  SUM("pools.CH4"                     * cohort_proportion) AS CH4,
  SUM("pools.CO"                      * cohort_proportion) AS CO,
  SUM("pools.NO2"                     * cohort_proportion) AS NO2,
  SUM("pools.Products"                * cohort_proportion) AS Products
FROM simulation a
LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

