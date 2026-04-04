
-- Carbon in pools for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM("pools.Input"                   * ("inventory.area" * cohort_proportion)) AS Input,
  SUM("pools.SoftwoodMerch"           * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodMerch"           * ("inventory.area" * cohort_proportion)) AS Merch,
  SUM("pools.SoftwoodFoliage"         * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodFoliage"         * ("inventory.area" * cohort_proportion)) AS Foliage,
  SUM("pools.SoftwoodOther"           * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodOther"           * ("inventory.area" * cohort_proportion)) AS Other,
  SUM("pools.SoftwoodCoarseRoots"     * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodCoarseRoots"     * ("inventory.area" * cohort_proportion)) AS CoarseRoots,
  SUM("pools.SoftwoodFineRoots"       * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodFineRoots"       * ("inventory.area" * cohort_proportion)) AS FineRoots,
  SUM("pools.AboveGroundVeryFastSoil" * ("inventory.area" * cohort_proportion)) AS AboveGroundVeryFastSoil,
  SUM("pools.BelowGroundVeryFastSoil" * ("inventory.area" * cohort_proportion)) AS BelowGroundVeryFastSoil,
  SUM("pools.AboveGroundFastSoil"     * ("inventory.area" * cohort_proportion)) AS AboveGroundFastSoil,
  SUM("pools.BelowGroundFastSoil"     * ("inventory.area" * cohort_proportion)) AS BelowGroundFastSoil,
  SUM("pools.MediumSoil"              * ("inventory.area" * cohort_proportion)) AS MediumSoil,
  SUM("pools.AboveGroundSlowSoil"     * ("inventory.area" * cohort_proportion)) AS AboveGroundSlowSoil,
  SUM("pools.BelowGroundSlowSoil"     * ("inventory.area" * cohort_proportion)) AS BelowGroundSlowSoil,
  SUM("pools.SoftwoodStemSnag"        * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodStemSnag"        * ("inventory.area" * cohort_proportion)) AS StemSnag,
  SUM("pools.SoftwoodBranchSnag"      * ("inventory.area" * cohort_proportion)) +
  SUM("pools.HardwoodBranchSnag"      * ("inventory.area" * cohort_proportion)) AS BranchSnag,
  SUM("pools.CO2"                     * ("inventory.area" * cohort_proportion)) AS CO2,
  SUM("pools.CH4"                     * ("inventory.area" * cohort_proportion)) AS CH4,
  SUM("pools.CO"                      * ("inventory.area" * cohort_proportion)) AS CO,
  SUM("pools.NO2"                     * ("inventory.area" * cohort_proportion)) AS NO2,
  SUM("pools.Products"                * ("inventory.area" * cohort_proportion)) AS Products
FROM simulation a
LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

