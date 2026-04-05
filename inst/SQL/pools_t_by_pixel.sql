
-- Carbon in pools for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM("pools.Input"                   * (c.area * a.cohort_proportion)) AS Input,
  SUM("pools.SoftwoodMerch"           * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodMerch"           * (c.area * a.cohort_proportion)) AS Merch,
  SUM("pools.SoftwoodFoliage"         * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodFoliage"         * (c.area * a.cohort_proportion)) AS Foliage,
  SUM("pools.SoftwoodOther"           * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodOther"           * (c.area * a.cohort_proportion)) AS Other,
  SUM("pools.SoftwoodCoarseRoots"     * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodCoarseRoots"     * (c.area * a.cohort_proportion)) AS CoarseRoots,
  SUM("pools.SoftwoodFineRoots"       * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodFineRoots"       * (c.area * a.cohort_proportion)) AS FineRoots,
  SUM("pools.AboveGroundVeryFastSoil" * (c.area * a.cohort_proportion)) AS AboveGroundVeryFastSoil,
  SUM("pools.BelowGroundVeryFastSoil" * (c.area * a.cohort_proportion)) AS BelowGroundVeryFastSoil,
  SUM("pools.AboveGroundFastSoil"     * (c.area * a.cohort_proportion)) AS AboveGroundFastSoil,
  SUM("pools.BelowGroundFastSoil"     * (c.area * a.cohort_proportion)) AS BelowGroundFastSoil,
  SUM("pools.MediumSoil"              * (c.area * a.cohort_proportion)) AS MediumSoil,
  SUM("pools.AboveGroundSlowSoil"     * (c.area * a.cohort_proportion)) AS AboveGroundSlowSoil,
  SUM("pools.BelowGroundSlowSoil"     * (c.area * a.cohort_proportion)) AS BelowGroundSlowSoil,
  SUM("pools.SoftwoodStemSnag"        * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodStemSnag"        * (c.area * a.cohort_proportion)) AS StemSnag,
  SUM("pools.SoftwoodBranchSnag"      * (c.area * a.cohort_proportion)) +
  SUM("pools.HardwoodBranchSnag"      * (c.area * a.cohort_proportion)) AS BranchSnag,
  SUM("pools.CO2"                     * (c.area * a.cohort_proportion)) AS CO2,
  SUM("pools.CH4"                     * (c.area * a.cohort_proportion)) AS CH4,
  SUM("pools.CO"                      * (c.area * a.cohort_proportion)) AS CO,
  SUM("pools.NO2"                     * (c.area * a.cohort_proportion)) AS NO2,
  SUM("pools.Products"                * (c.area * a.cohort_proportion)) AS Products
FROM simulation a
LEFT JOIN raster_index b ON a.index = b.index
LEFT JOIN area c ON b.raster_index = c.raster_index and b.chunk_index = c.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

