
-- Total pools by pixel (MgC)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM("pools.Input"                   * ("inventory.area" * cohort_proportion)) AS Input,
  SUM("pools.SoftwoodMerch"           * ("inventory.area" * cohort_proportion)) AS SoftwoodMerch,
  SUM("pools.SoftwoodFoliage"         * ("inventory.area" * cohort_proportion)) AS SoftwoodFoliage,
  SUM("pools.SoftwoodOther"           * ("inventory.area" * cohort_proportion)) AS SoftwoodOther,
  SUM("pools.SoftwoodCoarseRoots"     * ("inventory.area" * cohort_proportion)) AS SoftwoodCoarseRoots,
  SUM("pools.SoftwoodFineRoots"       * ("inventory.area" * cohort_proportion)) AS SoftwoodFineRoots,
  SUM("pools.HardwoodMerch"           * ("inventory.area" * cohort_proportion)) AS HardwoodMerch,
  SUM("pools.HardwoodFoliage"         * ("inventory.area" * cohort_proportion)) AS HardwoodFoliage,
  SUM("pools.HardwoodOther"           * ("inventory.area" * cohort_proportion)) AS HardwoodOther,
  SUM("pools.HardwoodCoarseRoots"     * ("inventory.area" * cohort_proportion)) AS HardwoodCoarseRoots,
  SUM("pools.HardwoodFineRoots"       * ("inventory.area" * cohort_proportion)) AS HardwoodFineRoots,
  SUM("pools.AboveGroundVeryFastSoil" * ("inventory.area" * cohort_proportion)) AS AboveGroundVeryFastSoil,
  SUM("pools.BelowGroundVeryFastSoil" * ("inventory.area" * cohort_proportion)) AS BelowGroundVeryFastSoil,
  SUM("pools.AboveGroundFastSoil"     * ("inventory.area" * cohort_proportion)) AS AboveGroundFastSoil,
  SUM("pools.BelowGroundFastSoil"     * ("inventory.area" * cohort_proportion)) AS BelowGroundFastSoil,
  SUM("pools.MediumSoil"              * ("inventory.area" * cohort_proportion)) AS MediumSoil,
  SUM("pools.AboveGroundSlowSoil"     * ("inventory.area" * cohort_proportion)) AS AboveGroundSlowSoil,
  SUM("pools.BelowGroundSlowSoil"     * ("inventory.area" * cohort_proportion)) AS BelowGroundSlowSoil,
  SUM("pools.SoftwoodStemSnag"        * ("inventory.area" * cohort_proportion)) AS SoftwoodStemSnag,
  SUM("pools.SoftwoodBranchSnag"      * ("inventory.area" * cohort_proportion)) AS SoftwoodBranchSnag,
  SUM("pools.HardwoodStemSnag"        * ("inventory.area" * cohort_proportion)) AS HardwoodStemSnag,
  SUM("pools.HardwoodBranchSnag"      * ("inventory.area" * cohort_proportion)) AS HardwoodBranchSnag,
  SUM("pools.CO2"                     * ("inventory.area" * cohort_proportion)) AS CO2,
  SUM("pools.CH4"                     * ("inventory.area" * cohort_proportion)) AS CH4,
  SUM("pools.CO"                      * ("inventory.area" * cohort_proportion)) AS CO,
  SUM("pools.NO2"                     * ("inventory.area" * cohort_proportion)) AS NO2,
  SUM("pools.Products"                * ("inventory.area" * cohort_proportion)) AS Products
FROM simulation a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

