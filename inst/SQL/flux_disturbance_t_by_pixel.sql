
-- Disturbance flux for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DisturbanceCO2Production     * (c.area * a.cohort_proportion)) AS DisturbanceCO2Production,
  SUM(DisturbanceCH4Production     * (c.area * a.cohort_proportion)) AS DisturbanceCH4Production,
  SUM(DisturbanceCOProduction      * (c.area * a.cohort_proportion)) AS DisturbanceCOProduction,
  SUM(DisturbanceBioCO2Emission    * (c.area * a.cohort_proportion)) AS DisturbanceBioCO2Emission,
  SUM(DisturbanceBioCH4Emission    * (c.area * a.cohort_proportion)) AS DisturbanceBioCH4Emission,
  SUM(DisturbanceBioCOEmission     * (c.area * a.cohort_proportion)) AS DisturbanceBioCOEmission,
  SUM(DisturbanceSoftProduction    * (c.area * a.cohort_proportion)) AS DisturbanceSoftProduction,
  SUM(DisturbanceHardProduction    * (c.area * a.cohort_proportion)) AS DisturbanceHardProduction,
  SUM(DisturbanceDOMProduction     * (c.area * a.cohort_proportion)) AS DisturbanceDOMProduction,
  SUM(DisturbanceMerchToAir        * (c.area * a.cohort_proportion)) AS DisturbanceMerchToAir,
  SUM(DisturbanceFolToAir          * (c.area * a.cohort_proportion)) AS DisturbanceFolToAir,
  SUM(DisturbanceOthToAir          * (c.area * a.cohort_proportion)) AS DisturbanceOthToAir,
  SUM(DisturbanceCoarseToAir       * (c.area * a.cohort_proportion)) AS DisturbanceCoarseToAir,
  SUM(DisturbanceFineToAir         * (c.area * a.cohort_proportion)) AS DisturbanceFineToAir,
  SUM(DisturbanceDOMCO2Emission    * (c.area * a.cohort_proportion)) AS DisturbanceDOMCO2Emission,
  SUM(DisturbanceDOMCH4Emission    * (c.area * a.cohort_proportion)) AS DisturbanceDOMCH4Emission,
  SUM(DisturbanceDOMCOEmission     * (c.area * a.cohort_proportion)) AS DisturbanceDOMCOEmission,
  SUM(DisturbanceMerchLitterInput  * (c.area * a.cohort_proportion)) AS DisturbanceMerchLitterInput,
  SUM(DisturbanceFolLitterInput    * (c.area * a.cohort_proportion)) AS DisturbanceFolLitterInput,
  SUM(DisturbanceOthLitterInput    * (c.area * a.cohort_proportion)) AS DisturbanceOthLitterInput,
  SUM(DisturbanceCoarseLitterInput * (c.area * a.cohort_proportion)) AS DisturbanceCoarseLitterInput,
  SUM(DisturbanceFineLitterInput   * (c.area * a.cohort_proportion)) AS DisturbanceFineLitterInput,
  SUM(DisturbanceVFastAGToAir      * (c.area * a.cohort_proportion)) AS DisturbanceVFastAGToAir,
  SUM(DisturbanceVFastBGToAir      * (c.area * a.cohort_proportion)) AS DisturbanceVFastBGToAir,
  SUM(DisturbanceFastAGToAir       * (c.area * a.cohort_proportion)) AS DisturbanceFastAGToAir,
  SUM(DisturbanceFastBGToAir       * (c.area * a.cohort_proportion)) AS DisturbanceFastBGToAir,
  SUM(DisturbanceMediumToAir       * (c.area * a.cohort_proportion)) AS DisturbanceMediumToAir,
  SUM(DisturbanceSlowAGToAir       * (c.area * a.cohort_proportion)) AS DisturbanceSlowAGToAir,
  SUM(DisturbanceSlowBGToAir       * (c.area * a.cohort_proportion)) AS DisturbanceSlowBGToAir,
  SUM(DisturbanceSWStemSnagToAir   * (c.area * a.cohort_proportion)) AS DisturbanceSWStemSnagToAir,
  SUM(DisturbanceSWBranchSnagToAir * (c.area * a.cohort_proportion)) AS DisturbanceSWBranchSnagToAir,
  SUM(DisturbanceHWStemSnagToAir   * (c.area * a.cohort_proportion)) AS DisturbanceHWStemSnagToAir,
  SUM(DisturbanceHWBranchSnagToAir * (c.area * a.cohort_proportion)) AS DisturbanceHWBranchSnagToAir
FROM disturbance_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.chunk_index = b.chunk_index
LEFT JOIN area c ON b.raster_index = c.raster_index and b.chunk_index = c.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

