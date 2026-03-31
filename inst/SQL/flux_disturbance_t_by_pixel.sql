
-- Disturbance flux for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DisturbanceCO2Production     * (area * cohort_proportion)) AS DisturbanceCO2Production,
  SUM(DisturbanceCH4Production     * (area * cohort_proportion)) AS DisturbanceCH4Production,
  SUM(DisturbanceCOProduction      * (area * cohort_proportion)) AS DisturbanceCOProduction,
  SUM(DisturbanceBioCO2Emission    * (area * cohort_proportion)) AS DisturbanceBioCO2Emission,
  SUM(DisturbanceBioCH4Emission    * (area * cohort_proportion)) AS DisturbanceBioCH4Emission,
  SUM(DisturbanceBioCOEmission     * (area * cohort_proportion)) AS DisturbanceBioCOEmission,
  SUM(DisturbanceSoftProduction    * (area * cohort_proportion)) AS DisturbanceSoftProduction,
  SUM(DisturbanceHardProduction    * (area * cohort_proportion)) AS DisturbanceHardProduction,
  SUM(DisturbanceDOMProduction     * (area * cohort_proportion)) AS DisturbanceDOMProduction,
  SUM(DisturbanceMerchToAir        * (area * cohort_proportion)) AS DisturbanceMerchToAir,
  SUM(DisturbanceFolToAir          * (area * cohort_proportion)) AS DisturbanceFolToAir,
  SUM(DisturbanceOthToAir          * (area * cohort_proportion)) AS DisturbanceOthToAir,
  SUM(DisturbanceCoarseToAir       * (area * cohort_proportion)) AS DisturbanceCoarseToAir,
  SUM(DisturbanceFineToAir         * (area * cohort_proportion)) AS DisturbanceFineToAir,
  SUM(DisturbanceDOMCO2Emission    * (area * cohort_proportion)) AS DisturbanceDOMCO2Emission,
  SUM(DisturbanceDOMCH4Emission    * (area * cohort_proportion)) AS DisturbanceDOMCH4Emission,
  SUM(DisturbanceDOMCOEmission     * (area * cohort_proportion)) AS DisturbanceDOMCOEmission,
  SUM(DisturbanceMerchLitterInput  * (area * cohort_proportion)) AS DisturbanceMerchLitterInput,
  SUM(DisturbanceFolLitterInput    * (area * cohort_proportion)) AS DisturbanceFolLitterInput,
  SUM(DisturbanceOthLitterInput    * (area * cohort_proportion)) AS DisturbanceOthLitterInput,
  SUM(DisturbanceCoarseLitterInput * (area * cohort_proportion)) AS DisturbanceCoarseLitterInput,
  SUM(DisturbanceFineLitterInput   * (area * cohort_proportion)) AS DisturbanceFineLitterInput,
  SUM(DisturbanceVFastAGToAir      * (area * cohort_proportion)) AS DisturbanceVFastAGToAir,
  SUM(DisturbanceVFastBGToAir      * (area * cohort_proportion)) AS DisturbanceVFastBGToAir,
  SUM(DisturbanceFastAGToAir       * (area * cohort_proportion)) AS DisturbanceFastAGToAir,
  SUM(DisturbanceFastBGToAir       * (area * cohort_proportion)) AS DisturbanceFastBGToAir,
  SUM(DisturbanceMediumToAir       * (area * cohort_proportion)) AS DisturbanceMediumToAir,
  SUM(DisturbanceSlowAGToAir       * (area * cohort_proportion)) AS DisturbanceSlowAGToAir,
  SUM(DisturbanceSlowBGToAir       * (area * cohort_proportion)) AS DisturbanceSlowBGToAir,
  SUM(DisturbanceSWStemSnagToAir   * (area * cohort_proportion)) AS DisturbanceSWStemSnagToAir,
  SUM(DisturbanceSWBranchSnagToAir * (area * cohort_proportion)) AS DisturbanceSWBranchSnagToAir,
  SUM(DisturbanceHWStemSnagToAir   * (area * cohort_proportion)) AS DisturbanceHWStemSnagToAir,
  SUM(DisturbanceHWBranchSnagToAir * (area * cohort_proportion)) AS DisturbanceHWBranchSnagToAir
FROM disturbance_flux a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

