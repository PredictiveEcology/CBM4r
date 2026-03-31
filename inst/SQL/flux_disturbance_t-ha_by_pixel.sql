
-- Disturbance flux density for each pixel
-- Units: tonnes per hectare (t/ha)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DisturbanceCO2Production     * (cohort_proportion)) AS DisturbanceCO2Production,
  SUM(DisturbanceCH4Production     * (cohort_proportion)) AS DisturbanceCH4Production,
  SUM(DisturbanceCOProduction      * (cohort_proportion)) AS DisturbanceCOProduction,
  SUM(DisturbanceBioCO2Emission    * (cohort_proportion)) AS DisturbanceBioCO2Emission,
  SUM(DisturbanceBioCH4Emission    * (cohort_proportion)) AS DisturbanceBioCH4Emission,
  SUM(DisturbanceBioCOEmission     * (cohort_proportion)) AS DisturbanceBioCOEmission,
  SUM(DisturbanceSoftProduction    * (cohort_proportion)) AS DisturbanceSoftProduction,
  SUM(DisturbanceHardProduction    * (cohort_proportion)) AS DisturbanceHardProduction,
  SUM(DisturbanceDOMProduction     * (cohort_proportion)) AS DisturbanceDOMProduction,
  SUM(DisturbanceMerchToAir        * (cohort_proportion)) AS DisturbanceMerchToAir,
  SUM(DisturbanceFolToAir          * (cohort_proportion)) AS DisturbanceFolToAir,
  SUM(DisturbanceOthToAir          * (cohort_proportion)) AS DisturbanceOthToAir,
  SUM(DisturbanceCoarseToAir       * (cohort_proportion)) AS DisturbanceCoarseToAir,
  SUM(DisturbanceFineToAir         * (cohort_proportion)) AS DisturbanceFineToAir,
  SUM(DisturbanceDOMCO2Emission    * (cohort_proportion)) AS DisturbanceDOMCO2Emission,
  SUM(DisturbanceDOMCH4Emission    * (cohort_proportion)) AS DisturbanceDOMCH4Emission,
  SUM(DisturbanceDOMCOEmission     * (cohort_proportion)) AS DisturbanceDOMCOEmission,
  SUM(DisturbanceMerchLitterInput  * (cohort_proportion)) AS DisturbanceMerchLitterInput,
  SUM(DisturbanceFolLitterInput    * (cohort_proportion)) AS DisturbanceFolLitterInput,
  SUM(DisturbanceOthLitterInput    * (cohort_proportion)) AS DisturbanceOthLitterInput,
  SUM(DisturbanceCoarseLitterInput * (cohort_proportion)) AS DisturbanceCoarseLitterInput,
  SUM(DisturbanceFineLitterInput   * (cohort_proportion)) AS DisturbanceFineLitterInput,
  SUM(DisturbanceVFastAGToAir      * (cohort_proportion)) AS DisturbanceVFastAGToAir,
  SUM(DisturbanceVFastBGToAir      * (cohort_proportion)) AS DisturbanceVFastBGToAir,
  SUM(DisturbanceFastAGToAir       * (cohort_proportion)) AS DisturbanceFastAGToAir,
  SUM(DisturbanceFastBGToAir       * (cohort_proportion)) AS DisturbanceFastBGToAir,
  SUM(DisturbanceMediumToAir       * (cohort_proportion)) AS DisturbanceMediumToAir,
  SUM(DisturbanceSlowAGToAir       * (cohort_proportion)) AS DisturbanceSlowAGToAir,
  SUM(DisturbanceSlowBGToAir       * (cohort_proportion)) AS DisturbanceSlowBGToAir,
  SUM(DisturbanceSWStemSnagToAir   * (cohort_proportion)) AS DisturbanceSWStemSnagToAir,
  SUM(DisturbanceSWBranchSnagToAir * (cohort_proportion)) AS DisturbanceSWBranchSnagToAir,
  SUM(DisturbanceHWStemSnagToAir   * (cohort_proportion)) AS DisturbanceHWStemSnagToAir,
  SUM(DisturbanceHWBranchSnagToAir * (cohort_proportion)) AS DisturbanceHWBranchSnagToAir
FROM disturbance_flux a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

