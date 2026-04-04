
-- Annual process flux density for each pixel
-- Units: tonnes per hectare (t/ha)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DecayDOMCO2Emission         * cohort_proportion) AS DecayDOMCO2Emission,
  SUM(DeltaBiomass_AG             * cohort_proportion) AS DeltaBiomass_AG,
  SUM(DeltaBiomass_BG             * cohort_proportion) AS DeltaBiomass_BG,
  SUM(TurnoverMerchLitterInput    * cohort_proportion) AS TurnoverMerchLitterInput,
  SUM(TurnoverFolLitterInput      * cohort_proportion) AS TurnoverFolLitterInput,
  SUM(TurnoverOthLitterInput      * cohort_proportion) AS TurnoverOthLitterInput,
  SUM(TurnoverCoarseLitterInput   * cohort_proportion) AS TurnoverCoarseLitterInput,
  SUM(TurnoverFineLitterInput     * cohort_proportion) AS TurnoverFineLitterInput,
  SUM(DecayVFastAGToAir           * cohort_proportion) AS DecayVFastAGToAir,
  SUM(DecayVFastBGToAir           * cohort_proportion) AS DecayVFastBGToAir,
  SUM(DecayFastAGToAir            * cohort_proportion) AS DecayFastAGToAir,
  SUM(DecayFastBGToAir            * cohort_proportion) AS DecayFastBGToAir,
  SUM(DecayMediumToAir            * cohort_proportion) AS DecayMediumToAir,
  SUM(DecaySlowAGToAir            * cohort_proportion) AS DecaySlowAGToAir,
  SUM(DecaySlowBGToAir            * cohort_proportion) AS DecaySlowBGToAir,
  SUM(DecaySWStemSnagToAir        * cohort_proportion) AS DecaySWStemSnagToAir,
  SUM(DecaySWBranchSnagToAir      * cohort_proportion) AS DecaySWBranchSnagToAir,
  SUM(DecayHWStemSnagToAir        * cohort_proportion) AS DecayHWStemSnagToAir,
  SUM(DecayHWBranchSnagToAir      * cohort_proportion) AS DecayHWBranchSnagToAir
FROM annual_process_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

