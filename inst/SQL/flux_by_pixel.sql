
-- Total flux by pixel (MgC/year)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DecayDOMCO2Emission         * ("inventory.area" * cohort_proportion)) AS DecayDOMCO2Emission,
  SUM(DeltaBiomass_AG             * ("inventory.area" * cohort_proportion)) AS DeltaBiomass_AG,
  SUM(DeltaBiomass_BG             * ("inventory.area" * cohort_proportion)) AS DeltaBiomass_BG,
  SUM(TurnoverMerchLitterInput    * ("inventory.area" * cohort_proportion)) AS TurnoverMerchLitterInput,
  SUM(TurnoverFolLitterInput      * ("inventory.area" * cohort_proportion)) AS TurnoverFolLitterInput,
  SUM(TurnoverOthLitterInput      * ("inventory.area" * cohort_proportion)) AS TurnoverOthLitterInput,
  SUM(TurnoverCoarseLitterInput   * ("inventory.area" * cohort_proportion)) AS TurnoverCoarseLitterInput,
  SUM(TurnoverFineLitterInput     * ("inventory.area" * cohort_proportion)) AS TurnoverFineLitterInput,
  SUM(DecayVFastAGToAir           * ("inventory.area" * cohort_proportion)) AS DecayVFastAGToAir,
  SUM(DecayVFastBGToAir           * ("inventory.area" * cohort_proportion)) AS DecayVFastBGToAir,
  SUM(DecayFastAGToAir            * ("inventory.area" * cohort_proportion)) AS DecayFastAGToAir,
  SUM(DecayFastBGToAir            * ("inventory.area" * cohort_proportion)) AS DecayFastBGToAir,
  SUM(DecayMediumToAir            * ("inventory.area" * cohort_proportion)) AS DecayMediumToAir,
  SUM(DecaySlowAGToAir            * ("inventory.area" * cohort_proportion)) AS DecaySlowAGToAir,
  SUM(DecaySlowBGToAir            * ("inventory.area" * cohort_proportion)) AS DecaySlowBGToAir,
  SUM(DecaySWStemSnagToAir        * ("inventory.area" * cohort_proportion)) AS DecaySWStemSnagToAir,
  SUM(DecaySWBranchSnagToAir      * ("inventory.area" * cohort_proportion)) AS DecaySWBranchSnagToAir,
  SUM(DecayHWStemSnagToAir        * ("inventory.area" * cohort_proportion)) AS DecayHWStemSnagToAir,
  SUM(DecayHWBranchSnagToAir      * ("inventory.area" * cohort_proportion)) AS DecayHWBranchSnagToAir
FROM annual_process_flux a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

