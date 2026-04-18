
-- Annual process flux for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(DecayDOMCO2Emission         * (c.area * a.cohort_proportion)) AS DecayDOMCO2Emission,
  SUM(DeltaBiomass_AG             * (c.area * a.cohort_proportion)) AS DeltaBiomass_AG,
  SUM(DeltaBiomass_BG             * (c.area * a.cohort_proportion)) AS DeltaBiomass_BG,
  SUM(TurnoverMerchLitterInput    * (c.area * a.cohort_proportion)) AS TurnoverMerchLitterInput,
  SUM(TurnoverFolLitterInput      * (c.area * a.cohort_proportion)) AS TurnoverFolLitterInput,
  SUM(TurnoverOthLitterInput      * (c.area * a.cohort_proportion)) AS TurnoverOthLitterInput,
  SUM(TurnoverCoarseLitterInput   * (c.area * a.cohort_proportion)) AS TurnoverCoarseLitterInput,
  SUM(TurnoverFineLitterInput     * (c.area * a.cohort_proportion)) AS TurnoverFineLitterInput,
  SUM(DecayVFastAGToAir           * (c.area * a.cohort_proportion)) AS DecayVFastAGToAir,
  SUM(DecayVFastBGToAir           * (c.area * a.cohort_proportion)) AS DecayVFastBGToAir,
  SUM(DecayFastAGToAir            * (c.area * a.cohort_proportion)) AS DecayFastAGToAir,
  SUM(DecayFastBGToAir            * (c.area * a.cohort_proportion)) AS DecayFastBGToAir,
  SUM(DecayMediumToAir            * (c.area * a.cohort_proportion)) AS DecayMediumToAir,
  SUM(DecaySlowAGToAir            * (c.area * a.cohort_proportion)) AS DecaySlowAGToAir,
  SUM(DecaySlowBGToAir            * (c.area * a.cohort_proportion)) AS DecaySlowBGToAir,
  SUM(DecaySWStemSnagToAir        * (c.area * a.cohort_proportion)) +
  SUM(DecayHWStemSnagToAir        * (c.area * a.cohort_proportion)) AS DecayStemSnagToAir,
  SUM(DecaySWBranchSnagToAir      * (c.area * a.cohort_proportion)) +
  SUM(DecayHWBranchSnagToAir      * (c.area * a.cohort_proportion)) AS DecayBranchSnagToAir
FROM annual_process_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index
LEFT JOIN area c ON b.raster_index = c.raster_index and b.chunk_index = c.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

