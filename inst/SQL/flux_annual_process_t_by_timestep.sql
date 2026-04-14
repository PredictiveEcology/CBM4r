
-- Annual process flux for each timestep
-- Units: tonnes (t)

SELECT
  timestep,
  SUM(DecayDOMCO2Emission       * ("inventory.area" * cohort_proportion)) AS DecayDOMCO2Emission,
  SUM(DeltaBiomass_AG           * ("inventory.area" * cohort_proportion)) AS DeltaBiomass_AG,
  SUM(DeltaBiomass_BG           * ("inventory.area" * cohort_proportion)) AS DeltaBiomass_BG,
  SUM(TurnoverMerchLitterInput  * ("inventory.area" * cohort_proportion)) AS TurnoverMerchLitterInput,
  SUM(TurnoverFolLitterInput    * ("inventory.area" * cohort_proportion)) AS TurnoverFolLitterInput,
  SUM(TurnoverOthLitterInput    * ("inventory.area" * cohort_proportion)) AS TurnoverOthLitterInput,
  SUM(TurnoverCoarseLitterInput * ("inventory.area" * cohort_proportion)) AS TurnoverCoarseLitterInput,
  SUM(TurnoverFineLitterInput   * ("inventory.area" * cohort_proportion)) AS TurnoverFineLitterInput,
  SUM(DecayVFastAGToAir         * ("inventory.area" * cohort_proportion)) AS DecayVFastAGToAir,
  SUM(DecayVFastBGToAir         * ("inventory.area" * cohort_proportion)) AS DecayVFastBGToAir,
  SUM(DecayFastAGToAir          * ("inventory.area" * cohort_proportion)) AS DecayFastAGToAir,
  SUM(DecayFastBGToAir          * ("inventory.area" * cohort_proportion)) AS DecayFastBGToAir,
  SUM(DecayMediumToAir          * ("inventory.area" * cohort_proportion)) AS DecayMediumToAir,
  SUM(DecaySlowAGToAir          * ("inventory.area" * cohort_proportion)) AS DecaySlowAGToAir,
  SUM(DecaySlowBGToAir          * ("inventory.area" * cohort_proportion)) AS DecaySlowBGToAir,
  SUM(DecaySWStemSnagToAir      * ("inventory.area" * cohort_proportion)) +
  SUM(DecayHWStemSnagToAir      * ("inventory.area" * cohort_proportion)) AS DecayStemSnagToAir,
  SUM(DecaySWBranchSnagToAir    * ("inventory.area" * cohort_proportion)) +
  SUM(DecayHWBranchSnagToAir    * ("inventory.area" * cohort_proportion)) AS DecayBranchSnagToAir
FROM annual_process_flux
-- WHERE
GROUP BY timestep
ORDER BY timestep
