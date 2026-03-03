
-- Total emissions by timestep (MgC/year)

SELECT
  a.timestep,
  SUM(COALESCE(a.DecayDOMCO2Emission       * (a."inventory.area" * a.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceBioCO2Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCO2Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceBioCH4Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCH4Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceBioCOEmission    * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCOEmission    * (d.area * d.cohort_proportion), 0)) AS Emissions,
  SUM(COALESCE(a.DecayDOMCO2Emission       * (a."inventory.area" * a.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceBioCO2Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCO2Emission   * (d.area * d.cohort_proportion), 0)) AS CO2,
  SUM(COALESCE(DisturbanceBioCH4Emission   * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCH4Emission   * (d.area * d.cohort_proportion), 0)) AS CH4,
  SUM(COALESCE(DisturbanceBioCOEmission    * (d.area * d.cohort_proportion), 0)) +
  SUM(COALESCE(DisturbanceDOMCOEmission    * (d.area * d.cohort_proportion), 0)) AS CO,
  SUM(COALESCE(a.DecayDOMCO2Emission       * (a."inventory.area" * a.cohort_proportion), 0)) AS DecayDOMCO2Emission,
  SUM(COALESCE(d.DisturbanceCO2Production  * (d.area * d.cohort_proportion), 0)) AS DisturbanceCO2Production,
  SUM(COALESCE(d.DisturbanceCH4Production  * (d.area * d.cohort_proportion), 0)) AS DisturbanceCH4Production,
  SUM(COALESCE(d.DisturbanceCOProduction   * (d.area * d.cohort_proportion), 0)) AS DisturbanceCOProduction,
  SUM(COALESCE(d.DisturbanceBioCO2Emission * (d.area * d.cohort_proportion), 0)) AS DisturbanceBioCO2Emission,
  SUM(COALESCE(d.DisturbanceBioCH4Emission * (d.area * d.cohort_proportion), 0)) AS DisturbanceBioCH4Emission,
  SUM(COALESCE(d.DisturbanceBioCOEmission  * (d.area * d.cohort_proportion), 0)) AS DisturbanceBioCOEmission,
  SUM(COALESCE(d.DisturbanceDOMCO2Emission * (d.area * d.cohort_proportion), 0)) AS DisturbanceDOMCO2Emission,
  SUM(COALESCE(d.DisturbanceDOMCH4Emission * (d.area * d.cohort_proportion), 0)) AS DisturbanceDOMCH4Emission,
  SUM(COALESCE(d.DisturbanceDOMCOEmission  * (d.area * d.cohort_proportion), 0)) AS DisturbanceDOMCOEmission,
FROM annual_process_flux a LEFT JOIN disturbance_flux d ON a.timestep = d.timestep AND a.index = d.index
-- WHERE
GROUP BY a.timestep ORDER BY a.timestep

