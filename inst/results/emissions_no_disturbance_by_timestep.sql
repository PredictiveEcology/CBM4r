
-- Total emissions by timestep (MgC/year) without disturbances

SELECT
  a.timestep,
  SUM(COALESCE(a.DecayDOMCO2Emission * (a."inventory.area" * a.cohort_proportion), 0)) AS Emissions,
  SUM(COALESCE(a.DecayDOMCO2Emission * (a."inventory.area" * a.cohort_proportion), 0)) AS CO2,
  0 AS CH4,
  0 AS CO,
  SUM(COALESCE(a.DecayDOMCO2Emission * (a."inventory.area" * a.cohort_proportion), 0)) AS DecayDOMCO2Emission,
  0 AS DisturbanceCO2Production,
  0 AS DisturbanceCH4Production,
  0 AS DisturbanceCOProduction,
  0 AS DisturbanceBioCO2Emission,
  0 AS DisturbanceBioCH4Emission,
  0 AS DisturbanceBioCOEmission,
  0 AS DisturbanceDOMCO2Emission,
  0 AS DisturbanceDOMCH4Emission,
  0 AS DisturbanceDOMCOEmission,
FROM annual_process_flux a
-- WHERE
GROUP BY a.timestep ORDER BY a.timestep

