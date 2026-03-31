
-- Disturbance emissions for each timestep
-- Units: tonnes (t)

SELECT
  timestep,
  SUM(DisturbanceBioCO2Emission * (area * cohort_proportion)) +
  SUM(DisturbanceDOMCO2Emission * (area * cohort_proportion)) AS CO2,
  SUM(DisturbanceBioCH4Emission * (area * cohort_proportion)) +
  SUM(DisturbanceDOMCH4Emission * (area * cohort_proportion)) AS CH4,
  SUM(DisturbanceBioCOEmission  * (area * cohort_proportion)) +
  SUM(DisturbanceDOMCOEmission  * (area * cohort_proportion)) AS CO
FROM disturbance_flux
-- WHERE
GROUP BY timestep
ORDER BY timestep

