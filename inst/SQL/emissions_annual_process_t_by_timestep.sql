
-- Annual process emissions for each timestep
-- Units: tonnes (t)

SELECT
  timestep,
  SUM(DecayDOMCO2Emission * ("inventory.area" * cohort_proportion)) AS CO2,
  0 AS CH4,
  0 AS CO,
FROM annual_process_flux
-- WHERE
GROUP BY timestep
ORDER BY timestep

