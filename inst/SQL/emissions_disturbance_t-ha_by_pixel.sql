
-- Disturbance emissions density for each pixel
-- Units: tonnes per hectare (t/ha)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DisturbanceBioCO2Emission * (a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCO2Emission * (a.cohort_proportion)) AS CO2,
  SUM(a.DisturbanceBioCH4Emission * (a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCH4Emission * (a.cohort_proportion)) AS CH4,
  SUM(a.DisturbanceBioCOEmission  * (a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCOEmission  * (a.cohort_proportion)) AS CO
FROM disturbance_flux a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

