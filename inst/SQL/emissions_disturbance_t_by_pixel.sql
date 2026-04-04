
-- Disturbance emissions for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DisturbanceBioCO2Emission * (a.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCO2Emission * (a.area * a.cohort_proportion)) AS CO2,
  SUM(a.DisturbanceBioCH4Emission * (a.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCH4Emission * (a.area * a.cohort_proportion)) AS CH4,
  SUM(a.DisturbanceBioCOEmission  * (a.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCOEmission  * (a.area * a.cohort_proportion)) AS CO
FROM disturbance_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.chunk_index = b.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

