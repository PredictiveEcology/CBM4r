
-- Disturbance emissions for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DisturbanceBioCO2Emission * (c.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCO2Emission * (c.area * a.cohort_proportion)) AS CO2,
  SUM(a.DisturbanceBioCH4Emission * (c.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCH4Emission * (c.area * a.cohort_proportion)) AS CH4,
  SUM(a.DisturbanceBioCOEmission  * (c.area * a.cohort_proportion)) +
  SUM(a.DisturbanceDOMCOEmission  * (c.area * a.cohort_proportion)) AS CO
FROM disturbance_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.chunk_index = b.chunk_index
LEFT JOIN area c ON b.raster_index = c.raster_index and b.chunk_index = c.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

