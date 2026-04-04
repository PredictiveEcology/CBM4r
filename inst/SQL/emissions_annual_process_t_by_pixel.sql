
-- Annual process emissions for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DecayDOMCO2Emission * (a."inventory.area" * a.cohort_proportion)) AS CO2,
  0 AS CH4,
  0 AS CO,
FROM annual_process_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

