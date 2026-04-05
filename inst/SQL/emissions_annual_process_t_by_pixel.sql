
-- Annual process emissions for each pixel
-- Units: tonnes (t)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DecayDOMCO2Emission * (c.area * a.cohort_proportion)) AS CO2,
  0 AS CH4,
  0 AS CO,
FROM annual_process_flux a
LEFT JOIN raster_index b ON a.timestep = b.timestep AND a.cohort_index = b.cohort_index AND a.chunk_index = b.chunk_index
LEFT JOIN area c ON b.raster_index = c.raster_index and b.chunk_index = c.chunk_index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

