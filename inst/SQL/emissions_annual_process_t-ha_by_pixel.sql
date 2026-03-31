
-- Annual process emissions density for each pixel
-- Units: tonnes per hectare (t/ha)

SELECT
  a.timestep,
  b.raster_index,
  b.chunk_index,
  SUM(a.DecayDOMCO2Emission * (a.cohort_proportion)) AS CO2,
  0 AS CH4,
  0 AS CO,
FROM annual_process_flux a LEFT JOIN raster_index b ON a.index = b.index
-- WHERE
GROUP BY a.timestep, b.raster_index, b.chunk_index
ORDER BY a.timestep, b.raster_index, b.chunk_index

