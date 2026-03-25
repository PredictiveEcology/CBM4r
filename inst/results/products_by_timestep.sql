
-- Total products by timestep (MgC/year)

SELECT
  timestep,
  SUM("pools.Products" * ("inventory.area" * cohort_proportion)) AS Products
FROM simulation
-- WHERE
GROUP BY timestep
ORDER BY timestep

