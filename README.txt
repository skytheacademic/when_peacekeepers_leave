"./scripts"
- clean.R: Merges and preprocesses PRIO grid, RADPKO peacekeeping deployment, and ACLED conflict event datasets into a unified panel structure with spatial neighbor calculations.

- analyze.R: Executes all models in the paper and appendix.

- plot.R: Generates tables and figures from model results, including statistical significance formatting and LaTeX output for manuscript integration.

- verify_data.R: Allows for manual verification of edge cases in spatial conflict data by examining specific grid cells and their neighbors to ensure data quality around data transformation.

"./results"
- provides the plots, figures, tables, and all results for the analyses in the paper.

"./data"

ACLED data:
	- Contains data on armed conflict and violence.
	- Observed at event-level, with day and latitude and longitude.
	- Downloaded from https://acleddata.com/data-export-tool/ on June 3, 2022.
	- "1999-01-01-2021-12-31.csv" is the original CSV file downloaded by SK.

PRIO data:
	- Contains geographic and spatial data.
	- Observed at the grid-cell level for static variables, and grid-cell-year level for yearly data.
	- Downloaded from https://grid.prio.org/#/download on June 3, 2022.
	- "PRIO-GRID Static Variables - 2022-06-03.csv" is the original static CSV file downloaded by ZW.
	- "PRIO-GRID Yearly Variables for 1999-2014 - 2022-06-03.csv" is the original yearly CSV file downloaded by ZW.
	- The "priogrid_cellshp" shapefiles were downloaded from https://grid.prio.org/#/extensions on June 3, 2022 by ZW and contain all of the shapefile information.

RADPKO data:
	- Contains data on peacekeeping operation deployments.
	- Observed at the grid-cell-month level.
	- Downloaded from https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/BQU5VD (version 4) on June 3, 2022.
	- "radpko_grid.csv" is the original CSV file downloaded by ZW.
