# Big Data with Spark: Tree Sparrow Observations from GBIF in combination with Climate and Land Use Variables

This project forms part of the C7084 Assignment.

Link to Github Respoitory https://github.com/FlorenceGalliers/big-data

Files in Repository:
- [data-preparation.R](./data-preparation.R) contains R code for extracting climate and land use data for the observation points and pulling it together into one dataset.
- [bigdata-assignment.R](./bigdata-assignment.R) contains all R code for EDA and analysis using the Spark framework.

- [report.pdf](./report.pdf) contains the final report for submission.
- [report.Rmd](./report.Rmd) is the R Markdown file used to create the above report.

- [passer-montanus](./passer-montanus) is the csv file containing the raw downloaded data from GBIF for the Tree Sparrow (*Passer montanus*). 
- [pm-data.csv](./pm-data.csv) contains the observations and extracted climate and land use variables. (decimalLongitude, decimalLatitude, year, tas, tasmin, tasmax, rainfall, hurs, land). NAs are dropped.
- [grid.csv](./grid.csv) .csv file containing the count of observations per grid cell.
- [final_data.csv](./final_data.csv) the final dataset combining counts per grid cell, averaged climate data, most popular land use classification. This is taken forward into the Spark framework for analysis.

- **plots** folder has all of the plots and images contained in the report.
- **grid_data** series of .csv files containing count data (this is aggregated in "grid.csv"
- **data** folder contains all raw climate data (tas, tasmin, tasmax, rainfall and hurs) and land use data for every year 2001-2019.
 
