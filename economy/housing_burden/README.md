# Housing Burden

**Data Source:** The American Community Survey (ACS) Public Use Microdata Sample (PUMS), One-Year file, 2007 - Current Year

## Outline of folder

`1-housing_burden_clean.R` is the first file to run and imports the PUMS data and creates the initial dataset. If the most recent year PUMS files are added to the share drive with all the PUMS files, the only variable that needs to be updated is the `current_year` variable on line 14. The script will recreate all years and output a data file that can be used to calculate housing burden.

The second file to run is `2-housing_burden_calculate.R`. No variables need to be changed. It imports the data created from `1-housing_burden_clean.R`, calculates housing burden, and creates the final dataset.

The final dataset can be found in `economy/housing_burden/clean_data/housing_burden_final.csv`.
