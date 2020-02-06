# Violent Crime

**Data Source:** NC State Bureau of Investigation, Crime Trends

http://crimereporting.ncsbi.gov/Reports.aspx

### Pulling data

Ten year data for NC and all comparison counties was clicked on an copied to a csv file 'safety/violent_crime/raw_data/crime_raw.csv`. Data for rates - under the link 'County Rates, Ten Year Trend' - and data for numbers - under the link 'County Offenses, Ten Year Trend' - were both added to this same raw data file.

yearly updates to this indicator only require manually updating the raw data and updating the county popualtion dataset.  County population numbers are needed so that standard errors can be created. Those are pulled from `misc_data/county_state_pop.csv`, which is a dataset of county and state populations through time. This dataset is created in `update_county_populations.R` and will need to be updated yearly, prior to updating the crime indicator.

Once these two datasets are updated, `safety/violent_crime/violent_crime_clean.R` can be ran without modification to update the final, cleaned dataset `safety/violent_crime/clean_data/violent_crime_clean.csv`.