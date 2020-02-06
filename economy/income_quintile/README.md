# Income by quintile

### Data Sources:

- US Census, The American Community Survey (ACS), One-Year Estimate, 2007 - Current Year
- US Census, The American Community Survey (ACS) Public Use Microdata Sample (PUMS), One-Year file, Current Year

### Outline of folder

The regular ACS dataset and the PUMS dataset are used to calculate income by quintile. The ACS is used to calculate each county's total income by quintile and the PUMS is used to calculate incomes by quintile within each race / ethnicity. Different files are used to work with each dataset, as noted in their file names.

In looking at income by quintiles and race / ethnicity (calculated in `income_quintile_pums_clean.R`), two types of output are created. The first is traditional income by quintiles. The second calculates the percentage of Forsyth County residents within each race / ethnicity residing within each quintile. The final output for this second metric is found in `cleaned_data/income_quintile_race_percentage_update.csv`.

The final cleaned dataset that contains income by quintile at the total county level and of race / ethnicities is found in `cleaned_data/income_quintile_full_update.csv`.

When updating each year, `income_quintile_acs_clean.R` should be ran first, followed by `income_quintile_pums_clean.R`. For each file, the only part that needs to be updated is the `current_year` object within the file.