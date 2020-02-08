library(tidyverse)
library(glue)
library(srvyr)
library(FFtools)

source('global_functions.R')
source('economy/median_home/median_home_functions.R')

# current year of PUMS dataset
current_year <- 2018

county_comparison <- c('Forsyth', 'Guilford', 'Durham')

# create PUMA file link to housing an population files for most recent year
yr_two <- str_extract(current_year, "[0-9]{2}$")
pop_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{yr_two}pnc.csv.gz')
house_file <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_housing/ss{yr_two}hnc.csv.gz')

# file to convert PUMA codes to counties by importing file with codes and counties
puma_file <- 'misc_data/puma_counties.csv'

# create workable dataset
housing <- create_pums_data(house_file, pop_file, puma_file, county_comparison)

  # calcualte median household income and se ------------------------------------

rep_weight_seq <- seq(1, 80)
rep_weight_cols <- glue("wgtp{rep_weight_seq}")
  
# create survey object
housing_survey <- housing %>%
  as_survey_rep(
    weights = wgtp,
    repweights = !!rep_weight_cols,
    type = "JKn",
    scale = 4/80,
    rscales = rep(1,80),
    combined_weights = T
  )

# calculate median housing for race and age - state and counties --------------------------

# race and age for state and counties
housing_race <- map_df(c(T, F), calc_med_housing, housing_survey = housing_survey, 
                       demo_col = 'rac1p', type_text = 'Race / Ethnicity')
housing_age <- map_df(c(T, F), calc_med_housing, housing_survey = housing_survey, 
                      demo_col = 'agep', type_text = 'Age')

# bind race and age
housing_results <- bind_rows(housing_race, housing_age) %>%
  drop_na(estimate) %>%
  # remove 'Other' race / ethnicity
  filter(subtype != "Other") %>%
  mutate(geo_description = str_replace(geo_description, "NC", "County, NC"))

write_csv(housing_results, 'economy/median_home/cleaned_data/median_home_cleaned_demo.csv')
