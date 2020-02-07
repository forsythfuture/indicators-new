library(tidyverse)
library(glue)
library(srvyr)
library(FFtools)

source('global_functions.R')
source('economy/median_home/median_home_functions.R')

# current year of PUMS dataset
current_year <- 2018

county_comparison <- c('Forsyth', 'Guilford', 'Durham')

# create list of links to PUMS housing and population files
years <- seq(2007, current_year)
yr_two <- str_extract(years, "[0-9]{2}$")
pop_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{yr_two}pnc.csv.gz')
house_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_housing/ss{yr_two}hnc.csv.gz')

i <- length(pop_files)

# convert PUMA codes to counties by importing file with codes and counties
puma_file <- 'misc_data/puma_counties.csv'

# create workable dataset
df <- create_pums_data(house_files[i], pop_files[i], puma_file, county_comparison)

# calcualte median household income and se ------------------------------------

rep_weight_seq <- seq(1, 80)
rep_weight_cols <- glue("WGTP{rep_weight_seq}")

grouping_var <- c('rac1p', 'cntyname')

# create survey object
house %>%
  #filter(cntyname == "Forsyth NC") %>%
  as_survey_rep(
    weights = WGTP,
    repweights = !!rep_weight_cols,
    type = "JKn",
    scale = 4/80,
    rscales = rep(1,80),
    combined_weights = T,
  ) %>%
  #group_by_at(grouping_var) %>%
  summarize(median_housing = survey_median(VALP, na.rm = T, vartype = 'se'))
  