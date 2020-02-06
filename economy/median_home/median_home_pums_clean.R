# TEN		1
# Tenure
# b .N/A  (GQ/vacant)
# 1 .Owned with mortgage or loan (include home equity loans)
# 2 .Owned free and clear
# 3 .Rented
# 4 .Occupied without payment of rent

library(tidyverse)
library(glue)
library(srvyr)
library(FFtools)

source('global_functions.R')

# current year of PUMS dataset
current_year <- 2018

county_comparison <- c('Forsyth', 'Guilford', 'Durham')

# create list of links to PUMS housing and population files
years <- seq(2007, current_year)
yr_two <- str_extract(years, "[0-9]{2}$")
pop_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{yr_two}pnc.csv.gz')
house_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_housing/ss{yr_two}hnc.csv.gz')

i <- length(pop_files)

# import and filter housing data --------------------------------

# read in and filter data
house <- read_csv(house_files[i],
                  col_types = cols(SERIALNO = col_character(),
                                   RT = col_character(),
                                  .default = col_double())) %>%
  select(SERIALNO, RT, ST, PUMA, TYPE, VALP, TEN, matches('^wgtp.*|^WGTP.*'), ACR) %>%
  filter(
    ST == 37, # only keep north carolina
    str_detect(RT, "^H|^h"), # only keep housing quarters
    !is.na(VALP), # remove all rows with missing values for home value
    TEN %in% c(1, 2), # only keep home owners
    TYPE == 1,
    !is.na(ACR)
  )

# convert PUMA codes to counties by importing file with codes and counties
puma_file <- 'misc_data/puma_counties.csv'

puma_codes <- puma_area_code(county_comparison, puma_file, T) %>%
  distinct()

house <- house %>%
  left_join(puma_codes, by = 'PUMA') 

# import and filter population data ------------------------------

pop <- read_csv(pop_files[i],
                col_types = cols(RT= col_character(),
                                NAICSP = col_character(),
                                SOCP = col_character(),
                                SERIALNO = col_character(),
                                HISP = col_character(),
                               .default = col_double())) %>%
  select(SERIALNO, matches('^REL$|^RELP$'), ST, AGEP, PUMA, RAC1P, HISP) %>%
  filter(ST == 37)

# only keep head of household
pop <- pop[pop[[2]] == 0,]

# remove household status column and state
pop <- pop[c('SERIALNO', 'AGEP', 'PUMA', 'RAC1P', 'HISP')]

# convert race / ethnicity to one column
pop <- ff_pums_ethnicity(pop) %>%
  # hispanic column is no longer needed
  select(-HISP)

# bind population with household dataset
joined <- house %>%
  left_join(pop, by = c('SERIALNO', 'PUMA')) %>%
  # reorder variables to ensure common order for renaming
  select(cntyname, AGEP, RAC1P, VALP, matches("wgtp|WGTP")) %>%
  # rename all column names to lower case, so they are standardized for all years
  rename_all(~tolower(.))

rm(house, pop, puma_codes)
gc()

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
  