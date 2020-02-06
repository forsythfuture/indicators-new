###############################################################################
#
# This file imports and cleans the PUMS data for income quintiles and creates
# two cleaned datasets:
#  1) income quintiels by race;
#  2) percentage of people in each race residing in quintiles
#
# The 'current_year' object is the only section that needs to be updated
#
###############################################################################

library(tidyverse)
library(glue)
library(FFtools)

# update to current year
current_year <- 2018

# to year to enter into file name, convert year to last two digits
yr_two <- str_extract(current_year, "[0-9]{2}$")

# get race quintiles from PUMS -------------------------------
  
# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'TYPE', # Type of husing unit; 1 is housing unit, which are the only units we need
                'HINCP') # housing costs as a percentage of income

# population variables that are needed
# the name of one population variable depends on year, so these variables
# must be defined inside the year loop
pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
              # relationship to reference person; variable name changed in 2010
              'RELP',
              'ST', # state
              'PUMA', # PUMA code
              'AGEP', # age
              'RAC1P', # race
              'HISP') # hispanic origin

# must define weight variable names within year loop because names change dpending on year
# replciate weight variable names are lower case until 2017 and upper case starting in 2017
weight_names <- 'WGTP'
replicate_weights <- c('WGTP', paste0(weight_names, seq(1, 80)))

# add the year's replicate weights to housing variables
house_yr_vars <- c(house_vars, replicate_weights)

# import and clean population variables
pop <- read_csv(glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{yr_two}pnc.csv.gz'),
                col_types = cols(.default = "c")) %>%
  # only keep needed variables
  select(!!pop_vars)

pop <- pop %>%
  # only keep Forsyth County PUMAs
  filter(str_detect(PUMA, '1801|1802|1803'))

# only keep head of household and recode race
pop <- pop %>%
  filter(RELP == '00') %>%
  # remove unneeded columns
  select(-RELP, -PUMA, -ST) %>%
  ff_pums_ethnicity()

# import housing variables
house <- read_csv(glue('https://censuspums.s3.amazonaws.com/oneyear/nc_housing/ss{yr_two}hnc.csv.gz'),
                  col_types = cols(.default = "c")) %>%
  # only keep needed variables
  select(!!house_yr_vars)

house <- house %>%
  filter(str_detect(PUMA, '1801|1802|1803'), # only keep Forsyth County
         # only keep housing units; remove institutional units
         TYPE ==1,
         # do not keep rows with NA values for income
         !is.na(HINCP)) %>%
  # remove unneeded columns
  select(-TYPE)

# merge housing and population
incomes_race <- left_join(house, pop, by = 'SERIALNO') %>%
  select(-ST, -PUMA)

# calculate income quintile information for PUMS --------------------------

# calculate income quintile percentages for each year and all replciate weights
quant_probs <- c(.2, .4, .6, .8)

incomes_race_quants <- incomes_race %>%
    mutate(HINCP = as.numeric(HINCP)) %>%
    group_by(RAC1P) %>% 
    group_modify(~{
      quantile(.x$HINCP, probs = quant_probs) %>% 
        tibble::enframe()
    })

# recode quintiles
map_quintiles <- c(`20%` = "20th Percentile",
                   `40%` = "40th Percentile",
                   `60%` = "60th Percentile",
                   `80%` = "80th Percentile")

incomes_race_quants <- incomes_race_quants %>%
  filter(RAC1P != "Other") %>%
  mutate(name = recode(name, !!!map_quintiles))

# put in proper format
incomes_race_quants <- incomes_race_quants %>%
  mutate(geo_description = "Forsyth County, NC",
         year = !!current_year,
         type = "Race / Ethnicity") %>%
  rename(subtype = RAC1P, description = name, estimate = value)

# merge PUMS and ACS data -----------------------------------------

# import ACS data

acs <- read_csv("economy/income_quintile/cleaned_data/income_quintile_acs_update.csv")

income_quintiles <- bind_rows(acs, incomes_race_quants)

write_csv(income_quintiles, "economy/income_quintile/cleaned_data/income_quintile_full_update.csv")

# create race percentiles ----------------------------------------------

# pull out ACS percentiles for Forsyth County
quintiles <- acs %>%
  filter(year == !!current_year,
         geo_description == "Forsyth County, NC",
         description != "95th Percentile") %>%
  select(estimate) %>%
  .[[1]]

# add low and high numbers to quintiles, so when we bin we have full categories
quintiles <- c(-10000, quintiles, 9999999)
labels <- c("20th Percentile and Under", "20th to 40th Percentile", 
            "40th to 60th Percentile", "60th to 80th Percentile", "80th Percentile and Above")

incomes_race$HINCP <- as.numeric(incomes_race$HINCP)

# bin PUMS incomes by quintile
incomes_race$percentile <- cut(incomes_race$HINCP, breaks = quintiles, 
                               labels = labels, include.lowest = T)

incomes_race <- incomes_race %>%
  # remove replicate weights but keep primary weight
  select(-matches("WGTP[0-9]+")) %>%
  # extend out dataset based on primary weight
  mutate(WGTP = as.numeric(WGTP)) %>%
  uncount(WGTP)

# calculate percent of each race in each quintile
perc_quintiles <- incomes_race %>%
  # count number in each race that are in each percentile
  group_by(RAC1P, percentile) %>%
  count() %>%
  # count total in each race
  group_by(RAC1P) %>%
  mutate(race_total = sum(n)) %>%
  # calcualte percent of total race population in each quintile
  mutate(percent_in_quintile = n / race_total) %>%
  rename(race_quintile = n, race = RAC1P) %>%
  filter(race != "Other") %>%
  mutate(geo_description = "Forsyth County, NC",
         year = !!current_year) %>%
  select(geo_description, year, everything())

write_csv(perc_quintiles, "economy/income_quintile/cleaned_data/income_quintile_race_percentage_update.csv")
