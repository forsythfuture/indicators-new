###################################################################################
#
# This file imports and cleans the ACS data for quintiles, which is the
# US, NC, and county totals
#
####################################################################################

library(tidyverse)
library(tidycensus)
library(FFtools)

# update total income quintiles for latest year -----------------

current_year <- 2018
comparison_counties <- c("United States", "North Carolina", "Forsyth County, NC", "Guilford County, NC", "Durham County, NC")

parameters <- list(geography = c('us', 'state', 'county'),
                   state = c(NA, rep('NC', 2)),
                   county = rep(NA, 3),
                   table = rep('B19080', 3),
                   survey = rep('acs1', 3),
                   year = rep(current_year, 3),
                   use_table = rep(T, 3))

# import table
quintiles <- ff_iterate_acs(parameters)

# filter for needed counties
quintiles <- quintiles %>%
  filter(geo_description %in% !!comparison_counties) %>%
  select(-concept, -geography)


# import and clean past year's data ------------------------------
quintiles_past <- read_csv("economy/income_quintile/raw_data/income_quintile_06_17.csv") %>%
  select(-label, -moe, -cv)

# merge and clean current and past year's datasets -------------------------
quintiles_update <- bind_rows(quintiles_past, quintiles)

# clean up the quintile descriptions
quintiles_update <- quintiles_update %>%
  mutate(description = str_replace(description, ".*Lowest.*", "20th Percentile"),
         description = str_replace(description, ".*Second.*", "40th Percentile"),
         description = str_replace(description, ".*Third.*", "60th Percentile"),
         description = str_replace(description, ".*Fourth.*", "80th Percentile"),
         description = str_replace(description, ".*Top 5.*", "95th Percentile")) %>%
  mutate(type = "Income by Quintiles",
         subtype = "Total Population")

write_csv(quintiles_update, 'economy/income_quintile/cleaned_data/income_quintile_acs_update.csv')
