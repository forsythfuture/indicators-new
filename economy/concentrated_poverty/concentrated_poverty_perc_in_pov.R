#################################################################################
#
# This script calcualtes the percentage of residents who are in poverty that are 
# also in census tracts of concentrated poverty
#
# mathematicaly, it is the sum of those living in poverty AND in a concentrated 
# poverty census tract divided by the total county number of people in poverty
#
#################################################################################

library(tidyverse)
library(glue)
library(tidycensus)

source('economy/concentrated_poverty/concentrated_poverty_functions.R')

current_year <- 2018

comparison_counties <- c('Forsyth', 'Guilford', 'Durham')

# get 2018 and 2013 poverty by census tract estimates and place in single dataframe
poverty_tract_import <- map_df(c(current_year-5, current_year), get_table, table_num = 'S1701')

# create column identifying whether county is either Forsyth or one of the comparison counties
county_re <-paste(comparison_counties, collapse = '|') # regular expression to extract all comparison counties

poverty_tract <- poverty_tract_import %>%
  mutate(comparison = str_detect(NAME, county_re),
         comparison = ifelse(comparison == TRUE, str_extract(NAME, county_re), 'State'),
         se = moe / 1.96) %>%
  select(-moe)

# percentage of residents living in concentrated poverty -------------------------------------
# create column identifying whether census tract is in concentrated poverty

# labels in current and previous years have different descriptions, so this expression extracts both
poverty_perc_re <- "^Estimate!!Percent below poverty level!!Population for whom poverty status is determined$|^Percent below poverty level!!Estimate!!Population for whom poverty status is determined$"

# filter for rows that contain overall poverty percent of census tract
poverty_perc <- poverty_tract %>%
  filter(str_detect(label, !!poverty_perc_re)) %>%
  # boolean identifying whether census tract is over 40% poverty rate (concentrated poverty)
  mutate(conc_poverty = ifelse(estimate > 40, T, F))

# total poverty concentration ----------------------------------
# extracts labels for old and new years
total_pop_re <- "^Total!!Estimate!!Population for whom poverty status is determined$|^Estimate!!Total!!Population for whom poverty status is determined$"
poverty_total_re <- "^Below poverty level!!Estimate!!Population for whom poverty status is determined$|^Estimate!!Below poverty level!!Population for whom poverty status is determined$"

# calculate point estimate for total concentrated poverty
total_poverty_conc <- calc_conc_pov(poverty_tract, poverty_perc, total_pop_re, poverty_total_re)

# people in poverty ----------------------------------------------------------

poverty_perc_cal <- left_join(poverty_tract, poverty_perc[c('NAME', 'conc_poverty', 'year')], by = c('NAME', 'year'))

# filter for rows that contain overll number of people in poverty per census tract
total_pop <- poverty_perc_cal %>%
  filter(str_detect(label, !!total_pop_re)) %>%
  mutate(merged_label = str_extract(label, 'Population.*')) %>%
  select(NAME, total_estimate = estimate, total_se = se, year, comparison, conc_poverty)

poverty_total <- poverty_perc_cal %>%
  filter(str_detect(label, !!poverty_total_re)) %>%
  mutate(merged_label = str_extract(label, 'Population.*')) %>%
  select(NAME, poverty_estimate = estimate, poverty_se = se, year, comparison)

# combine the poverty percent and poverty total datasets
conc_pov_perc <- left_join(total_pop, poverty_total, by = c('NAME', 'year', 'comparison'))

# calculate concentrated poverty percentage for counties and state
conc_pov_perc <- map_df(c(T, F), poverty_and_conc_poverty, df = conc_pov_perc)

# clean up dataframe
conc_pov_perc <- conc_pov_perc %>%
  mutate(comparison = ifelse(comparison != "NC", glue("{comparison} County, NC"), "North Carolina"),
         type = "Poverty and Conc Poverty",
         subtype = "Total Population") %>%
  select(geo_description = comparison, year, type, subtype, estimate, se)

write_csv(conc_pov_perc, "economy/concentrated_poverty/cleaned_data/conc_poverty_in_poverty.csv")  
