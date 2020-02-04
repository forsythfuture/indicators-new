#################################################################################
#
# This script imports and cleans the concentrated poverty data
#
#################################################################################

library(tidyverse)
library(glue)
library(tidycensus)

source('economy/concentrated_poverty/concentrated_poverty_functions.R')

current_year <- 2018

# number of standard error bootstrap iterations
bootstrap_iterations <- 5000

comparison_counties <- c('Forsyth', 'Guilford', 'Durham')

# get 2018 and 2013 poverty by census tract estimates and place in single dataframe
poverty_tract_import <- map_df(c(current_year-5, current_year), get_table, table_num = 'S1701')

# poverty_tract_import <- get_table(var_names= var, table_num = 'S1701', year = 2018)

# create column identifying whether county is ei    ther Forsyth or one of the comparison counties
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
    
# calculate bootstrapped standard errors for state and copmparison counties
total_conc_list <- map_df(c(T, F), function(x) {
    
      total_poverty_conc_agg <- conc_pov(total_poverty_conc, state = x)
      
      # calculate standard errors
      total_poverty_conc_se <- bootstrap_se(conc_pov_df = total_poverty_conc, pov_perc_estimate = poverty_perc$estimate, 
                                            pov_perc_se = poverty_perc$se, num_iterations = bootstrap_iterations, state = x)
      
      df <- left_join(total_poverty_conc_agg, total_poverty_conc_se, 
                                           by = c('comparison', 'year', 'merged_label'))
      
      return(df)
})
    
# clean up final dataset
total_conc_list_cleaned <- total_conc_list %>%
  mutate(
    type = 'Concentrated Poverty',
    subtype = 'Total Population',
    comparison = ifelse(comparison != 'NC', glue("{comparison} County, NC"), comparison)
  ) %>%
  select(geo_description = comparison, year, type, subtype, estimate = perc_in_conc_pov, se_regular, se_bootstrap)

# Percentage living in poverty by race / ethnicity ---------------------------------------

# we only want the most recent year for demographic information, so filter dataset
poverty_tract <- poverty_tract %>%
  filter(year == !!current_year)

poverty_perc <- poverty_perc %>%
  filter(year == !!current_year)

race_pop_re <- "^Estimate!!Total!!Population.*RACE AND.*$|^Total!!Estimate!!Population.*RACE AND.*$"
race_poverty_total_re <- "^Estimate!!Below poverty level.*RACE AND.*$|^Below poverty level!!Estimate!!Population.*RACE AND.*$"

# calculate point estimate for total concentrated poverty
race_poverty_conc <- calc_conc_pov(poverty_tract, poverty_perc, race_pop_re, race_poverty_total_re)

# calculate bootstrapped standard errors for state and copmparison counties
race_conc_list <- map_df(c(T, F), function(x) {
  
  race_poverty_conc_agg <- conc_pov(race_poverty_conc, state = x)
  
  # calculate standard errors
  race_poverty_conc_se <- bootstrap_se(conc_pov_df = race_poverty_conc, pov_perc_estimate = poverty_perc$estimate, 
                                        pov_perc_se = poverty_perc$se, num_iterations = bootstrap_iterations, state = x)
  
  df <- left_join(race_poverty_conc_agg, race_poverty_conc_se, 
                  by = c('comparison', 'year', 'merged_label'))
  
  return(df)
})

# clean up final dataset

# needed ethnicities for filtering
race_ethnicity <- c(`White alone, not Hispanic or Latino` = "White, non-Hispanic",
                    `Hispanic or Latino origin (of any race)` = "Hispanic / Latino",
                    `Black or African American alone` = "African American")

# clean up labels
race_conc_list_clean <- race_conc_list %>%
  # extract only text of race
  mutate(merged_label = str_extract(merged_label, "(?<=LATINO ORIGIN!!).*$")) %>%
  # filter to only keep needed race / ethnicity
  filter(merged_label %in% !!names(race_ethnicity)) %>%
  mutate(merged_label = recode(merged_label, !!!race_ethnicity),
         type = 'Race / Ethnicity',
         comparison = ifelse(comparison != 'NC', glue("{comparison} County, NC"), comparison)) %>%
  select(geo_description = comparison, year, type, subtype = merged_label, estimate = perc_in_conc_pov, se_regular, se_bootstrap)

# by age ----------------------------------------------

age_pop_re <- "^Estimate!!Total!!Population.*AGE.*$|^Total!!Estimate!!Population.*AGE.*$"
age_poverty_total_re <- "^Estimate!!Below poverty level.*AGE.*$|^Below poverty level!!Estimate!!Population.*AGE.*$"

# calculate point estimate for total concentrated poverty
age_poverty_conc <- calc_conc_pov(poverty_tract, poverty_perc, age_pop_re, age_poverty_total_re)

# calculate bootstrapped standard errors for state and copmparison counties
age_conc_list <- map_df(c(T, F), function(x) {
  
  age_poverty_conc_agg <- conc_pov(age_poverty_conc, state = x)
  
  # calculate standard errors
  age_poverty_conc_se <- bootstrap_se(conc_pov_df = age_poverty_conc, pov_perc_estimate = poverty_perc$estimate, 
                                       pov_perc_se = poverty_perc$se, num_iterations = bootstrap_iterations, state = x)
  
  df <- left_join(age_poverty_conc_agg, age_poverty_conc_se, 
                  by = c('comparison', 'year', 'merged_label'))
  
  return(df)
})

# clean ages

# we only want to keep under 18, 18-64, and 65 and over
age_filter <- c(`Under 18 years` = "18 years old and under",
                `18 to 64 years` = "18 to 64 years old",
                `65 years and over` = "65 years old and over")

age_conc_list_cleaned <- age_conc_list %>%
    mutate(merged_label = str_remove(merged_label, "^Population.*AGE!!")) %>%
    filter(merged_label %in% !!names(age_filter)) %>%
    mutate(merged_label = recode(merged_label, !!!age_filter),
           type = 'Age',
           comparison = ifelse(comparison != 'NC', glue("{comparison} County, NC"), comparison)) %>%
    select(geo_description = comparison, year, type, subtype = merged_label, estimate = perc_in_conc_pov, se_regular, se_bootstrap)

# combine all datasets and write out
conc_poverty_combined <- bind_rows(list(total_conc_list_cleaned, race_conc_list_clean, age_conc_list_cleaned)) %>%
  # only keep regualr standard error
  select(-se_bootstrap) %>%
  rename(se = se_regular)

write_csv(conc_poverty_combined, "economy/concentrated_poverty/cleaned_data/conc_poverty_demograhic.csv")
