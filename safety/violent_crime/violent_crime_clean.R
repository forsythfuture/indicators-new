library(tidyverse)
library(glue)

crime <- read_csv("safety/violent_crime/raw_data/crime_raw.csv") %>%
  select(-index_rate)

pop <- read_csv('misc_data/county_state_pop.csv') %>%
  select(-geo_category)

# convert to long form where all crimes are in same column
crime <- crime %>%
  pivot_longer(cols = violent:arson) %>%
  drop_na(value) %>%
  filter(name %in% c('violent', 'assault', 'murder', 'rape', 'robbery')) %>%
  # convert to wide form where rate and number are on different columns
  pivot_wider(names_from = 'estimate', values_from = 'value')

# violent crime does not have a number for counties, so we need to create it by summing sub-components
number_violent <- crime %>%
  mutate(is_violent = ifelse(name == 'violent', T, F)) %>%
  group_by(year, geo_description, is_violent) %>%
  summarize(number_violent = sum(number, na.rm = T)) %>%
  filter(is_violent == FALSE) %>%
  select(-is_violent)

# add number of violent to main crime dataset
crime <- crime %>%
  left_join(number_violent, by = c('year', 'geo_description')) %>%
  # make the number column be the number of violent crimes in the newly matched
  # dataset for violent crime rows
  mutate(number = ifelse(name == 'violent', number_violent, number)) %>%
  select(-number_violent) %>%
  mutate(geo_description = ifelse(geo_description != "North Carolina", 
                                  glue("{geo_description} County, NC"), geo_description))

# add county or state population to main dataset, so we can calcualte se
crime <- crime %>%
  left_join(pop, by = c('geo_description', 'year')) %>%
  # standard error calculation from poisson distribution: 
  # https://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm
  mutate(se = (sqrt(number)/population)*100000,
         type = "Crime Rate",
         name = str_to_title(name),) %>%
  select(year, geo_description, type, subtype = name, estimate = rate, se)

write_csv(crime, 'safety/violent_crime/clean_data/violent_crime_clean.csv')
