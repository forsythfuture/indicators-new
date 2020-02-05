library(tidyverse)

crime <- read_csv("safety/violent_crime/raw_data/crime_raw.csv") %>%
  select(-index_rate)

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

# (number / population) * per_capita = rate
# add number of violent to main crime dataset
crime <- crime %>%
  left_join(number_violent, by = c('year', 'geo_description')) %>%
  # make the number column be the number of violent crimes in the newly matched
  # dataset for violent crime rows
  mutate(number = ifelse(name == 'violent', number_violent, number)) %>%
  select(-number_violent)

# infer population total from number of crimes and rate, so we can calculate standard error
crime1 <- crime %>%
  # use population inferred from rate, to 
  mutate(population = number*100000/rate,
         # standard error calculation from poisson distribution: 
         # https://seer.cancer.gov/seerstat/WebHelp/Rate_Algorithms.htm
         se = (sqrt(number)/population)*100000)
