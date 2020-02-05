library(tidyverse)

comp_counties <- c('Forsyth|Guilford|Durham')

df <- read_tsv('misc_data/bridge_race_06-18.txt') %>%
  select(-ends_with('Code'))

colnames(df) <- c('notes', 'state', 'county', 'year', 'race', 'ethnicity', 'population')

# create total county and state populations -----------------

# only keep total categories
total <- df %>%
  drop_na(notes, year) %>%
  filter(is.na(race)) %>%
  select(geo_description = county, year, population) %>%
  mutate(geo_category = 'County')

# create state population from sum of county populations
state <- total %>%
  group_by(year) %>%
  summarize(population = sum(population)) %>%
  mutate(geo_description = "North Carolina",
         geo_category = 'State')

# add state to county dataframe
total <- bind_rows(total, state)

write_csv(total, "misc_data/county_state_pop.csv")
