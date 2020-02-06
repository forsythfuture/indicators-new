library(tidyverse)
library(tidycensus)
library(FFtools)

# update this parameter yearly 
# (this should be the only item that changes each year)
current_year <- 2018

# comparison counties
counties <- c("Forsyth", "Guilford", "Durham")

# pull in most recent year's data from census api -------------

# pull in US, NC, and county poverty data

counties <- get_acs(geography = "county",
                  state = "NC",
                  county = counties,
                  table = "S1701",
                  variables = NULL,
                  survey = "acs1",
                  year = current_year)

nc <- get_acs(geography = "state",
              state = "NC",
              table = "S1701",
              variables = NULL,
              survey = "acs1",
              year = current_year)

us <- get_acs(geography = "US",
              table = "S1701",
              variables = NULL,
              survey = "acs1",
              year = current_year)

poverty <- bind_rows(list(counties, nc, us))

# combine string of variable names to dataframe
vars <- load_variables(current_year, "acs1/subject")

poverty <- poverty %>%
  left_join(vars, by = c("variable" = "name")) %>%
  select(-variable, -concept, -GEOID)

# total poverty rates ----------------------------------------------

# total rates are in this string
total_string <- "Estimate!!Percent below poverty level!!Population for whom poverty status is determined"

total <- poverty %>%
  filter(label == !!total_string) %>%
  mutate(type = "Poverty Rate",
         subtype = "Total",
         # convert MOE to se
         moe = moe / 1.96) %>%
  select(geo_description = NAME, type, subtype, estimate, se = moe)

# poverty rates by race ----------------------------------------

ethnicity <- poverty %>%
  # we don't need counts, only need percentages
  filter(str_detect(label, "Percent")) %>%
  # extract and format race / ethnicity
  ff_acs_ethnicity(label) %>%
  # remove label column
  select(-label) %>%
  # convert moe to se
  mutate(moe = moe / 1.96) %>%
  # rename columns to standard names
  rename(geo_description = NAME, se = moe)

# poverty rates by age ----------------------------------------

# regular expression of age descriptions to pull out
# all start with Percent because we do not need raw numbers
age_re <- "Percent.*Under 5 years|Percent.*5 to 17 years|Percent.*18 to 34 years|Percent.*35 to 64 years|Percent.*65 years and over"

age <- poverty %>%
  filter(str_detect(label, !!age_re)) %>%
  # replace the long-winded string value for the age with just the age group
  mutate(label = str_replace(label, '.*Under 5 years', 'Under 5 years'),
         label = str_replace(label, '.*5 to 17 years', '5 to 17 years'),
         label = str_replace(label, '.*18 to 34 years', '18 to 34 years'),
         label = str_replace(label, '.*35 to 64 years', '35 to 64 years'),
         label = str_replace(label, '.*65 years and over', '65 years and over'),
         type = "Age",
         # convert moe to se
         moe = moe / 1.96) %>%
  rename(geo_description = NAME, subtype = label, se = moe)

# combine and clean all demographic datasets
all_poverty <- bind_rows(list(total, ethnicity, age)) %>%
  # convert percentages to decimal form for standardization
  mutate_at(vars(estimate, se), ~(./100)) %>%
  mutate(year = 2018) %>%
  # reorder columns, by making year the first columns, 
  # and keeping all other columns in the same order
  select(year, everything())

# import prior year data and combine with current year
prior_poverty <- read_csv("economy/poverty_rate/cleaned_data/poverty_rate_06_17.csv")

cleaned_poverty <- bind_rows(prior_poverty, all_poverty)

# write out final cleaned dataset
write_csv(cleaned_poverty, "economy/poverty_rate/cleaned_data/poverty_rate_cleaned.csv")

# get census tract data -------------------------------

poverty_tract <- get_acs(geography = "tract", 
                     variables = "S1701_C03_001", 
                     state = "NC",
                     county = "Forsyth",
                     survey = "acs5",
                     year = current_year)

poverty_tract <- poverty_tract %>%
  mutate(variable = "Poverty Rate",
         moe = round(moe / 1.96, 3)) %>%
  rename(geo_description = NAME, type = variable, se = moe)

write_csv(poverty_tract, "economy/poverty_rate/cleaned_data/poverty_update_tract.csv")
