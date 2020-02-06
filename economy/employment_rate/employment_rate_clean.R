library(tidyverse)
library(tidycensus)
library(FFtools)

current_year <- 2018
comparison_counties <- c("United States", "North Carolina", "Forsyth County, NC", "Guilford County, NC", "Durham County, NC")

# import data ---------------------------------

# import previous years data
non_counties <- c("United States", "North Carolina")

# import previous year data
emp_previous <- read_csv("economy/employment_rate/raw_data/employment_06_17.csv",
                         col_names=c("year", "geo_description", "type", "subtype", "estimate", "moe"),
                         skip=1) %>%
  # add "County, NC" to counties
  mutate(geo_description = ifelse(geo_description %in% non_counties, 
                                  geo_description, str_c(geo_description, " County, NC"))) %>%
  filter(type != "Age Group") %>%
  # convert MOE to SE and change name
  mutate(se = moe / 1.96) %>%
  select(-moe)

parameters <- list(geography = c('us', 'state', 'county'),
                   state = c(NA, rep('NC', 2)),
                   county = rep(NA, 3),
                   table = rep('S2301', 3),
                   variables = rep(NA, 3),
                   survey = rep('acs1', 3),
                   year = rep(current_year, 3),
                   use_table = rep(T, 3))

# import table
employment <- ff_iterate_acs(parameters)

# convert percentage to decimal and convert standard error to 

# clean data ------------------------------------

emp <- employment %>%
  # employment rate columns start with "Employment/Population Ratio!!Estimate" in the description column
  filter(str_detect(description, '^Employment/Population'))
# convert percentages that are whole numbers to decimals
  mutate_at(vars(estimate, se), ~(./100))

# demographics#

# total employment rate for comparison communities
comp <- emp %>%
  filter(str_detect(description, "Population 16 years and over"),
         geo_description %in% !! comparison_counties) %>%
  mutate(description = "Total Population",
         type = "Employment Rate") %>%
  select(year, geo_description, subtype = description, type, estimate, se)

# make separate dataset for forsyth since only forsyth needs demographic
forsyth <- emp %>%
  # filter county for comparison counties only
  filter(geo_description == "Forsyth County, NC")

# race ----------------------------------------
race <- forsyth %>%
  ff_acs_ethnicity(description) %>%
  select(year, geo_description, subtype, type, estimate, se)

# gender ---------------------------------------
gender <- forsyth %>%
  filter(str_detect(description, "Male$|Female$")) %>%
  mutate(description = str_replace_all(description, ".*Male$", "Male"),
         description = str_replace_all(description, ".*Female$", "Female"),
         type = "Gender") %>%
  select(year, geo_description, subtype = description, type, estimate, se)

# bind all datasets into one and write out ---------------------------
emp_current <- bind_rows(list(emp_previous, comp, race, gender)) %>%
  arrange(year, subtype, type, geo_description)

# write out updated cleand data
write_csv(emp_current, "economy/employment_rate/cleaned_data/employment_update.csv")

# get census tracts ----------------------------------------------
emp_tract <- get_acs(geography = "tract", 
                     variables = "S2301_C03_001", 
                     state = "NC",
                     county = "Forsyth",
                     survey = "acs5",
                     year = current_year)

emp_tract <- emp_tract %>%
  mutate(variable = "Employment Rate",
         moe = round(moe / 1.96, 3)) %>%
  rename(geo_description = NAME, type = variable, se = moe)

write_csv(emp_tract, "economy/employment_rate/cleaned_data/employment_update_tract.csv")
  
# rgdal::writeOGR(emp_tract, 
#                 "economy/employment_rate/cleaned_data/employment_update_tract",
#                 driver="ESRI Shapefile")

# for age, we need to create the ratio estimates from rebinned ages
# therefore, we need to recalculate from raw numbers Total!!Estimate

# age_bins <- list(`16 to 19 years` = "16 to 24",
#                  `20 to 24 years` = "16 to 24",
#                  `25 to 29 years` = "25 to 44",
#                  `30 to 34 years` = "25 to 44",
#                  `35 to 44 years` = "25 to 44",
#                  `45 to 54 years` = "45 to 64",
#                  `55 to 59 years` = "45 to 64",
#                  `60 to 64 years` = "45 to 64",
#                  `65 to 74 years` = "65 and over",
#                  `75 years and over` = "65 and over")
# 
# age <- employment %>%
#   # filter for the employment ratio or total employed
#   filter((str_detect(description, '^Employment/Population') | str_detect(description, '^Total!!Estimate')),
#          geo_description == "Forsyth County, NC") %>%
#   # recode ages
#   ff_acs_age(description, recode = T, recode_list = age_bins) %>%
#   # remove years from descriptio ncolumn, so we can pivo column
#   mutate(description = str_replace_all(description, "[0-9].*", "")) %>%
#          # need to create a unique identifier so the pivot works
#          #key = seq(1, nrow(.), 1)) %>%
#   # pivot out employment rate and total population, so we can calculate total working
#   pivot_wider(id_cols = key, names_from = description, values_from = c(estimate, se))