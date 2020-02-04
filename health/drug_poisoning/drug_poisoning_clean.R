#########################################################################
#
# This script cleans and creates the final dataset for drug poisonings
# per 100,000 persons
#
#########################################################################

library(tidyverse)

# comparison counties
comp_counties <- c("Forsyth|Guilford|Durham")

# list all files
file_paths <- list.files("health/drug_poisoning/raw_data", full.names = T)

# function that imports data, which can be used for all demographics
import_drug_pois <- function(file_paths, re_file_paths, comp_counties) {
  
  # parameter:
  #  file_paths: paths to state and county files
  #  re_file_paths: regular expression to extract needed demographic file paths
  
  # extract file paths
  needed_paths_true <- str_detect(file_paths, re_file_paths)
  needed_paths <- file_paths[needed_paths_true]
  
  map_df(needed_paths, function(x) { 
    read_tsv(x, col_types = cols(.default = col_character())) %>%
      # the geographic area is either 'State' or 'County', make it common to both datasets
      rename(geo_description = 2) %>%
      # remove notes column, columns that are jsut codes, and crude rate columns
      select(-Notes, -ends_with('Code'), -starts_with('Crude'))
  }) %>%
    mutate_all(~str_remove(., "Suppressed|Not Applicable|Unreliable")) %>%
    # only keep comparison counties and north carolina
    filter((str_detect(geo_description, comp_counties)) | geo_description == "North Carolina")
  
}

# clean total demographic ----------------------------------------------

# import data
total_drug <- import_drug_pois(file_paths, 'total', comp_counties)

# rename columns by converting to one-word column names
colnames(total_drug) <- c('geo_description', 'year', 'deaths', 'pop', 'estimate', 'se')

total_drug <- total_drug %>%
  # drop year NA columns, as these signify totals which are not needed
  drop_na(year)

# add descriptive type and subtype, and remove population count items and crude rates, which are not needed
total_drug <- total_drug %>%
  mutate(type = "Drug Poisoning",
         subtype = "Total Population") %>%
  select(-deaths, -pop) %>%
  select(geo_description, year, type, subtype, estimate, se)

# clean race -------------------------------------

# import data and only keep Forsyth
race_drug <- import_drug_pois(file_paths, 'race', comp_counties)

colnames(race_drug) <- c('geo_description', 'hisp', 'race', 'deaths', 'pop', 'estimate', 'se')

# isolate just hispanic / latino
hisp_drug <- race_drug %>%
  filter(hisp == "Hispanic or Latino",
         is.na(race)) %>%
  mutate(race = "Hispanic / Latino") %>%
  select(-hisp)

# hispanic data does not show up in the data, so it cannot be used

# isolate white, african-american
white_aa_drug <- race_drug %>%
  filter(hisp == "Not Hispanic or Latino",
         str_detect(race, "^White|^Black")) %>%
  select(-hisp) %>%
  # change working of race descriptions
  mutate(race = str_replace(race, "^White", "White, non-Hispanic"),
         race = str_replace(race, "^Black.*", "African American")) %>%
  mutate(type = "Race / Ethnicity",
         year = "2013 - 2017") %>%
  select(geo_description, year, type, subtype = race, estimate, se)

# clean gender ----------------------------------------

# import data
gender_drug <- import_drug_pois(file_paths, 'gender', comp_counties)

colnames(gender_drug) <- c('geo_description', 'gender', 'deaths', 'pop', 'estimate', 'se')

gender_drug <- gender_drug %>%
  drop_na(gender) %>%
  mutate(type = "Gender",
         year = "2013 - 2017") %>%
  select(geo_description, year, type, subtype = gender, estimate, se)

# combine all datasets
total_drug_pois <- bind_rows(list(total_drug, white_aa_drug, gender_drug)) %>%
  mutate_at(vars(estimate, se), ~as.numeric(.))
  
write_csv(total_drug_pois, "health/drug_poisoning/cleaned_data/drug_pois_cleaned.csv")
