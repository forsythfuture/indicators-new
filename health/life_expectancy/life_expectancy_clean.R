#####################################################################
#
# This script cleans life expectancy
#
  # The data comes from https://schs.dph.ncdhhs.gov/data/lifexpectancy/
# From the drop down arros at the bottom, the 
# "2018 State of North Carolina and 2016-2018 County Life Expectancy at Birth"
# option was selected, then the output from the next page was coy and pasted
# into a csv file and saved into the raw_data folder
#
# Note: The dataset contains White, but it is not clear from the source
# whether this is white in total, or just white, non-hispanic
#
#######################################################################

library(tidyverse)

# comparison counties, written as a regular expressions for easier filtering
comparison_counties <- c('Forsyth|Durham|Guilford')

# get all the raw data files
file_paths <- list.files('health/life_expectancy/raw_data', full.names = T)

# raw data does not have c  olumn names, so create column names for import
col_names <-c('geo_description', 'total_estimate', 'total_ci', 'male_estimate', 'male_ci', 
              'female_estimate', 'female_ci', 'white_estimate', 'white_ci', 'aa_estimate', 'aa_ci')

# import all datasets
life_expec <- map_df(file_paths, function(x) {
  read_csv(x, col_names = col_names) %>%
    mutate(year = parse_number(x))
})

# NA values are represented as 'N/A' replace with blank character
life_expec <- life_expec %>%
  mutate_all(~str_replace(., "N/A", ""))

# create two dataframes, one for estimates and one for CI, which 
# then both be converted to long form and later be combines
life_expec_est <- life_expec %>%
  select(geo_description, year, ends_with('estimate'))  %>%
  pivot_longer(cols = ends_with('estimate'), 
               values_to = 'estimate') %>%
  mutate(name = str_remove(name, "_estimate"))

life_expec_ci <- life_expec %>%
  select(geo_description, year, ends_with('ci')) %>%
  pivot_longer(cols = ends_with('ci'), 
               values_to = 'ci') %>%
  mutate(name = str_remove(name, "_ci")) %>%
  separate(ci, into = c('lower_ci', 'upper_ci'), sep = '-')

# bind estimate and ci datasets
life_expec_full <- left_join(life_expec_est, life_expec_ci,
                              by = c('geo_description', 'year', 'name'))

# convert to numeric and convert CI t ostandard error
life_expec_full <- life_expec_full %>%
  mutate_at(vars(year, estimate:upper_ci), ~as.numeric(.)) %>%
  mutate(lower_estimate = estimate - lower_ci,
         upper_estmate = upper_ci - estimate,
         # make the moe the avergage between the two
         moe = (lower_estimate + upper_estmate) / 2,
         se = moe / 1.96) %>%
  # convert whole numbers to decimal percentages
  mutate_at(vars(estimate, se), ~round(.*.01, 4)) %>%
  select(-lower_ci:-moe)

# relabel demographic descriptions and add type
recode_demo <- c(`total` = "Life Expectancy",  
                 `male` = "Male", `female` = "Female",
                 `white` = "White", `aa` = "African-American")

life_expec_full <- life_expec_full %>%
  # relabel demographics
  mutate(subtype = recode(name, !!!recode_demo),
         # relable type
         type = ifelse(name == "total", "Total Population",
                       ifelse(str_detect(name, "male"), "Gender",
                              ifelse(str_detect(name, "whi|aa"), "Race / Ethnicity", "")))) %>%
  select(-name) %>%
  # relabel geography
  mutate(geo_description = str_remove(geo_description, "State of "),
         geo_description = str_replace(geo_description, "County", "County, NC")) %>%
  # only keep comparison counties and NC
  filter((geo_description == "North Carolina") | (str_detect(geo_description, comparison_counties))) %>%
  select(geo_description, year, type, subtype, everything())

write_csv(life_expec_full, "health/life_expectancy/clean_data/life_expenctany_clean.csv")
