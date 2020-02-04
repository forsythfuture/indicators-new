################################################################################
#
# This script creates the funal housing cost burden dataset.
#
# Description of dataset variables:
#   AGEP: age category; number represents top age in age bracket
#         for example: 44 represents people 25 to 44
#                      150 represents top age bracket: 65 and over
#   RAC1P: Racial category:
#           1 = White
#           2 = African American
#           3 = Hispanic
#           4 = other
#   housing_status: whether the person is an owner or renter
#   percentge: the percentage of houshold income that goes to rent or homeownership
#
######################################################################################

library(tidyverse)

source('economy/housing_burden/housing_burden_functions.R')

# import raw housing burden file
housing_burden <- read_csv('economy/housing_burden/clean_data/housing_burden_intermediate.csv.gz')

# rename columns so they are more descriptive
housing_burden <- housing_burden %>%
  rename(geography = GROUP,
         age = AGEP,
         race = RAC1P,
         tenure = housing_status,
         pct_housing = percentage_housing)

# recode races and ages
housing_burden <- housing_burden %>%
  mutate(race = ifelse(race=='1','White, non-Hispanic', 
                     ifelse(race=='2','African American',
                            ifelse(race=='3','Hispanic','Other'))))%>%
  mutate(age = ifelse(age=='24', '24 years and under',
                      ifelse(age=='44', '25 to 44 years',
                             ifelse(age=='64', '45 to 64', '65 years and over'))))
 
# create vector of weight column names, so we can iterate through them calculating burden
weight_cols <- str_extract(names(housing_burden), 'WG.*')
weight_cols <- weight_cols[!is.na(weight_cols)]

# calcualte housing burden for each replicate weight
burdens <- lapply(weight_cols,
                  function(x) calculate_burden(find_burden_house(housing_burden, x)))

# calculate standard errors
se <- find_se(burdens)

# pull out housing burdens of primary weights and
# add standard errors to dataframe of housing burdens of primary weight
burden_primary <- burdens[[1]] %>%
  ungroup() %>%
  mutate(se = !!se[[1]],
         # add ' County, NC' to county names
         geography = ifelse(geography %in% c('Forsyth', 'Guilford', 'Durham'),
                            paste0(geography, ' County, NC'), geography)) %>%
  rename(geo_description = geography)

write_csv(burden_primary, 'economy/housing_burden/clean_data/housing_burden_final.csv')
