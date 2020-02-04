library(tidyverse)
library(tidycensus)
library(FFtools)

geo_areas <- c('United States', 'North Carolina', 
               'Forsyth County, NC', 'Guilford County, NC', 'Durham County, NC')

df <- read_csv('economy/poverty_rate/raw_data/poverty_all_years_counties.csv') %>%
  # only keep selected NC counties, the state of NC, and US data
  filter(geo_description %in% geo_areas,
         # only need rows showing the percent below poverty level, don't need count data
         str_detect(description, '^Percent'))

total_data <- df %>%
  # only keep total poverty rate
  filter(str_detect(description, '^Percent *below poverty level; Estimate; Population for whom poverty status is determined$')) %>%
  mutate(type = "Poverty Rate",
         subtype = "Total Population") %>%
  # divide estiates and se by 100, so percentages are decimals
  mutate_at(vars(estimate, se), ~(./100)) %>%
  select(geo_description, year, type, subtype, estimate, se)

# filter for race
ethnicity <- df %>%
  ff_acs_ethnicity(description) %>%
  select(geo_description, year, subtype, estimate, se) %>%
  # add type of column
  mutate(type = 'Race and Ethnicity') %>%
  # divide estiates and se by 100, so percentages are decimals
  mutate_at(vars(estimate, se), ~(./100))

# poverty by age, uses different table --------------------------------

# create list of age descriptions that will be created
age_descriptions <- c('Under 5 years',
                      '5 to 17 years',
                      '18 to 34 years',
                      '35 to 64 years',
                      '65 years and over')

age <- read_csv('economy/poverty_rate/raw_data/poverty_age_groups.csv') %>%
  # only keep selected NC counties, the state of NC, and US data
  filter(geo_description %in% geo_areas) %>%
  # extract age group and place age group name in its own columns
  mutate(age = str_extract(description, '- [0-9].*|- Under.*'),
         # remove '- ' from age group
         age = str_replace(age, '- ', ''),
         # exract whether row signifies above or below poverty and place in its own column
         level = str_extract(description, 'below poverty|above poverty')) %>%
  # filter out NA values for age group
  filter(!is.na(age)) %>%
  # create new age bins
  mutate(age = str_replace_all(age, '^5 y.*|^6 t.*|^12.*|^15.*|^16.*', age_descriptions[2])) %>%
  mutate(age = str_replace_all(age, '^18.*|^25.*', age_descriptions[3])) %>%
  mutate(age = str_replace_all(age, '^35.*|^45.*|^55.*', age_descriptions[4])) %>%
  mutate(age = str_replace_all(age, '^65.*|^75.*', age_descriptions[5])) %>%    
  # we don't need information about sex, so group by year, above/below poverty, and age group and sum
  group_by(geo_description, year, level, age) %>%
  # calculate aggregate counts and aggregate count MOE for aggregate of males and females
  summarize(estimate = sum(estimate),
            moe = sqrt( sum(moe^2) ))

# create separate dataframe for the total number of people in each age group
# calculate by adding number of people below poverty line to those above poverty line
age_total <- age %>%
  # group by year and age group
  group_by(geo_description, year, age) %>%
  # calculate aggregate counts and aggregate count MOE for each age group
  summarize(estimate = sum(estimate),
            moe = sqrt( sum(moe^2) ))
# filter out those above poverty because we only need percentage below poverty
age <- age %>%
  filter(level == 'below poverty') %>%
  # add age group totals to data frame; needed for creating percentages
  left_join(age_total, by = c('geo_description', 'year', 'age'), suffix = c('_below', '_total'))

# calculate percentages, along with MOE, se, and cv

# calcualte percent in poverty
age$estimate <- age$estimate_below / age$estimate_total

# calcuate 95% MOE and convert to SE
# it is a 95% MOE because the raw numbers are 95% MOE
age$se <- moe_prop(age$estimate_below,age$estimate_total,  
                   age$moe_below, age$moe_total)
age$se <- age$se / 1.96

# only keep needed variables
age <- age %>%
  ungroup() %>%
  mutate(type = "Age") %>%
  select(year, geo_description, type, subtype = age, estimate, se)

# bind all old years poverty rate demographics
poverty_old <- bind_rows(list(total_data, ethnicity, age))

# write out
write_csv(poverty_old, "economy/poverty_rate/cleaned_data/poverty_rate_06_17.csv")
