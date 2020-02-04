# the current webpage with data is at: 
# https://www.forsythfutures.org/indicator_post-secondary-completion/

library(tidyverse)
library(tidycensus)
library(FFtools) # install with devtools::install_github('forsythfuture/FFtool')

current_year <- 2018

year_seq <- seq(2012, 2018, 1)

# pull in most recent year's data from census api -------------

# create function to pull in US, NC, and counties for given year
get_educ_year <- function(current_year, table_num) {
  
  counties <- c("Forsyth", "Guilford", "Durham")
  
  # pull in county data
  counties <- get_acs(geography = "county",
                      state = "NC",
                      county = counties,
                      table = table_num,
                      variables = NULL,
                      survey = "acs1",
                      year = current_year) %>%
    # change geography string from full state to state abbreviation
    mutate(NAME = str_replace(NAME, "North Carolina", "NC"))
  
  # pull in NC state data
  nc <- get_acs(geography = "state",
                state = "NC",
                table = table_num,
                variables = NULL,
                survey = "acs1",
                year = current_year)
  
  # pull in US data
  us <- get_acs(geography = "US",
                table = table_num,
                variables = NULL,
                survey = "acs1",
                year = current_year)
  
  education <- bind_rows(list(counties, nc, us)) %>%
    mutate(year = !!current_year)
  
  return(education)

}

education <- map_df(year_seq, get_educ_year, table_num = "B15002")

# combine string of variable names to dataframe
vars <- load_variables(current_year, "acs1")

education <- education %>%
  left_join(vars, by = c("variable" = "name")) %>%
  select(-variable, -concept, -GEOID)

# start cleaning data ------------------------------------------------

# get total -----------------------------------------------------

education_total <- education %>%
  mutate(label = str_replace_all(label, "Male|Female", "Sex")) %>%
  group_by(NAME, year, label) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# save total popualtion as object for reference when creating percentage
total_pop <- education_total %>%
  group_by(NAME, year) %>%
  filter(label == "Estimate!!Total") %>%
  select(NAME, year, pop_estimate = estimate, pop_moe = moe)

# only keep people with an associates or higher (associates, bachelors, masters, Phd)
education_total <- education_total %>%
  filter(str_detect(label, "Associate|Bachelor|Master|Profes|Doctorate")) %>%
  # clean up label
  mutate(label = str_remove(label, "Estimate!!Total!!Sex!!")) %>%
  # sum education levels to create single category signifying associates or higher
  group_by(NAME, year) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# add population totals to education totals
education_total <- education_total %>%
  ungroup() %>%
  left_join(total_pop, by = c("NAME", "year")) %>%
  # calcualte proportion with associates or higher
  mutate(prop = estimate / pop_estimate,
         prop_moe = moe_prop(estimate, pop_estimate, moe, pop_moe),
         se = prop_moe / 1.645) %>%
  # add type and subtype
  mutate(type = "Educational Attainment",
         subtype = "Total Population") %>%
  select(geo_description = NAME, year, type, subtype, estimate = prop, se)

# by race / ethnicity -----------------------------------------

# create function to get race
get_race <- function(current_year, table_num, race_name, vars) {
  
  get_acs(geography = "county",
          state = "NC",
          county = "Forsyth",
          table = table_num,
          variables = NULL,
          survey = "acs1",
          year = current_year) %>%
    # change geography string from full state to state abbreviation
    mutate(NAME = str_replace(NAME, "North Carolina", "NC"),
           subtype = !!race_name,
           year = !!current_year) %>%
    left_join(vars, by = c("variable" = "name")) %>%
    select(-variable, -concept, -GEOID)
  
}

# import race educational attainment tables for all years and races
white <- map_df(year_seq, get_race, table_num = "B15002H", race_name = "White, non-Hispanic", vars = vars)
aa <- map_df(year_seq, get_race, table_num = "B15002B", race_name = "African American", vars = vars)
hl <- map_df(year_seq, get_race, table_num = "B15002I", race_name = "Hispanic / Latino", vars = vars)

# bind all years and races into one dataset
race <- bind_rows(list(white, aa, hl))

# combine both sexes
race_total <- race %>%
  mutate(label = str_replace_all(label, "Male|Female", "Sex")) %>%
  group_by(NAME, year, subtype, label) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# save total popualtion as object for reference when creating percentage
race_total_pop <- race_total %>%
  filter(label == "Estimate!!Total") %>%
  select(NAME, year, subtype, pop_estimate = estimate, pop_moe = moe)

# only keep people with an associates or higher (associates, bachelors, masters, Phd)
race_total <- race_total %>%
  # clean up label
  mutate(label = str_remove(label, "Estimate!!Total!!Sex!!")) %>%
  filter(str_detect(label, "Associate|Bachelor|Grad")) %>%
  # sum education levels to create single category signifying associates or higher
  group_by(NAME, year, subtype) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# add population totals to education totals
race_total <- race_total %>%
  ungroup() %>%
  left_join(race_total_pop, by = c("NAME", "year", "subtype")) %>%
  # calcualte proportion with associates or higher
  mutate(prop = estimate / pop_estimate,
         prop_moe = moe_prop(estimate, pop_estimate, moe, pop_moe),
         se = prop_moe / 1.645) %>%
  # add type and subtype
  mutate(type = "Race / Ethnicity") %>%
  select(geo_description = NAME, year, type, subtype, estimate = prop, se)

# Gender ------------------------------------------------------------------

gender_total <- education %>%
  filter(NAME == "Forsyth County, NC")

# save total popualtion as object for reference when creating percentage
gender_total_pop <- gender_total %>%
  filter((label == "Estimate!!Total!!Male") |  (label == "Estimate!!Total!!Female")) %>%
  mutate(label = str_remove(label, "Estimate!!Total!!")) %>%
  rename(pop_estimate = estimate, pop_moe = moe, subtype = label)

# only keep people with an associates or higher (associates, bachelors, masters, Phd)
gender_total <- gender_total %>%
  # clean up label
  mutate(label = str_remove(label, "Estimate!!Total!!")) %>%
  filter(str_detect(label, "Associate|Bachelor|Master|Profes|Doctorate")) %>%
  mutate(subtype = str_extract(label, "Female|Male"),
         label = str_remove(label, "Female!!|Male!!")) %>%
  # sum education levels to create single category signifying associates or higher
  group_by(NAME, year, subtype) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# add population totals to education totals
gender_total <- gender_total %>%
  ungroup() %>%
  left_join(gender_total_pop, by = c("NAME", "year", "subtype")) %>%
  # calcualte proportion with associates or higher
  mutate(prop = estimate / pop_estimate,
         prop_moe = moe_prop(estimate, pop_estimate, moe, pop_moe),
         se = prop_moe / 1.645) %>%
  # add type and subtype
  mutate(type = "Gender") %>%
  select(geo_description = NAME, year, type, subtype, estimate = prop, se)

# age ---------------------------------------------------------------

# race function can also work for pulling in all age data
age <- map_df(year_seq, get_race, table_num = "B15001", race_name = "Age", vars = vars)

age <- age %>%
  rename(type = subtype)

# age groups to keep as regular expression
age_re <- "25 to 34 years|35 to 44 years|45 to 64 years|65 years and over"

# combine both sexes
age_total <- age %>%
  mutate(label = str_replace_all(label, "Male|Female", "Sex")) %>%
  group_by(NAME, year, label) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  # only keep needed age groups
  filter(str_detect(label, !!age_re)) %>%
  # extract age in its own column
  mutate(subtype = str_extract(label, !!age_re))

# save total popualtion as object for reference when creating percentage
age_total_pop <- age_total %>%
  filter(str_detect(label, "years$|years and over$")) %>%
  select(NAME, year, subtype, pop_estimate = estimate, pop_moe = moe)

# only keep people with an associates or higher (associates, bachelors, masters, Phd)
age_total <- age_total %>%
  # clean up label
  mutate(label = str_remove(label, "^E.*years!!|^E.*over!!")) %>%
  filter(str_detect(label, "Associate|Bachelor|Grad")) %>%
  # sum education levels to create single category signifying associates or higher
  group_by(NAME, year, subtype) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# add population totals to education totals
age_total <- age_total %>%
  ungroup() %>%
  left_join(age_total_pop, by = c("NAME", "year", "subtype")) %>%
  # calcualte proportion with associates or higher
  mutate(prop = estimate / pop_estimate,
         prop_moe = moe_prop(estimate, pop_estimate, moe, pop_moe),
         se = prop_moe / 1.645) %>%
  # add type and subtype
  mutate(type = "Age") %>%
  select(geo_description = NAME, year, type, subtype, estimate = prop, se)

# combine all datasets and write out -------------------------------------------

educ_attain <- bind_rows(list(education_total, race_total, gender_total, age_total))

write_csv(educ_attain, "education/educational_attainment/cleaned_data/educational_attainment.csv")