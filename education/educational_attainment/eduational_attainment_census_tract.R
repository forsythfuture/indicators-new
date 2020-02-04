library(tidyverse)
library(tidycensus)
library(FFtools) # install with devtools::install_github('forsythfuture/FFtool')

current_year <- 2018

tracts <- get_acs(geography = "tract",
                  state = "NC",
                  county = "Forsyth",
                  table = "B15002",
                  survey = "acs5",
                  year = current_year)

var <- load_variables(current_year, "acs5")

tracts <- tracts %>%
  left_join(var, by = c("variable" = "name")) %>%
  select(-variable, -concept, -GEOID) 

# get total population in each census tract
tracts_total_pop <- tracts %>%
  filter(label == "Estimate!!Total") %>%
  select(NAME, pop_estimate = estimate, pop_moe = moe)

# combine sexes
tracts_total <- tracts %>%
  mutate(label = str_replace_all(label, "Male|Female", "Sex")) %>%
  group_by(NAME, label) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2))) %>%
  filter(str_detect(label, "Associate|Bachelor|Master|Profes|Doctorate")) %>%
  # sum education levels to create single category signifying associates or higher
  group_by(NAME) %>%
  summarize(estimate = sum(estimate),
            moe = sqrt(sum(moe^2)))

# add total census tract populations to educational attainment populations
tracts_total <- tracts_total %>%
  ungroup() %>%
  left_join(tracts_total_pop, by = "NAME") %>%
  # calcualte proportion with associates or higher
  mutate(prop = estimate / pop_estimate,
         prop_moe = moe_prop(estimate, pop_estimate, moe, pop_moe),
         se = prop_moe / 1.645) %>%
  # add type and subtype
  mutate(type = "Educational Attainment",
         subtype = "Census Tract",
         year = !!current_year) %>%
  select(geo_description = NAME, year, type, subtype, estimate = prop, se)

# write out
write_csv(tracts_total, "education/educational_attainment/cleaned_data/educational_attainment_census_tract.csv")
