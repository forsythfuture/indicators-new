library(tidyverse)

dir <- "health/low_birthweight/"

weight <- read_csv(str_c(dir, "raw_data/low_birthweight_raw.csv")) %>%
  # filter out wake and mecklenburg, which were part of original dataset
  filter(!(str_detect(geo_comparison, "Wake|Meck"))) %>%
  # calculate estimated low birthweight
  mutate(estimate = success / trials) %>%
  rename(geo_description = geo_comparison)

# for the counties, add ", NC" to the end of them
weight <- weight %>%
  mutate(geo_description = ifelse(str_detect(geo_description, "County"),
                                  str_c(geo_description, ", NC"), geo_description))

write_csv(weight, str_c(dir, "cleaned_data/low_birthweight.csv"))

# don't run because we'll compute standard error in indicators app
# calculate standard errors
# weight <- weight %>%
#   mutate(norm = sqrt((estimate*(1-estimate))/nrow(.)),
#          poisson = sqrt((estimate*100)/trials))
  

