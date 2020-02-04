####################################################
#
# This script creates a table of ACS variable names
# and outputs the table as an RDS file
#
###################################################

library(tidyverse)
library(tidycensus)

# import acs 1 year variables for both profile and subject tables
acs1 <- map_df(c("profile", "subject"), 
                function(x) {
                  load_variables(year = 2017,
                  dataset = str_c("acs1/", x))
                  }
                ) %>%
  mutate(time = "acs1")

# import acs 5 year variables for both profile and subject tables
acs5 <- map_df(c("profile", "subject"), 
               function(x) {
                 load_variables(year = 2017,
                                dataset = str_c("acs5/", x))
               }
              ) %>%
  mutate(time = "acs5")

# combine the one and five year tables into one dataset
acs_vars <- bind_rows(acs1, acs5)

# output the dataset as an rds file
write_rds(acs_vars, "acs_table_vars/acs_vars.rds")

