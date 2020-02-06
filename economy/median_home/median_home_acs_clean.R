library(tidyverse)
library(tidycensus)
library(FFtools)

current_year <- 2018

# vector of years we want median income for, data starts in 2012
years <- seq(2012, current_year, 1)

# comparison counties
counties <- c("Forsyth", "Guilford", "Durham")

# get US median home value for 2012-2018
us <- map_df(years, function(x) {
  get_acs(geography = "US",
          variables = ifelse(x < 2015, "DP04_0088E", "DP04_0089E"),
          year = x,
          survey="acs1",
          moe_level = 95) %>%
    mutate(year = !!x)
  }
)

# get NC median home values
nc <- map_df(years, function(x) {
  get_acs(geography = "state",
          state = "NC",
          variables = ifelse(x < 2015, "DP04_0088E", "DP04_0089E"),
          year = x,
          survey="acs1",
          moe_level = 95) %>%
    mutate(year = !!x)
  }
)

# get county median home values
county <- map_df(years, function(x) {
  get_acs(geography = "county",
          state = "NC",
          county = counties,
          variables = ifelse(x < 2015, "DP04_0088E", "DP04_0089E"),
          year = x,
          survey="acs1",
          moe_level = 95) %>%
    mutate(year = !!x)
  }
)

# combine all geographies into one dataset
home <- bind_rows(list(us, nc, county))

home <- home %>%
  # convert "North Carolina" to "NC" in county column
  mutate(NAME = str_replace(NAME, ", North Carolina", ", NC"),
         # convert 95% MOE to standard error
         moe = moe / 1.96,
         # add type and subtype columns, so dataset works in shiny app
         type = "Comparison Community",
         subtype = "Total") %>%
  select(year, geo_description = NAME, type, subtype, estimate, se = moe)


# adjust for inflation to 2018 dollars
home <- ff_inflation_adjust(home, wages_col = 'estimate', se_col = 'se', year_col = 'year',
                            year_adjust = 2018, errors = TRUE)

write_csv(home, "economy/median_home/cleaned_data/median_home_cleaned.csv")

# get by census tract --------------------------------

median_home_census_tract <- get_acs(geography = "tract", 
                                    variables = "DP04_0089E", 
                                    state = "NC",
                                    county = "Forsyth",
                                    survey = "acs5",
                                    year = current_year)

median_home_census_tract <- median_home_census_tract %>%
  mutate(variable = "Median Home Value",
         moe = round(moe / 1.96, 3)) %>%
  rename(geo_description = NAME, type = variable, se = moe)

write_csv(median_home_census_tract, "economy/median_home/cleaned_data/median_home_tract.csv")

