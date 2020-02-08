create_pums_data <- function(housing_file, population_file, puma_to_county_file, county_comparison) {
  
  # This functions creates the combined housing and population dataset to analyze
  # median housing by demographics
  
  # parameters
  #  housing_file: file path or url to housing dataset
  #  puma_to_county_file: file path to dataset containing mapping from PUMA codes to counties
  #  county_comparison: counties to use for comparisons
  #  population_file: file path or url to population dataset
  
  # create housing and population datasets
  house <- create_pums_housing(housing_file, puma_to_county_file, county_comparison)
  pop <- create_pums_pop(population_file)
  
  # bind population with household dataset
  joined <- house %>%
    left_join(pop, by = c('SERIALNO', 'PUMA')) %>%
    # reorder variables to ensure common order for renaming
    select(cntyname, AGEP, RAC1P, VALP, matches("wgtp|WGTP")) %>%
    # rename all column names to lower case, so they are standardized for all years
    rename_all(~tolower(.))
  
  return(joined)
  
}

create_pums_housing <- function(housing_file, puma_to_county_file, county_comparison) {
  
  # This functions creates the cleaned housing file for calculating median income by demographic
  
  # parameters
  #  housing_file: file path or url to housing dataset
  #  puma_to_county_file: file path to dataset containing mapping from PUMA codes to counties
  #  county_comparison: counties to use for comparisons
  
  # read in and filter data
  house <- read_csv(housing_file,
                    col_types = cols(SERIALNO = col_character(),
                                     RT = col_character(),
                                     .default = col_double())) %>%
    select(SERIALNO, RT, ST, PUMA, TYPE, VALP, TEN, matches('^wgtp.*|^WGTP.*')) %>%
    filter(
      ST == 37, # only keep north carolina
      str_detect(RT, "^H|^h"), # only keep housing quarters
      !is.na(VALP), # remove all rows with missing values for home value
      TEN %in% c(1, 2), # only keep home owners
      TYPE == 1 # only keep housing units
    )
  
  # import PUMA codes / county mapping and combine with housing data, so we will
  # know which counties each row belong to
  puma_codes <- puma_area_code(county_comparison, puma_to_county_file, T) %>%
    distinct()
  
  house <- house %>%
    left_join(puma_codes, by = 'PUMA') 
  
  return(house)
  
}

create_pums_pop <- function(population_file) {
  
  # This functions creates the population dataset to analyze
  # median housing by demographics
  
  # parameters
  #  population_file: file path or url to population dataset
  
  pop <- read_csv(population_file,
                  col_types = cols(RT= col_character(),
                                   NAICSP = col_character(),
                                   SOCP = col_character(),
                                   SERIALNO = col_character(),
                                   HISP = col_character(),
                                   .default = col_double())) %>%
    select(SERIALNO, matches('^REL$|^RELP$'), ST, AGEP, PUMA, RAC1P, HISP) %>%
    filter(ST == 37)
  
  # only keep head of household
  pop <- pop[pop[[2]] == 0,]
  
  # remove household status column and state
  pop <- pop[c('SERIALNO', 'AGEP', 'PUMA', 'RAC1P', 'HISP')]
  
  # convert race / ethnicity to one column
  pop <- ff_pums_ethnicity(pop) %>%
    # hispanic column is no longer needed
    select(-HISP)
  
  # rebin ages; ages under 25 will be NA (missing)
  age_categories <- c('25 to 44',
                      '45 to 64',
                      'Over 65 years of age')
  
  pop$AGEP <- cut(pop$AGEP, breaks = c(25, 44, 64, 120), labels = age_categories, include.lowest = T)
  
  pop$AGEP <- as.character(pop$AGEP)
  
  return(pop)
}
calc_med_housing <- function(housing_survey, demo_col, county, type_text) {
  
  # function that calculates medina oncome of demogrpahics for both state and counties
  
  # parameters:
  #  housing_survey: survey object of the housing data
  #  demo_col: demographic column in housing_survey dataset
  #  county: boolean, whether to calculate for counties or whole state
  #  type_text: text to display in the type column
  
  # set grouping variables, based on whether grouping on county or not
  if (county == T) {
    grouping_var <- c(demo_col, 'cntyname')
  } else if (county == F) {
    grouping_var <- demo_col
  } else {
    stop('county must be TRUE or FALSE')
  }
  
  df <- housing_survey %>%
    group_by_at(grouping_var) %>%
    summarize(median_housing = survey_median(valp, na.rm = T, vartype = 'se')) %>%
    mutate(type = !!type_text) %>%
    rename(estimate = median_housing, se = median_housing_se, subtype = !!demo_col)
  
  if (county == T) {
    df <- rename(df, geo_description = cntyname)
  } else {
    df$geo_description <- 'North Carolina'
  }
  
  return(df)
  
}
