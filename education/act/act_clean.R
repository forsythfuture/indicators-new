library(tidyverse)
library(glue)

# filters for needed schools and relabels school codes to string of county name
get_schools <- function(df) {
  
  # vector of school codes to keep
  code <- c("340", # Forsyth County
            "410", # Guilford County,
            "320", # Durham County,
            "NC" # state of NC
  )
  
  # recode school code to school name
  schoolcode_recode <- c(`340` = "Forsyth County, NC",
                         `410` = "Guilford County, NC",
                         `320` = "Durham County, NC",
                         `NC` = "North Carolina")
  
  df <- df %>%
    filter(school_code %in% !!code) %>%
    mutate(school_code = recode(school_code, !!!schoolcode_recode))
  
  return(df)
  
}

# data from 2014 to 2017 is formatted separately than data from 2018-2019
# therefore, they are cleaned differently

# cleand 2014 - 2017 data --------------------------------------

# calculating non-eds percentage -----
calculate_non_eds <- function(df) {
  # total number that passed
  total_passed <- df$all_count * (df$all / 100)
  
  # total eds that passed
  total_eds_pass <- df$eds_count * (df$eds / 100)
  
  # total non-eds that passed
  total_non_eds_pass <- total_passed - total_eds_pass
  
  # total non-eds
  total_non_eds <- df$all_count - df$eds_count
  
  # percentage of non-eds that passed
  perc_non_eds <- total_non_eds_pass / total_non_eds
  
  df$non_eds <- round(perc_non_eds * 100,1)
  df$non_eds_count <- total_non_eds
  
  return(df)
  
}
url <- 'https://nc-school-data.s3.amazonaws.com/act/data-report2014.csv.gz'
# this function cleans school data from 2017 and prior
clean_old_schools <- function(url) {

  # columns to keep
  cols <- c("school_code", "all", "female", "male", "black", "hispanic", "white",
            "eds", "all_count", "female_count", "male_count", "black_count", "hispanic_count", 
            "white_count", "eds_count")
  
  school <- read_csv(url,
                     # read in all columsn as character because the
                     # greater than and less than signs on numeric columns
                     # cause parsing failures
                     col_types = cols(.default = "c")) %>%
    filter(subject == "The ACT - Composite Score") %>%
    select(!!cols)
  
  # filter for needed schools and recode
  school <- get_schools(school)
  
  # values less than 5% and greater than 95% have < or > signs
  # remvoe these signs and convert values to numeric
  school <- school %>%
    mutate_all(~(str_replace_all(., "<|>", ""))) %>%
    mutate_at(vars(all:eds_count), as.numeric)
  
  school <- calculate_non_eds(school)
  
  # pivot to long form
  school <- pivot_longer(school,
                          cols = c(all:non_eds_count),
                          names_to = "subtype",
                          values_to = "estimate",
                          )
  
  
  # separate counts and estimates into different datasets,
  # so we can recombine them into different columns
  estimates <- school %>%
    filter(!(str_detect(subtype, "count")))
  
  counts <- school %>%
    filter(str_detect(subtype, "count")) %>%
    # replace 'count' word, so we can merge back with estiamte
    # dataframe based on demographic
    mutate(subtype = str_replace_all(subtype, "_count", "")) %>%
    rename(trials = estimate)
  
  schools_merge <- left_join(estimates, counts,
                             by = c("school_code", "subtype"))
  
  schools_merge$success <- round(schools_merge$trials * (schools_merge$estimate/100), 0)
  
  # recode type from subtype
  schools_merge$type <- ifelse(schools_merge$subtype == "all", "Comparison Community",
                               ifelse(schools_merge$subtype %in% c("black","hispanic","white"), "Race / Ethnicity",
                                      ifelse(schools_merge$subtype %in% c("male", "female"), "Gender",
                                              ifelse(schools_merge$subtype %in% c("eds", "non_eds"), "Economic Status", "No match"))))
  
  subtype_recode <- c(all = "Total",
                      female = "Female",
                      male = "Male",
                      black = "African American",
                      hispanic = "Hispanic / Latino",
                      white = "White, non-hispanic",
                      eds = "Economically Disadvantaged",
                      non_eds = "Non-economically Disadvantaged")
  
  schools_merge <- schools_merge %>%
    # recode subtype
    mutate(subtype = recode(subtype, !!!subtype_recode),
           # convert the estimate from whole number percent to decimal
           estimate = estimate / 100) %>%
    rename(geo_description = school_code)
  
  # extract year from url and add it to dataframe
  year <- str_extract(url, "[0-9]{4}")
  schools_merge$year <- year
    
  return(schools_merge)

}

# clean 2018 - 2019 data ---------------------------------

# function to clean 2018 and 2019 act scores
clean_new_schools <- function(url) {  

  new_cols <- c("reporting_year", "school_code", "school_name", "subject", "subgroup", "den", "total_pct")
  
  df <- read_csv(url,
                # read in all columsn as character because the
                # greater than and less than signs on numeric columns
                # cause parsing failures
                col_types = cols(.default = "c")) %>%
    select(!!new_cols)
  
  # get the needed school districts
  df <- get_schools(df)
  
  # only keep aggregate district totals (remove individual schools)
  df <- df %>%
    filter(str_detect(school_name, "Public Schools|County Schools|State of"),
           # only keep ACT
           subject == "ACT") %>%
    mutate_at(vars(den, total_pct), ~as.numeric(str_replace_all(., "<|>", "")))
  
  # calcualte non-eds by pulling out EDS and total
  eds <- df %>%
    filter(subgroup %in% c("ALL", "EDS")) %>%
    pivot_wider(names_from = "subgroup",
                values_from = c("den", "total_pct")) %>%
    # rename to match names needed for function that calcualtes non-eds rate
    rename(all_count = den_ALL, eds_count = den_EDS, 
           all = total_pct_ALL, eds = total_pct_EDS)
  
  # calcualte non-eds
  eds <- calculate_non_eds(eds) %>%
    # make columns match regular dataframe, so they can be combined
    select(reporting_year:subject, total_pct = non_eds, den = non_eds_count) %>%
    mutate(subgroup = "NONEDS")
  
  # combine non-eds to regular dataframe
  df <- bind_rows(df, eds)
  
  # filter for needed demographics, and recode demographics
  demos <- c("MALE", "EDS", "HISP", "ALL", "BLCK", "FEM", "WHTE", "NONEDS")
  
  df <- df %>% 
    filter(subgroup %in% !!demos)
  
  # recode type from demo
  df$type <- ifelse(df$subgroup == "ALL", "Comparison Community",
                    ifelse(df$subgroup %in% c("BLCK","HISP","WHTE"), "Race / Ethnicity",
                           ifelse(df$subgroup %in% c("MALE", "FEM"), "Gender",
                                  ifelse(df$subgroup %in% c("EDS", "NONEDS"), "Economic Status", "No match"))))
  
  subtype_recode <- c(ALL = "Total",
                      FEM = "Female",
                      MALE = "Male",
                      BLCK = "African American",
                      HISP = "Hispanic / Latino",
                      WHTE = "White, non-hispanic",
                      EDS = "Economically Disadvantaged",
                      NONEDS = "Non-economically Disadvantaged")
  
  df$subgroup <- recode(df$subgroup, !!!subtype_recode)
  
  # only keep needed columns and change column names to match shiny app rewuirements
  df <- df %>%
    mutate(total_pct = total_pct / 100,
           success = round(den * total_pct, 0)) %>%
    select(year = reporting_year, geo_description = school_code, type, 
           subtype = subgroup, estimate = total_pct, trials = den, success)
  
  return(df)

}


# create urls to s3 buckets for all years of data
url_prefix <- "https://nc-school-data.s3.amazonaws.com/act/data-report"
url <- str_c(url_prefix, seq(2014, 2017, 1), ".csv.gz")

# run cleaning function for 2014-2017
schools_old <- map_df(url, clean_old_schools)

# create urls to s3 buckets for all years of data
url <- str_c(url_prefix, c(2018, 2019), ".csv.gz")

# run cleaning function for 2018-2018
schools_new <- map_df(url, clean_new_schools)

# bind old and new, and add a column that combines years
schools <- bind_rows(schools_old, schools_new) %>%
  mutate(year = as.integer(year),
         year_concat = glue('{year-1} - {year}')) %>%
  # reorder columns by placing years first
  select(year, year_concat, geo_description, type, everything())

write_csv(schools, "education/act/cleaned_data/act_cleaned.csv")

# get individual schools ---------------------------------------------------

# list of school codes of individual schools that we do not plot
remove_schools <- c(340568, 340446, 340362, 340311, 340488)

url <- "https://nc-school-data.s3.amazonaws.com/act/data-report2019.csv.gz"

ind_schools <- read_csv(url) %>%
  # only want Forsyth County schools
  filter(school_code == 340)

# filter down dataset to only include each school's overall ACT score
ind_schools_list <- ind_schools %>%
  filter(# don't need demographic information for individual schools, only want all students
         subgroup == 'ALL',
         # want ACT scores
         subject == "ACT",
         # don't want county-wide metrics for data set of individual schools
         school_name != "Forsyth County Schools",
         # remove schools we don't want to plot
         !(code %in% !!remove_schools)) %>%
  # divide act score by 100, so the percentage is converted to a decimal
  # then, round to 2 decimal places
  mutate(total_pct = as.numeric(total_pct) / 100) %>%
  select(school_code = code, school_name, act_score = total_pct)

write_csv(ind_schools_list, "education/act/cleaned_data/act_schools_cleaned.csv")




