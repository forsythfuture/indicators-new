#######################################################################################
# Third grade reading
# 
# Original data comes from:
# http://www.dpi.state.nc.us/accountability/reporting/leaperformancearchive/
# each year the new dataset can be placed in the AWS bucket and the script can be rerun
#
# The codebook can be found in the zip files at the link
#
#######################################################################################

library(tidyverse)
library(glue)

# function that imports and cleans school data for a single year
get_school <- function(url, year) {

  # vector of demographics that we will need from the dataset
  demographics <- c("ALL", # all students 
                    "BLCK", # african-american 
                    "HISP", # hispanic / latino
                    "WHTE", # white
                    "EDS", # economically disadvantaged
                    "NOT_EDS", # not economically disadvantaged 
                    "FEM", # female 
                    "MALE" # male
                    )
  
  # read in aggregate school data
  df <- read_tsv(url) %>%
    filter(grade == "03", # only keep third grade
           subject == "RD", # only keep reading
           type == "ALL", # keep "All" assessment
           subgroup %in% !! demographics # only keep the needed demographics
           )
  
  # vector of school disricts we want to filter for
  code <- c("340LEA", # Forsyth County
            "410LEA", # Guilford County,
            "320LEA", # Durham County,
            "NC-SEA" # state of NC
            )
  
  # filter for needed disricts,
  df <- df %>%
    filter(school_code %in% !!code) %>%
    select(school_code, subgroup, num_tested, pct_glp) %>%
    # convert percent grade level proficient to number, and make a decimal percentage (.55)
    mutate(pct_glp = as.numeric(pct_glp)/100,
           # calculate number of passers; needed to calcualte confidence intervale of binomial
           num_passers = round(num_tested * pct_glp),
           year = !!year)
  
  # create demographic group (type columns) based on subgroup
  df$type <- ifelse(df$subgroup == "ALL", "Comparison Community",
                             ifelse(df$subgroup %in% c("BLCK", "HISP", "WHTE"), "Race / Ethnicity",
                                    ifelse(df$subgroup %in% c("EDS", "NOT_EDS"), "Economic Status",
                                           ifelse(df$subgroup %in% c("FEM", "MALE"), "Gender", ""))))
  
  # recode demographic subgroups to the standard wordings
  demo_lookup <- c(ALL = "Total Population",
                    BLCK = "Black / African-American",
                    EDS = "Economically Disadvantaged",
                    FEM = "Female",
                    HISP = "Hispanic / Latino",
                    MALE = "Male",
                    NOT_EDS = "Non-Economically Disadvantaged",
                    WHTE = "White")
  
  df$subgroup <- recode(df$subgroup, !!!demo_lookup)
  
  # recode lea code as school name
  name_lookup <- c(`340LEA` = "Forsyth County, NC",
                   `410LEA` = "Guilford County, NC",
                   `320LEA` = "Durham County, NC",
                   `NC-SEA` = "North Carolina")
  
  df$geo_description <- recode(df$school_code, !!!name_lookup)

  # select and rename the needed columns,
  # columns must be renamed to match shiny app conventions
  df <- df %>%
    select(year, geo_description, type, subtype = subgroup,
           estimate = pct_glp, success = num_passers, trials = num_tested)
  
  return(df)

}

# create vector of urls to school datasets
prefix_url <- "https://nc-school-data.s3.amazonaws.com/Disag_"
suffix_url <- "_Data.txt.gz"
years_url <- c("2013-14", "2014-15", "2015-16", "2016-17", "2017-18","2018-19")

urls <- str_c(prefix_url, years_url, suffix_url)
years <- seq(2014, 2019, 1)

# iterate though all years, importing data
schools <- map2_df(urls, years, get_school)

schools <- schools %>%
  # add column with total school year ex: 2013 - 2014
  mutate(year_concat = glue("{year-1} - {year}")) %>%
  # reorder columns
  select(year, year_concat, everything())

# write out data as a csv
write_csv(schools, "education/third_reading/cleaned_data/third_reading.csv")

# import individual school data for Forsyth County in the most recent year -------------------

# url to most recent year's dataset
recent_url <- "https://nc-school-data.s3.amazonaws.com/Disag_2018-19_Data.txt.gz"

# import data and filter for needed information
ind_schools <- read_tsv(recent_url) %>%
  filter(grade == "03", # only keep third grade
         subject == "RD", # only keep reading
         type == "ALL", # keep "All" assessment
         subgroup =="ALL", # all students
         str_detect(school_code, "^340[0-9]") # forsyth county schools
  )

# select needed columns
ind_schools <- ind_schools %>%
  mutate(estimate = as.numeric(estimate) / 100) %>%
  select(school_code, school_name = name, third_reading = estimate)

# write out dataset
write_csv(ind_schools, "education/third_reading/cleaned_data/third_reading_schools_cleaned.csv")
  