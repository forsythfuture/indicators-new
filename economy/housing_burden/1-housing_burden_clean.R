##########################################################################
#
# This script imports the PUMs data that is needed for housing cost burden
# and performs intermediate calculations
#
# Run this script first!!
#
###########################################################################

library(tidyverse)
library(data.table)
library(glue)

current_year <- 2018

source('economy/housing_burden/housing_burden_functions.R')

# create list of links to PUMS housing and population files
years <- seq(2007, current_year) - 2000
years <- str_pad(years, 2, "0", side = 'left')
pop_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_pop/ss{years}pnc.csv.gz')
house_files <- glue('https://censuspums.s3.amazonaws.com/oneyear/nc_housing/ss{years}hnc.csv.gz')

# housing PUMs variables
house_vars <- c('SERIALNO', # serial number of housing unit; used to match housing units and populations 
                'ST', # state
                'PUMA', # four digit PUMA code
                'TYPE', # Type of husing unit; 1 is housing unit, which are the only units we need
                'OCPIP', # housing costs as a percentage of income
                'GRPIP' # gross rent as a percentage of income
                )

# initialize empty dataframe to add all years to
housing_burden <- data.frame()

for (i in seq_along(years)) {
  
  yr <- 2000 + as.numeric(years[i])
  print(yr)
  
  # population variables that are needed
  # msut be defined in year loop because name depends on year
  pop_vars <- c('SERIALNO', # serial number, matches with housing unit serial number
                # relationship to reference person; variable name changed in 2010
                ifelse(yr < 2010, 'REL', 'RELP'),
                'ST', # state
                'PUMA', # PUMA code
                'AGEP', # age
                'RAC1P', # race
                'HISP' # hispanic origin
  )
  
  # import and clean population variables
  pop <- read_csv(pop_files[i],
                  col_types = cols(RT= col_character(),
                                   NAICSP = col_character(),
                                   SOCP = col_character(),
                                   SERIALNO = col_character(),
                                   .default = col_double())) %>%
    # only keep needed variables
    select(!!pop_vars) %>%
    filter(ST == 37 # only keep NC, which is state number 37
           )
  
  # change REL column name to RELP if REL is a column (less than 2010)
  # this allows us to use the same column names for all years
  pop <- if ('REL' %in% colnames(pop)) rename(pop, RELP = REL) else pop
  
  pop <- pop %>%
    # we only want the deomographic data from the person filling out the survey
    # this person is indicated by RELP == 0
    filter(RELP == 0)
  
  # recode race and create age bins
  pop <- clean_demographics(pop)
  
  # import housing variables
  house <- read_csv(house_files[i],
                    col_types = cols(RT= col_character(),
                                     SERIALNO = col_character(),
                                     .default = col_double())) %>%
    # only keep needed variables
    select(!!house_vars, matches("wgtp|WGTP")) %>%
    filter(ST == 37, # only keep NC, which is state number 37
           # only keep housing units; remove institutional units
           TYPE ==1
    ) %>%
      groupings(., 'county', yr) %>%
    # some years have replicate weight column names that are capitalized,
    # while other years have lower case names
    # convert all column names to upper case to ensure rows properly bind
    rename_all(funs(stringr::str_to_upper(.))) #%>%
        #filter(group == 'Forsyth')
  
  # join population data to housing data by merging on serial number and PUMA
  housing_burden <- left_join(house, pop, by = c('SERIALNO', 'PUMA', 'ST')) %>%
    # remove rows that are missing both percentage going to rent and percentage going to housing
    filter(!is.na(OCPIP) | !is.na(GRPIP)) %>%
    # remove unneeded variables
    select(-SERIALNO, -ST, -PUMA, -TYPE, -RELP) %>%
    # convert to long form where each row is either rent or housing percentage share
    gather('housing_status', 'percentage_housing', OCPIP, GRPIP) %>%
    # change wording of housing_status so that it is more descriptive
    mutate(housing_status = recode(housing_status, OCPIP = 'owner',
                                                   GRPIP = 'renter')) %>%
    # remove missing values
    drop_na(percentage_housing) %>%
    # add year and county name
    mutate(year = !!yr) %>%
           #cntyname = 'Forsyth County, NC') %>%
    bind_rows(., housing_burden)
  
}

# reorder columns so that weights are at the end
housing_burden <- housing_burden %>%
  select(GROUP:year, everything())

# write out intermediate results
write_csv(housing_burden, 'economy/housing_burden/clean_data/housing_burden_intermediate.csv.gz')
