clean_demographics <- function(df) {
  
  # This function cleans the demographics of a PUMS dataset.
  # It does the following:
  #   Bin ages
  #   Recode Hispanics as 100 in RAC1P column
  #   Recode race to 1 = white, 2 = AA, 3 = Hispanic, and 4 = other
  # Data frame must have columns for each of these demographics to work
  
  # create age categories to be used
  age_bins <- c(0, 24, 44, 64, 150)
  # create labels that are the end age
  age_labels <- age_bins[-1]
  
  df <- df %>%
    # create age bins
    mutate(AGEP = cut(AGEP, breaks = !!age_bins, 
                      labels = !!age_labels, 
                      include.lowest = TRUE),
           # convert to integer
           AGEP = as.integer(as.character(AGEP)),
           # make race 100 for Hispanic if person is of hispanic origin
           RAC1P = ifelse(.$HISP != 1, 100, .$RAC1P)) %>%
    select(-HISP)
  
  # recode race to group races we will not use into one race
  # this will reduce the number of groups and speed up grouping
  
  race_labels <- c(seq(1, 9), 100)
  race_recode <- c(1, 2, rep(4, 7), 3)
  
  df$RAC1P <- plyr::mapvalues(df$RAC1P, race_labels, race_recode)
  
  return(df)
  
}

groupings <- function(df, level, year) {
  
  # this function determines what groupings to use in calculating Palma
  # Input:
  #     df: dataframe of PUMA values
  #     levels: 'US', 'state', 'county', 'puma'
  
  # convert level to lowercase
  level <- str_to_lower(level)
  
  # replace PUMA codes with county names if true
  if (level %in% c('county', 'counties')) {
    
    # dataframe of all NC puma codes and county names
    # codes are different starting in 2012, so ensure we are pulling in the right year
    if (year > 2011) {
      
      nc_codes <- puma_area_code(letters, 'misc_data/puma_counties.csv') %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    } else {
      
      nc_codes <- puma_area_code(letters, 'misc_data/puma_counties.csv', puma12 = FALSE) %>% 
        distinct(PUMA, .keep_all = TRUE) %>%
        # only keep first word of county
        # needed because names become columns later
        mutate(cntyname = word(cntyname, 1))
      
    }
    
    # add county name to income dataframe
    df <- df %>%
      left_join(nc_codes, by = 'PUMA') %>%
      rename(group = cntyname)
    
  } else if (level %in% c('united states', 'us')) {
    
    # make integer to conserve ram
    df$group <- 0
    
  } else if (level %in% c('state', 'states')) {
    
    df$group <- df$ST
    
  } else if (level %in% c('puma', 'pums')) {
    
    df$group <- df$PUMA
    
  }
  
  return(df)
  
}

puma_area_code <- function(counties, names_file, puma12 = TRUE) {
  
  # This function takes as input a vector of county names
  # and return all PUMA area codes within a county
  
  # convert vector of county names into single RE string
  counties <- paste(counties, collapse = '|')
  
  # create columns to select based on whether user is selcting PUMA10 or PUMA2K
  cols <- if(puma12 == TRUE)  c('puma12', 'cntyname') else c('puma2k', 'cntyname')
  
  # read in table of PUMA names and filter for counties of interest
  puma_names <- read_csv(names_file) %>%
    # filter for needed counties
    filter(str_detect(cntyname, counties)) %>%
    # extract area code for either 2000 or 2010
    select(cols)
  
  # rename first column so that it has the same name, whether from 2K or 2010
  # this is also the same name as the PUMA area code column in the PUMA datasets
  names(puma_names)[1] <- 'PUMA'
  
  # sort by area code
  puma_names <- arrange(puma_names, PUMA)
  
  return(puma_names)
}

find_burden_house <- function(df, wgt) {
  
  # This function calculates whether each household is burdened
  
  #wgt <- enquo(wgt)
  
  # df <- df %>%
  #   select(geography:year, !!wgt) %>%
  #   # extend the number of rows based on the weight
  #   uncount(!!wgt) %>%
  #   mutate(pct_housing = ifelse(pct_housing > 30, 'yes', 'no'))
  print(wgt)
  
  df <- df %>%
    select(geography:year, !!wgt)
  
  # remove rows with weights less than 1
  df <- df[df[[wgt]] >= 1, ]
  
  # expand dataframe  
  df <- df[rep(row.names(df), df[[wgt]]), ]
  
  # calcualte whether homeowner is housing burdened
  df <- df %>%
    mutate(pct_housing = ifelse(pct_housing > 30, 'yes', 'no'))
  
  return(df)
  
}

calculate_burden <- function(df, wgt) {
  
  # This function calculates the percentage of households within
  # a demographic that are burdened
  # Input:
  #   df: a dataframe that has already calculated whether each household
  #       is burdened with the find_burden_house function
  
  # create extended dataset with weights
  #df <- find_burden_house(df, wgt)
  
  # calcualate county trends
  
  df_county <- df  %>%
    filter(geography %in% c('Forsyth', 'Guilford', 'Durham'))
  
  total_trend <- df_county %>%
    group_by(geography, year)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    mutate(type = 'Comparison Community',
           subtype = 'Total')
  
  race <- df_county %>%
    group_by(geography, year, race)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    filter(race != 'Other') %>%
    rename(subtype = race) %>%
    mutate(type = 'Race / Ethnicity')
  
  age <- df_county %>%
    group_by(geography, year, age)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    rename(subtype = age) %>%
    mutate(type = 'Age')
  
  county_burden <- bind_rows(total_trend, race) %>%
    bind_rows(., age)
  
  # caclualte state trends
  
  total_trend <- df %>%
    group_by(year)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    mutate(geography = 'North Carolina',
           type = 'Comparison Community',
           subtype = 'Total')
  
  race <- df %>%
    group_by(year, race)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    filter(race != 'Other') %>%
    rename(subtype = race) %>%
    mutate(geography = 'North Carolina',
           type = 'Race / Ethnicity')
  
  age <- df %>%
    group_by(year, age)%>%
    summarise(estimate = sum(pct_housing == "yes")/n()) %>%
    rename(subtype = age) %>%
    mutate(geography = 'North Carolina',
           type = 'Age')
  
  burden <- bind_rows(county_burden, total_trend) %>%
    bind_rows(., race) %>%
    bind_rows(., age)
  
  return(burden)
  
}

find_se <- function(burden_list) {
    
    # This function takes the list of housing burdens created when
    # 'calcualte_burdens' is used for all replciate weights, and 
    # calculates overall standard error
    
    # calcualte squared difference between primary weight
    # and every replicate weight
    sq_diff <- lapply(seq(2, length(burden_list)),
                      function(x) (burden_list[[1]][3] - burden_list[[x]][3])^2)
    
    # create dataframe to store all squared difference weight,
    # needed all of them in one dataframe so we can sum
    sum_sq_diff <- data.frame(a = rep(100, nrow(sq_diff[[1]])))
    
    # iterate through squared differences list, adding as columns to dataframe
    for (i in seq_along(sq_diff)) {
      
      sum_sq_diff[[i]] = sq_diff[[i]]
      
    }
    
    # sum the difference and multiply by 4/80
    # but first, transpose so that each column is all relicate weights
    # of a given demograhic; this will make it easier to sum all replciate weights
    sum_sq_diff <- sum_sq_diff %>%
      t() %>%
      as.data.frame() %>%
      summarize_all(funs(sqrt(sum(.)*(4/80)))) %>%
      # transpose back so that there is one column and each row represents different demographic
      t() %>%
      as.data.frame()
    
    return(sum_sq_diff)
  
}