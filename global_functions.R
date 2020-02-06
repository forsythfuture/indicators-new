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