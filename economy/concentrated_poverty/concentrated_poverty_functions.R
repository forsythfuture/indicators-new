library(tidyverse)

get_table <- function(table_num, year) {
  
  var_names <- load_variables(year, "acs5/subject")
  
  # imports acs data
  df <- get_acs(geography = "tract", 
                table = table_num, 
                state = "NC",
                survey = "acs5",
                year = year,
                moe_level = 95) %>%
    left_join(var_names, by = c("variable" = "name")) %>%
    select(-variable, -concept, -GEOID) %>%
    mutate(year = !!year)
  
  return(df)
}

calc_conc_pov <- function(poverty_tract, poverty_perc, total_pop_re, poverty_total_re) {
  
  # parameters:
  #  poverty_tract: master dataframe of census tracts with poverty information from table S1701
  #  poverty_tract: dataframe that shows whether census tract is in concentrated poverty
  #  total_pop_re: regular expression to filter for rows containg the census tract's total population
  #  poverty_total_re: regular expression to filter for rows containing the total number of people
  #                     in poverty in the census tract
  
  # add whether census tract is concentrated poverty to primary dataset
  poverty_tract <- left_join(poverty_tract, poverty_perc[c('NAME', 'conc_poverty', 'year')], by = c('NAME', 'year')) 
  
  # filter for rows that contain overll number of people in poverty per census tract
  total_pop <- poverty_tract %>%
    filter(str_detect(label, !!total_pop_re)) %>%
    mutate(merged_label = str_extract(label, 'Population.*')) %>%
    select(NAME, total_estimate = estimate, total_se = se, year, comparison, conc_poverty, merged_label)

  poverty_total <- poverty_tract %>%
    filter(str_detect(label, !!poverty_total_re)) %>%
    mutate(merged_label = str_extract(label, 'Population.*')) %>%
    select(NAME, poverty_estimate = estimate, poverty_se = se, year, comparison, merged_label)
  
  # combine the poverty percent and poverty total datasets
  conc_pov_perc <- left_join(total_pop, poverty_total, by = c('NAME', 'year', 'comparison', 'merged_label'))
  
  return(conc_pov_perc)
  
}

# create function to calculate total concentracted poverty
conc_pov <- function(df, state = FALSE) {
  
  # parameters:
  #  df: dataframe from calc_conc_pov function
  #  state: boolean (TRUE / FALSE) whether calculating rate for entire state
  
  group_one <- if (state == F) c('year', 'comparison', 'merged_label', 'conc_poverty') else c('year', 'merged_label', 'conc_poverty')
  group_two <- if (state == F) c('year', 'comparison', 'merged_label') else c('year', 'merged_label')
  
  # filter out non-comparison communities if state is FALSE
  if (state == F) {
    df <- df %>%
      filter(comparison != "State")
  }
  
  # calculate
  df <- df %>%
    # drop census tracts without enough people to create estimates (have NA values)
    drop_na(conc_poverty) %>%
    group_by_at(group_one) %>%
    # calculate the total number of people in concentrated poverty and
    # not in concetranted poverty in each comp. community, along with total population
    summarize(in_conc_poverty = sum(poverty_estimate),
              in_conc_poverty_se = sqrt(sum(poverty_se^2)),
              total_pop = sum(total_estimate),
              total_se = sqrt(sum(total_se^2))) %>%
    ungroup()
  
  # create bootstrapped population, since we will create bootstrapped standard errors
  df$bootstrap_pop <- rnorm(n = length(df$total_pop), df$total_pop, df$total_se)
  
  df <- df %>%
    group_by_at(group_two) %>%
    mutate(geography_pop = sum(total_pop),
           geography_se = sqrt(sum(total_se^2)),
           geography_pop_boot = sum(bootstrap_pop),
           perc_in_conc_pov = total_pop / geography_pop,
           perc_in_conc_pov_boot = bootstrap_pop / geography_pop_boot,
           # non-bootstrapped standard-error, does not take account of uncertainty
           # of whether census tract itself is greater than 40% poverty rate
           se_regular = moe_prop(total_pop, geography_pop, total_se, geography_se)) %>%
    ungroup() %>%
    filter(conc_poverty == T)
  
  # add comparison column that says state, if state == T
  if (state == T) {
    df[['comparison']] <- 'NC'
  }

  df <- df %>%
    select(comparison, year, merged_label, perc_in_conc_pov, perc_in_conc_pov_boot, se_regular)
  
  return(df)
  
}

# creates a boolean (TRUE / FALSE) of whether census tract is in concentrated povetry (greater 40% poverty)
# used to create bootstrapped confidence intervals for metric
bootstrap_conc_pov <- function(estimate, se) {
  
  bootstrap_estimate <- rnorm(n = length(estimate), mean = estimate, sd = se)
  
  conc_poverty <- ifelse(bootstrap_estimate > 40, T, F)
  
  return(conc_poverty)
  
}

conc_pov_bootstrap <- function(conc_pov_df, pov_perc_estimate, pov_perc_se, n, state = F) {
  
  # This function calculates bootstrapped percentage of residents in concentrated poverty
  # parameters:
  #  conc_pov_df: dataframe created from calc_conc_pov
  #  pov_perc_estimate: column of census tract poverty rate estimates
  #  pov_perc_se: column of census tract poverty rate standard errors
  #  note: the previous two columns must be the same length as conc_pov_df, and msut also be in the
  #  same census tract order
  #  n: grouping variable added to final dataframe, so it is easier to create bootsrapped standard errors
  
  # replace the concentrated poverty status for each census tract
  # with the bootstrapped concentrated poverty status
  conc_pov_df[['conc_poverty']] <- bootstrap_conc_pov(pov_perc_estimate, pov_perc_se)
  
  # calculate total concentrated poverty for geographic area
  conc_pov_df <- conc_pov(conc_pov_df, state)
  
  # add number signifying bootstrapp iteration
  conc_pov_df[['iteration']] <- n
  
  return(conc_pov_df)

}

bootstrap_se <- function(conc_pov_df, pov_perc_estimate, pov_perc_se, num_iterations = 5000, state = F) {
  
  # This function calculates the bootstrapped standard errors
  # Parameters (All parameters are from the conc_pov_bootstrap function):
  #  conc_pov_df: dataframe created from calc_conc_pov
  #  pov_perc_estimate: column of census tract poverty rate estimates
  #  pov_perc_se: column of census tract poverty rate standard errors
  #  note: the previous two columns must be the same length as conc_pov_df, and msut also be in the
  #  same census tract order
  #  num_iterations: number of bootstrap iterations
  #  state: boolean, whether calculating state-level estimate (TRUE) or county-level (FALSE)
  
  # calculate standard errors
  map_df(seq(1, num_iterations), conc_pov_bootstrap, conc_pov_df = conc_pov_df,
                                  pov_perc_estimate = pov_perc_estimate, pov_perc_se = pov_perc_se,
                                  state = state) %>%
    select(-perc_in_conc_pov, -se_regular) %>%
    group_by(comparison, year, merged_label) %>%
    # bootstrapped standard error is just standard deviation of bootstrapped percentages
    summarize(se_bootstrap = sd(perc_in_conc_pov_boot)) %>%
    ungroup()
  
}


# function creates percentage of people in poverty who live in concentrated poverty census tracts
poverty_and_conc_poverty <- function(df, state = F) {
  
  group_one <- if (state == F) c('year', 'comparison', 'conc_poverty') else c('year', 'conc_poverty')
  group_two <- if (state == F) c('year', 'comparison') else 'year'
  
  # in poverty and conc. poverty tract / total number in poverty
  
  # filter out non-comparison communities if state is FALSE
  if (state == F) {
    df <- df %>%
      filter(comparison != "State")
  }
  
  df <- df %>%
    # drop census tracts without enough people to create estimates (have NA values)
    drop_na(conc_poverty) %>%
    group_by_at(group_one) %>%
    # calculate the total number of people in concentrated poverty and
    # not in concetranted poverty in each comp. community, along with total population
    summarize(in_conc_poverty = sum(poverty_estimate),
              in_conc_poverty_se = sqrt(sum(poverty_se^2)),
              total_pop = sum(total_estimate),
              total_se = sqrt(sum(total_se^2))) %>%
    ungroup() %>%
    group_by_at(group_two) %>%
    mutate(total_in_poverty = sum(in_conc_poverty),
           total_in_poverty_se = sqrt(sum(in_conc_poverty_se^2)),
           #poverty_and_conc = total_in_poverty + total_pop,
           #poverty_and_conc_se = sqrt(total_in_poverty^2 + total_pop^2),
           estimate = in_conc_poverty / total_in_poverty,
           se = moe_prop(in_conc_poverty, total_in_poverty, in_conc_poverty_se, total_in_poverty_se)) %>%
    ungroup() %>%
    filter(conc_poverty == T)
  
  if (state == T) {
    df[['comparison']] <- "NC"
  }
  
  df <- df %>%
    select(comparison, year, estimate, se)
  
  return(df)
}
