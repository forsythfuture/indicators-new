library(tidyverse)
library(aws.s3)
library(FFtools)

path <- "economy/employment_rate/cleaned_data/"
path_viz <- "economy/employment_rate/visualization/"

# read in employment non-tract data
data <- read_csv(str_c(path, "employment_update.csv")) %>%
  # remove 2017 for now
  filter(year != 2017)

# chart for comparison communities ---------------------------------

# create chart
compare_chart <- ff_plot_compare(data, "Employment Rate", "Employment Rate (%)", 
                                 "2016 Employment Rate (%)", percent = T)

# save file of chart as html page
cat(repr::repr_html(compare_chart), file = str_c(path_viz, "employment_rate_compare.html"))

# send to s3
put_object(file = str_c(path_viz, "employment_rate_compare.html"), 
           object = "economy/employment_rate_compare.html", 
           bucket = "indicators-plots",
           acl = "public-read")

# chart for race ------------------------------------------------------
race_chart <- ff_plot_demo(data, "Race and Ethnicity", "Employment Rate (%)", 
                           "2016 Employment Rate (%)", percent = T)

# save file of chart as html page
cat(repr::repr_html(race_chart), file = str_c(path_viz, "employment_rate_race.html"))

# send to s3
put_object(file = str_c(path_viz,  "employment_rate_race.html"), 
           object = "economy/employment_rate_race.html", 
           bucket = "indicators-plots",
           acl = "public-read")

# chart for gender ------------------------------------------------------
gender_chart <- ff_plot_demo(data, "Gender", "Employment Rate (%)", "2016 Employment Rate (%)", 
                             percent = T)

# save file of chart as html page
cat(repr::repr_html(gender_chart), file = str_c(path_viz, "employment_rate_gender.html"))

# send to s3
put_object(file = str_c(path_viz,  "employment_rate_gender.html"), 
           object = "economy/employment_rate_gender.html", 
           bucket = "indicators-plots",
           acl = "public-read")
