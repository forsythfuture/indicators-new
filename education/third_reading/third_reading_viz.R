library(tidyverse)
library(aws.s3)
library(FFtools)

path <- "education/third_reading/cleaned_data/"
path_viz <- "education/third_reading/visualization/"

data <- read_csv(str_c(path, "third_reading.csv"))

# add dual year to school year (example: from 2019 to 2018-2019)

data$year <- str_c(data$year-1, "-", data$year)

# common parameters for all charts
line_axis <- "% Grade Level Proficient in 3rd Grade Reading"
bar_axis <- "2018-2019 | % Grade Level Proficient in 3rd Grade"
geo_order <- c("Forsyth County, NC", "Guilford County, NC", "Durham County, NC",
               "North Carolina")

# chart for comparison communities ---------------------------------

# create chart
compare_chart <- ff_plot_compare(data, "Comparison Community", line_axis, bar_axis, percent = T,
                                 geography_order = geo_order)

# save file of chart as html page
cat(repr::repr_html(compare_chart), file = str_c(path_viz, "third_reading_compare.html"))

# send to s3
put_object(file = str_c(path_viz, "third_reading_compare.html"), 
           object = "reading/third_reading_compare.html", 
           bucket = "indicators-plots",
           acl = "public-read")

# chart for gender ------------------------------------------------------
gender_chart <- ff_plot_demo(data, "Gender", line_axis, bar_axis, 
                             percent = T, color_palette = "Cold")

# save file of chart as html page
cat(repr::repr_html(gender_chart), file = str_c(path_viz, "third_reading_gender.html"))

# send to s3
put_object(file = str_c(path_viz,  "third_reading_gender.html"), 
           object = "education/third_reading_gender.html", 
           bucket = "indicators-plots",
           acl = "public-read")

# chart for race ------------------------------------------------------
race <- ff_plot_demo(data, "Race / Ethnicity", line_axis, bar_axis, 
                             percent = T, "Dark 3")

# save file of chart as html page
cat(repr::repr_html(race), file = str_c(path_viz, "third_reading_race.html"))

# send to s3
put_object(file = str_c(path_viz,  "third_reading_race.html"), 
           object = "education/third_reading_race.html", 
           bucket = "indicators-plots",
           acl = "public-read")

# chart for EDS ------------------------------------------------------
eds_chart <- ff_plot_demo(data, "Economic Status", line_axis, bar_axis, 
                          percent = T, color_palette = "Dynamic")

# save file of chart as html page
cat(repr::repr_html(eds_chart), file = str_c(path_viz, "third_reading_eds.html"))

# send to s3
put_object(file = str_c(path_viz,  "third_reading_eds.html"), 
           object = "education/third_reading_eds.html", 
           bucket = "indicators-plots",
           acl = "public-read")