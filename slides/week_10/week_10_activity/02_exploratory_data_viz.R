# 01_exploratory_data_viz.R
# 2024-08-13

# be sure to install the plotly package before running this script!

# load --------
library(tidyverse)
library(edbuildr)
library(scales)
library(plotly)

dist_raw <- masterpull(data_type = "geo", data_year = "2019")

# clean ---------

# filter to state of interest
nj_dist <- dist_raw |> 
  rename_with(tolower) |> 
  filter(state == "New Jersey")


# create df of known districts of interest
nj_labels <- nj_dist |> 
  filter(ncesid %in% c("3411340", # newark
                       "3410560", # montclair
                       "3410200", # millburn
                       "3401830", # bloomfield
                       "3411880"  # nutley
  ))

# exploratory plotting ----------

# initial plot
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

# use ggplotly on same plot
static_plot <-  ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll,
                 text = name),
             alpha = .5) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

ggplotly(static_plot, label = "text")

