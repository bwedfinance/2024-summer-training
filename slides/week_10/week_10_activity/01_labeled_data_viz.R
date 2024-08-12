# 01_labeled_data_viz.R
# 2024-08-13

# be sure to install the ggrepel package before running this script!

# load --------
library(tidyverse)
library(edbuildr)
library(scales)
library(ggrepel)

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

# plot using geom_label ----------

# initial plot
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add text labels using geom_text()
  geom_label(data = nj_labels,
             aes(x = mpv, 
                 y = slrpp, 
                 label = name)) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()


# plot using geom_label_repel ----------

# initial plot
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = nj_labels,
                   aes(x = mpv, 
                   y = slrpp, 
                   label = name)) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

# force repel
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = nj_labels,
                   aes(x = mpv, 
                       y = slrpp, 
                       label = name)) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

# change repel options
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = nj_labels,
                   aes(x = mpv, 
                       y = slrpp, 
                       label = name),
                   min.segment.length = 0,
                   box.padding = 1,
                   segment.color = "red") +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

# add colored layer of points
ggplot() +
  # create points using main data
  geom_point(data = nj_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = nj_labels,
                   aes(x = mpv, 
                       y = slrpp, 
                       label = name),
                   min.segment.length = 0,
                   box.padding = 1,
                   segment.color = "red") +
  # add another layer of points of interest
  geom_point(data = nj_labels,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             color = "red",
             alpha = .5) +
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()


