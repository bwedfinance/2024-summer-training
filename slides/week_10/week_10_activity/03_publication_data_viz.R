# 03_publication_data_viz.R
# 2024-08-12

# load ----------
library(tidyverse)
library(edbuildr)
library(scales)
library(ggrepel)
library(plotly)

dist_raw <- masterpull(data_type = "geo", data_year = "2019")

# clean ---------

# filter to state of interest
state_dist <- dist_raw |> 
  rename_with(tolower) |> 
  filter(state == "YOUR STATE GOES HERE")


# create df of known districts of interest in your state
state_labels <- state_dist |> 
  filter(ncesid %in% c("NCESID goes here", # district name
                       "NCESID goes here", # district name
                       "NCESID goes here", # district name
                       "NCESID goes here"  # district name
  ))

# labeled plot -------------------
ggplot() +
  # create points using main data
  geom_point(data = state_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add another layer of points of interest
  geom_point(data = state_labels,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             color = "red",
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = state_labels,
                   aes(x = mpv, 
                       y = slrpp, 
                       label = name),
                   min.segment.length = 0,
                   box.padding = 1, # adj to make labels closer together/farther apart
                   segment.color = "red") +
  
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  theme_bw()

# interactive plot -------------

# create a variable that has your basic ggplot's code (no label layer)
static_plot <-  ggplot() +
  # create points using main data
  geom_point(data = state_dist,
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

# create interactive plot; identify additional outlier districts
ggplotly(static_plot, label = "text")

# publication plot ---------------

# create df of known districts outliers from interactive analysis
outlier_labels <- state_dist |> 
  filter(ncesid %in% c("NCESID goes here", # district name
                       "NCESID goes here", # district name
                       "NCESID goes here", # district name
                       "NCESID goes here"  # district name
  ))

# join outliers and districts of interest
label_dist <- state_labels |> 
  bind_rows(outlier_labels)

# create publication-quality ggplot with both sets of labels, using ggrepel
ggplot() +
  # create points using main data
  geom_point(data = state_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             alpha = .5) +
  # add another layer of points of interest
  geom_point(data = label_dist,
             aes(x = mpv,
                 y = slrpp, 
                 size = enroll),
             color = "red",
             alpha = .5) +
  # add text labels using geom_label()
  geom_label_repel(data = label_dist,
                   aes(x = mpv, 
                       y = slrpp, 
                       label = name),
                   min.segment.length = 0,
                   box.padding = 1, # adj to make labels closer together/farther apart
                   segment.color = "red") +
  
  scale_size_area(max_size = 10, 
                  label = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_dollar()) +
  # add nice labels!
  labs(x = "",
       y = "",
       size = "",
       title = "")
  theme_bw()

  # save plot ------
  
  ggsave("state_plot.png", units = "in",
         width = 10, height = 5.5,
         plot.background = element_rect(fill = 'transparent', color = NA))
