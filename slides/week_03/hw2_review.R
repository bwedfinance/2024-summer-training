# hw2_review
# last updated by Krista Kaput on 2024-06-018


# load ---------------

# load packages at the top of the script
library(tidyverse)
library(edbuildr)
library(scales)
library(viridis)

# load the data from edbuild
dist_raw_sy19 <- masterpull(data_type = "geo", data_year = "2019")

# create state df --------

# filter ou the Minnesota data 
state_sy19 <- dist_raw_sy19 |>
  # change this filter to apply to your state!
  filter(State == "Minnesota") |>
  # Take out the districts with no enrollment 
  filter(ENROLL > 0) |>
  # Create the categories for urbanicity. We only need to do this once! 
  mutate(urbanicity = fct_collapse(as.factor(dUrbanicity),
                                   City = c("11-City: Large", 
                                            "12-City: Mid-size",
                                            "13-City: Small"),
                                   Suburb = c("21-Suburb: Large",
                                              "22-Suburb: Mid-size",
                                              "23-Suburb: Small"),
                                   Town = c("31-Town: Fringe",
                                            "32-Town: Distant",
                                            "33-Town: Remote"),
                                   Rural = c("41-Rural: Fringe",
                                             "42-Rural: Distant",
                                             "43-Rural: Remote")))


# create bar charts ------

# apply labels and update color theme like we do in a scatter plot 
ggplot(state_sy19, aes(x = MPV)) +
  geom_histogram(mapping = aes(x = MPV), binwidth = 30000) +
  scale_x_continuous(labels = label_dollar())+
  labs(x = "Median Property Value", y = "Number of Counties",
       title = "Distribution of Median Property Values in Minnesota",
       caption = "Source: Edbuild Data, 2019") +
  theme_bw()

# create scatter plots ----------

ggplot(state_sy19, aes(x = StPovRate, y = LRPP, size = ENROLL,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  # adjust color range
  scale_color_viridis_d(end = .8) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_size_area(labels = label_comma(), max_size = 10) +
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Poverty Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment", color = "Urbanicity") +
  theme_bw()





