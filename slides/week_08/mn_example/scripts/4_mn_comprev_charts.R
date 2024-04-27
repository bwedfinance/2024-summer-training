# 4_mn_comprev_chart
# Last edited by Krista Kaput on 2022-07-24


# load -------
library(tidyverse)
library(ggplot2)
library(scales) # formats text in charts
library(edbuildr) # data analysis and tables from EdBuild
library(plotly) # interactive charts
library(readxl)
library(viridis)
library(stringr)   


source("slides/week_08/mn_example/scripts/3_mn_comprev_calculation.R")


# Chart 1: Graph the compensatory Revenue model -----

ggplot(comprev_schools_model, aes(x = frpl_pct,
                                  y = comprev_pp, 
                                  size = enroll,
                                  color = bipoc_pct, group = 1)) + 
  geom_point(alpha = .8) +
  scale_color_viridis(end = .8, direction = -1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(0, 3500)) +
  labs(x = "Student FRPL Rate, 2019-20", 
       y = "Comp. Rev. Per-Pupil Funding, 2020-21",
       title = "MN Comp. Rev. Per-Pupil Funding by Student FRPL Rate",
       size = "Enrollment", 
       color = "BIPOC %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, size = 10))


ggsave("slides/week_08/mn_example/figures/plot1_current_comp_rev.png", units = "in", 
       height = 5, width = 8)







