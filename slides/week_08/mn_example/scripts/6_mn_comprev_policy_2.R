# 6_mn_comprev_policy_2
# Last edited by Krista Kaput on 2022-07-6


# load -------
library(tidyverse)
library(ggplot2)
library(scales) # formats text in charts
library(edbuildr) # data analysis and tables from EdBuild
library(plotly) # interactive charts
library(readxl)
library(viridis)
library(stringr)   


source("slides/week_08/mn_example/scripts/5_mn_comprev_policy_1.R")

# Model the policy change -----

# Increase weight from 0.5 to .75

comprev_schools_policy_2 <-comprev_schools_model |>
  # I changed the number from 0.5 to 0.75
  mutate(comprev_reduced_count_policy2 = reduced_lunch * .75, 
         
         comprev_frpl_reduced_total_policy2 = comprev_reduced_count_policy2 + free_lunch, 
         
         comprev_frpl_reduced_pct_policy2 = comprev_frpl_reduced_total_policy2/enroll, 
         
         comprev_weight_factor_step_policy2 = comprev_frpl_reduced_pct_policy2/.8, 
         
         
         comprev_weight_factor_policy2 = ifelse(comprev_weight_factor_step_policy2 > 1, 
                                                1, comprev_weight_factor_step_policy2), 
         
         # determine the compensatory revenue pupil unit count 
         comprev_comp_pupil_unit_policy2 = comprev_frpl_reduced_total_policy2 * comprev_weight_factor_policy2 * .6, 
         
         # determine the amount of money that the school generates
         comprev_total_policy2 = comprev_comp_pupil_unit_policy2 * 5599, 
         
         # per-pupil amount for the school that generates the funds 
         comprev_pp_policy2 = comprev_total_policy2/frpl_total,
         
         policy2_pp_increase = comprev_pp_policy2 - comprev_pp)


# Chart 2: Graph the compensatory model lifting the 80% cap -----

ggplot(comprev_schools_policy_2, aes(x = frpl_pct,
                                     y =  policy2_pp_increase, 
                                     size = enroll,
                                     color = bipoc_pct, group = 1)) + 
  geom_point(alpha = .8) +
  scale_color_viridis(end = .8, direction = -1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(0, 800)) +
  labs(x = "Student FRPL Rate, 2019-20", 
       y = "Increase in Comp. Rev. Per-Pupil Funding, 2020-21",
       title = "Impact of Increased Reduced Price Weight on Per-Pupil Funding",
       size = "Enrollment", 
       color = "BIPOC %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, size = 10))


ggsave("slides/week_08/mn_example/figures/plot3_policy2_comp_rev.png", units = "in", 
       height = 5, width = 8)

