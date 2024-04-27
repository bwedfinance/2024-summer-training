# 5_mn_comprev_policy_1
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


source("slides/week_08/mn_example/scripts/4_mn_comprev_charts.R")

# Model the policy change -----

# Lift the 80% Cap 

comprev_schools_policy_1 <- mn_comprev_final |>
  mutate(comprev_reduced_count = reduced_lunch * .5, 
         
         comprev_frpl_reduced_total = comprev_reduced_count + free_lunch, 
         
         comprev_frpl_reduced_pct = comprev_frpl_reduced_total/enroll, 
         
         comprev_weight_factor = comprev_frpl_reduced_pct/.8, 
         
         # I commented out this code because we are no longer capping it at 1 
         
         # comprev_weight_factor = ifelse(comprev_weight_factor_step > 1, 
         #                                1, comprev_weight_factor_step), 
         
         # determine the compensatory revenue pupil unit count 
         comprev_comp_pupil_unit_policy_1 = comprev_frpl_reduced_total * comprev_weight_factor *.6, 
         
         # determine the amount of money that the school generates
         comprev_total_policy_1 = comprev_comp_pupil_unit_policy_1 * 5599, 
         
         # per-pupil amount for the school that generates the funds 
         comprev_pp_policy1 = comprev_total_policy_1/frpl_total)



# Chart 2: Graph the compensatory model lifting the 80% cap -----

ggplot(comprev_schools_policy_1, aes(x = frpl_pct,
                                  y = comprev_pp_policy1, 
                                  size = enroll,
                                  color = bipoc_pct, group = 1)) + 
  geom_point(alpha = .8) +
  scale_color_viridis(end = .8, direction = -1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(0, 4500)) +
  labs(x = "Student FRPL Rate, 2019-20", 
       y = "Comp. Rev. Per-Pupil Funding, 2020-21",
       title = "Impact of Lifting the 80% Cap for Compensatory Revevenue on Per-Pupil Funding",
       size = "Enrollment", 
       color = "BIPOC %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, size = 10))


ggsave("slides/week_08/mn_example/figures/plot2_policy1_comp_rev.png", units = "in", 
       height = 5, width = 8)






