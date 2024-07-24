# 6_mn_comprev_policy_4
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


source("slides/week_08/mn_example/scripts/7_mn_comprev_policy_3.R")

# Model the policy change -----

# hard code in the variables 

reduced_weight <- 0.75

funding_cap <- 0.8

weight_factor <- 0.6

mn_pp_base <- 6567

base_subtractor <- 839


# Increase the reduced weight and lift the 80% cap 

comprev_schools_policy_4 <-comprev_schools_model |>
  # I changed the number from 0.5 to 0.75
  mutate(comprev_reduced_count_policy4 = reduced_lunch * reduced_weight, 
         
         comprev_frpl_reduced_total_policy4 = comprev_reduced_count_policy4 + free_lunch, 
         
         comprev_frpl_reduced_pct_policy4 = comprev_frpl_reduced_total_policy4/enroll, 
         
         comprev_weight_factor_step_policy4 = comprev_frpl_reduced_pct_policy4/funding_cap, 
         
         
         # comprev_weight_factor_policy4 = ifelse(comprev_weight_factor_step_policy4 > 1, 
         #                                        1, comprev_weight_factor_step_policy4), 
         # 
         # determine the compensatory revenue pupil unit count 
         comprev_comp_pupil_unit_policy4 = comprev_frpl_reduced_total_policy4 *comprev_weight_factor_step_policy4 * weight_factor, 
         
         # determine the amount of money that the school generates
         # I removed the base subtractor
         comprev_total_policy4 = comprev_comp_pupil_unit_policy4 * (mn_pp_base - base_subtractor), 
         
         # per-pupil amount for the school that generates the funds 
         comprev_pp_policy4 = comprev_total_policy4/frpl_total,
         
         policy4_pp_increase = comprev_pp_policy4 - comprev_pp)



# Summarize the data at the district and state level -----

# district summary 
comprev_policy4_district_summary <- comprev_schools_policy_4 |>
  select(district_school_num, comprev_total_policy4) |>
  left_join(comprev_schools_model, by = c("district_school_num")) |>
  group_by(district) |>
  summarise(district = first(district), 
            comprev_total_policy4 = sum(comprev_total_policy4, na.rm = T),
            comprev_total = sum(comprev_total, na.rm = T), 
            frpl_total = sum(frpl_total, na.rm = T)) |> 
  mutate(comprev_total_diff = comprev_total_policy4 - comprev_total,
         comprev_pp_diff = comprev_total_diff  / frpl_total) 


# State summary 
comprev_policy4_state_summary <- comprev_policy4_district_summary  |>
  summarise(comprev_total_policy4 = sum(comprev_total_policy4, na.rm = T),
            comprev_total = sum(comprev_total, na.rm = T), 
            frpl_total = sum(frpl_total, na.rm = T)) |> 
  mutate(comprev_total_diff = comprev_total_policy4 - comprev_total,
         comprev_pp_diff = comprev_total_diff  / frpl_total)

# Chart 2: Graph the compensatory model with changing the reduced weight  -----

ggplot(comprev_schools_policy_4, aes(x = frpl_pct,
                                     y =  policy4_pp_increase, 
                                     size = enroll,
                                     color = bipoc_pct, group = 1)) + 
  geom_point(alpha = .8) +
  scale_color_viridis(end = .8, direction = -1) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(0, 1000)) +
  labs(x = "Student FRPL Rate, 2019-20", 
       y = "Increase in Comp. Rev. Per-Pupil Funding, 2020-21",
       title = "Impact of Increased Reduced Price Weight on Per-Pupil Funding",
       size = "Enrollment", 
       color = "BIPOC %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, size = 10))


ggsave("slides/week_08/mn_example/figures/plot3_policy4_comp_rev.png", units = "in", 
       height = 5, width = 8)
