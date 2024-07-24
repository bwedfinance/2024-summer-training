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

# hard code in the variables 

reduced_weight <- 0.5

funding_cap <- 0.8

weight_factor <- 0.6

mn_pp_base <- 6567

base_subtractor <- 839

# Lift the 80% Cap 

comprev_schools_policy_1 <- mn_comprev_final |>
  mutate(comprev_reduced_count = reduced_lunch * reduced_weight, 
         
         comprev_frpl_reduced_total = comprev_reduced_count + free_lunch, 
         
         comprev_frpl_reduced_pct = comprev_frpl_reduced_total/enroll, 
         
         comprev_weight_factor = comprev_frpl_reduced_pct/funding_cap, 
         
         # I commented out this code because we are no longer capping the per-pupil funding 
         
         # comprev_weight_factor = ifelse(comprev_weight_factor_step > 1, 
         #                                1, comprev_weight_factor_step), 
         
         # determine the compensatory revenue pupil unit count 
         comprev_comp_pupil_unit_policy_1 = comprev_frpl_reduced_total * comprev_weight_factor * weight_factor, 
         
         # determine the amount of money that the school generates
         comprev_total_policy_1 = comprev_comp_pupil_unit_policy_1 * (mn_pp_base - base_subtractor), 
         
         # per-pupil amount for the school that generates the funds 
         comprev_pp_policy1 = comprev_total_policy_1/frpl_total)

# Summarize the data at the district and state level -----

# district summary 
comprev_policy1_district_summary <- comprev_schools_policy_1 |>
  select(district_school_num, comprev_total_policy_1) |>
  left_join(comprev_schools_model, by = c("district_school_num")) |>
  group_by(district) |>
  summarise(district = first(district), 
            comprev_total_policy1 = sum(comprev_total_policy_1, na.rm = T),
            comprev_total = sum(comprev_total, na.rm = T), 
            frpl_total = sum(frpl_total, na.rm = T)) |> 
  mutate(comprev_total_diff = comprev_total_policy1 - comprev_total,
         comprev_pp_diff = comprev_total_diff  / frpl_total) |>
  filter(comprev_pp_diff > 0)


# State summary 
comprev_policy1_state_summary <- comprev_policy1_district_summary  |>
  summarise(comprev_total_policy1 = sum(comprev_total_policy1, na.rm = T),
            comprev_total = sum(comprev_total, na.rm = T), 
            frpl_total = sum(frpl_total, na.rm = T)) |> 
  mutate(comprev_total_diff = comprev_total_policy1 - comprev_total,
         comprev_pp_diff = comprev_total_diff  / frpl_total)



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






