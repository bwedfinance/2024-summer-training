# 1_mn_adm_clean
# Last edited by Krista Kaput on 2022-07-24


# load ------

library(tidyverse)
library(readxl)
library(dplyr)

options(scipen = 999)

mn_adm_raw <- read_excel("slides/week_08/mn_example/data/FY 22 adjusted adm by district and grade (1).xlsx", 
                         skip = 1, n_max = 509)


# Clean MN ADM student data -------

mn_adm_clean <- mn_adm_raw |> 
  rename_with(tolower) |>
  mutate(dist_id = str_pad(dst_num, 4, side = "left", 0),
         total_adm = total - adj_adm_ec - adj_adm_pk) |>
  rename(district = dst_nam) |>
  select(dist_id, district, total_adm)

# Calculate Minnesota Base -------

# Step 1: This is the per-pupil base amount that is used for calculating the total base and then the weights 
pp_mn_base <- 6863

# Step 2: Calculate the base amount
mn_base_funding_sch24 <- mn_adm_clean |>
  # total base = school's total adm * 6863
  mutate(mn_base_total = total_adm * pp_mn_base)







