# 1_updated_district_data.R
# last updated by Krista Kaput on 2024-04-16

# load ---------------------------------
library(tidyverse)
library(scales)
library(viridis)
library(sf)
library(ggrepel)
library(readxl)
library(edbuildr)
library(edbuildmapr)

bw_primary <- c("#6D1E4A", # 1 plum
                "#007786", # 2 teal
                "#0D525A", # 3 dark green
                "#212B46", # 4 navy
                "#5A6675", # 5 grey
                "#F0DEC1") # 6 cream

bw_secondary <- c("#FFC762", # 1 yellow
                  "#FFB653", # 2 orange
                  "#BEC6CE", # 3 light grey
                  "#2E1A4A", # 4 deep purple
                  "#7EA2D1", # 5 soft blue
                  "#CAD3FB", # 6 lavender
                  "#9CD4EA", # 7 sky
                  "#FFA497") # 8 peach

text_repel_size <- 2.5

options(scipen = 999)

updated_ms_enroll_raw  <- read_excel("data/UPDATED_2024-04-16_FinanceSimulationData (1).xls")

ms_data <- read_csv("data/processed/current_modeling_addon_data.csv")

# Clean the updated enrollment data ------

updated_ms_enroll_clean <- updated_ms_enroll_raw |>
  rename_with(tolower) |>
  rename(dist_id = id, 
         total_enroll = "total enrollment (fy23)",
         direct_cert_pct = "direct certification % (fy24)",
         ell_enroll = "ell (fy23)",
         vocational_enroll = "vocational (fy23)",
         sped_tier1_pct = "sped tier 1 %",
         sped_tier2_pct = "sped tier 2 %",
         sped_tier3_pct = "sped tier 3 %",
         students_sq_mile = "students per square mile",
         sparsity_multiplier = "calculations: sparsity multiplier %",
         matt_wsf_pp = "calculations: formula amount pp",
         matt_wsf_total = "calculations: formula amount") |>
  mutate(sped_tier_1_enroll = ceiling(sped_tier1_pct * total_enroll), 
         sped_tier_2_enroll = ceiling(sped_tier2_pct * total_enroll), 
         sped_tier_3_enroll = ceiling(sped_tier3_pct * total_enroll),
         sped_enroll = sped_tier_1_enroll + sped_tier_2_enroll + 
           sped_tier_3_enroll, 
         econ_dis_enroll = ceiling(direct_cert_pct * total_enroll)) |> 
  select(dist_id, total_enroll, direct_cert_pct, ell_enroll, vocational_enroll, 
         sped_tier_1_enroll, sped_tier_2_enroll, sped_tier_3_enroll, sped_enroll,
         sparsity_multiplier, students_sq_mile, matt_wsf_pp, matt_wsf_total) |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0"))


# Remove the enrollment data we don't want ------

ms_data_no_enroll <- ms_data |>
  select(-total_enroll, -direct_cert_pct, -vocational_enroll, -ell_enroll, -autism, -deaf_blind, 
         -developmentally_delayed, -emotional_disability, -hearing_impaired, 
         -intellectual_disability, -language_speech_impaired, -multiple_disabilities,
         -other_health_impairment, -orthopedic_impairment, -specific_learning_disability,
         -traumatic_brain_injury, -visually_impaired, -sped_enroll, -shortName,
         -sped_tier_1_enroll, -sped_tier_2_enroll, -sped_tier_3_enroll, econ_dis_enroll)

# Join the data with funding data ------

updated_ms_modeling_data <- ms_data_no_enroll |>
  left_join(updated_ms_enroll_clean, by = "dist_id") |>
  select(dist_id, district, total_enroll, everything())
  

# Export the data -------

write_csv(updated_ms_modeling_data, "data/processed/updated_2024-04-16-ms_modeling_data.csv")










