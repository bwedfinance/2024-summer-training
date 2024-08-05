# state_example_model_1.R
# last updated by Krista Kaput on 2024-08-05

# load ---------------------------------
library(tidyverse)
library(scales)
library(viridis)
library(sf)
library(ggrepel)


options(scipen = 999)
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

# load in the data 
state_example_data <- read_csv("slides/week_09/class_example/data/processed/current_modeling_data.csv")

# hard code formula elements -----------

base_amount <- 7500

ed_weight <- .3

sped_tier_1_weight <- 0.5
sped_tier_2_weight <- 1.1
sped_tier_3_weight <- 1.5

ell_weight <- .2

max_mills <- 20


# run model ----------------------------
state_example_model1 <- state_example_data |>
  # base funding
  mutate(base_funding = total_enroll * base_amount, 

         # special education funding 
         
         # special education tier 1
         sped_tier_1_funding = ifelse(is.na(sped_tier_1_enroll),
                                      0, 
                                      base_amount * sped_tier_1_weight * sped_tier_1_enroll), 
         # special education tier 2
         sped_tier_2_funding = ifelse(is.na(sped_tier_2_enroll),
                                      0, 
                                      base_amount * sped_tier_2_weight * sped_tier_2_enroll), 
         
         # special education tier 3
         sped_tier_3_funding = ifelse(is.na(sped_tier_3_enroll),
                                      0, 
                                      base_amount * sped_tier_3_weight * sped_tier_3_enroll), 
         
         # total special education funding 
         
         total_sped_funding = sped_tier_1_funding + sped_tier_2_funding + sped_tier_3_funding, 
         
         # ELL 
         ell_funding = ifelse(is.na(ell_enroll),
                              0, 
                              base_amount * ell_weight * ell_enroll),
         
         # Economically disadvantaged weight 
         ed_funding = ifelse(is.na(econ_dis_enroll),
                             0, 
                             base_amount * ed_weight * econ_dis_enroll),
         

         # total model 1 funding 
         model_1_total_funding = base_funding + total_sped_funding + ell_funding + 
           ed_funding,
         model_1_pp_funding = model_1_total_funding / total_enroll, 
         
         # model 1 local share with the mills 
         model_1_local_share_total = mill_1_value * max_mills,


         # Apply the if_else to calculate the local share 
         model_1_local_share_pp =  model_1_local_share_total / total_enroll,
         
         # Model 1 State share
         model_1_state_share = model_1_total_funding  - model_1_local_share_total,
         model_1_state_share_pp = model_1_state_share / total_enroll) |> 
  
         # Model 1 and current funding differences 
         mutate(model_1_diff_total = model_1_total_funding - maep_expected_local_total, 
                model_1_diff_pp = model_1_pp_funding - maep_expected_local_pp, 
                
                model_1_local_diff_total = model_1_local_share_total - local_revenue, 
                current_local_pp = local_revenue / total_enroll, 
                model_1_local_diff_pp = model_1_local_share_pp - current_local_pp, 
                
                model_1_state_diff_total = model_1_state_share - maep_rev_full_funding_fy24) |>

  
  mutate(model_1_state_local_total = local_revenue_total + model_1_state_share,
         model_1_state_local_pp = model_1_state_local_total / total_enroll) |>
  
  filter(mill_1_value_pp < 1000) |>
  filter(dist_id != "2545") |>
  filter(dist_id != "1425") |>
  filter(dist_id != "2525") |>
  filter(dist_id != "2515") |>
  filter(dist_id != "2505") |>
  filter(dist_id != "2535") |>
  filter(dist_id != "4225") |>

           select(dist_id, district, model_1_diff_total, model_1_diff_pp, total_sped_funding, 
                   everything())

# # SCATTERPLOT 1: Before and after -------



# State summary --------
