# ms_model_9.R
# last updated by Krista Kaput on 2023-08-30

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

ms_data <- read_csv("data/processed/current_modeling_addon_updated_charter.csv")

# hard code formula elements -----------


base_amount <- 6700

ed_weight <- .30

conc_pov_weight <- .10
direct_cert_cutoff <- .35

sped_tier_1_weight <- 0.6
sped_tier_2_weight <- 1.25
sped_tier_3_weight <- 1.7

sparsity_limit <- 8

ell_weight <- .20

gifted_weight <- .05 

# gr_9_12_weight <- .05

cte_weight <- .10

max_mills <- 28

max_local_pct <- .30



# run model ----------------------------
ms_model9 <- ms_data |>
  # community calculations and enrollment 
  mutate(sparsity_weight_adj = case_when(st_per_sq_mile > sparsity_limit ~ 0,
                                         # if districts are between the max and min rates, the get a scaled
                                         # version of the concentrated poverty weight
                                         st_per_sq_mile <= sparsity_limit ~ (sparsity_limit - st_per_sq_mile) /
                                           100,
                                         # districts below the minimum threshold get nothing
                                         TRUE ~ 0),
         
         
         # gr_9_12_enroll = gr_9_enroll + gr_10_enroll + gr_11_enroll + gr_12_enroll,   
         
         gifted_enroll = total_enroll * .05, 
         
         # concentrated poverty weight adjustment 
         conc_pov_weight_adj = case_when(direct_cert_pct < direct_cert_cutoff ~ 0,
                                         direct_cert_pct >= direct_cert_cutoff ~ direct_cert_pct - direct_cert_cutoff,
                                         TRUE ~ 0), 
         
         conc_pov_adm = conc_pov_weight_adj * total_enroll, 
         
         
         # Mississippi student-based funding model 
         # base funding
         base_funding = total_enroll * base_amount, 
         
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
         
         # gifted funding 
         gifted_funding = ifelse(is.na(gifted_enroll),
                                 0, 
                                 base_amount * gifted_weight * gifted_enroll),
         
         # vocational funding 
         vocational_funding = base_amount * cte_weight * vocational_enroll,
         
         # sparsity funding 
         sparsity_funding = sparsity_weight_adj * base_amount * total_enroll, 
         
         # concentrated poverty funding
         conc_pov_funding = ifelse(is.na(conc_pov_adm),
                                   0, 
                                   base_amount * conc_pov_adm * conc_pov_weight),
         
         #high school funding 
         # gr_9_12_funding = ifelse(is.na(gr_9_12_enroll),
         #                          0, 
         #                          base_amount * gr_9_12_weight * gr_9_12_enroll),
         
         
         # total model 9 funding 
         model_9_total_funding = base_funding + total_sped_funding + ell_funding + 
           ed_funding + gifted_funding + vocational_funding + conc_pov_funding + 
           + sparsity_funding,
         
         model_9_pp_funding = model_9_total_funding / total_enroll, 
         
         # model 9 local share with the mills 
         model_9_local_mills_total = mill_1_value * max_mills,
         
         # model 9 local share with 28% 
         model_9_local_pct_total = model_9_total_funding * max_local_pct, 
         
         # Apply the if_else to calculate the local share 
         model_9_local_share_total = ifelse(model_9_local_mills_total < model_9_local_pct_total, 
                                            model_9_local_mills_total, model_9_local_pct_total),
         model_9_local_share_pp = model_9_local_share_total / total_enroll,
         
         # model 9 State share
         model_9_state_share = model_9_total_funding  - model_9_local_share_total,
         model_9_state_share_pp = model_9_state_share / total_enroll) |> 
  
  # model 9 and current funding differences 
  mutate(model_9_diff_total = model_9_total_funding - maep_expected_local_total, 
         model_9_diff_pp = model_9_pp_funding - maep_expected_local_pp, 
         
         model_9_local_diff_total = model_9_local_share_total - local_revenue, 
         current_local_pp = local_revenue / total_enroll, 
         model_9_local_diff_pp = model_9_local_share_pp - current_local_pp, 
         
         model_9_state_diff_total = model_9_state_share - maep_rev_full_funding_fy24, 
         model_9_state_diff_pp = model_9_state_share_pp - maep_full_pp_fy24) |>
  # filter(mill_1_value_pp < 1000) |>
  select(dist_id, district, model_9_diff_total, model_9_diff_pp, ell_funding, 
         conc_pov_funding, sparsity_funding, model_9_state_share_pp, everything())


# State summary --------

model9_state_summary <- ms_model9 |>
  summarise(model_9_total_funding = sum(model_9_total_funding, na.rm = T),
            base_funding = sum(base_funding, na.rm = T),
            sped_tier_1_funding = sum(sped_tier_1_funding, na.rm = T),
            sped_tier_2_funding = sum(sped_tier_2_funding, na.rm = T),
            sped_tier_3_funding = sum(sped_tier_3_funding, na.rm = T), 
            total_sped_funding = sum(total_sped_funding, na.rm = T), 
            ell_funding = sum(ell_funding, na.rm = T),
            gifted_funding = sum(gifted_funding, na.rm = T),
            ed_funding = sum(ed_funding, na.rm = T), 
            conc_pov_funding = sum(conc_pov_funding, na.rm = T),
            # gr_9_12_funding = sum(gr_9_12_funding, na.rm = T),
            sparsity_funding = sum(sparsity_funding, na.rm = T), 
            vocational_funding = sum(vocational_funding, na.rm = T),
            model_9_state_share = sum(model_9_state_share, na.rm = T),
            model_9_local_share_total = sum(model_9_local_share_total, na.rm = T),
            model_9_local_mills_total = sum(model_9_local_mills_total, na.rm = T)) 




