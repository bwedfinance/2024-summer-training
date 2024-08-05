# ms_modeling_check
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

ms_data <- read_csv("data/processed/current_modeling_data.csv")

# hard code formula elements -----------

base_amount <- 6300

ed_weight <- .30

conc_pov_weight <- .20
direct_cert_cutoff <- .40



sped_tier_1_weight <- .10
sped_tier_2_weight <- .40
sped_tier_3_weight <- 1.50

sparsity_limit <- 10

ell_weight <- .10

gifted_weight <- .05 

gr_9_12_weight <- .05

cte_weight <- .15 

max_mills <- 28 

max_local_pct <- .27


# run model ----------------------------
ms_modeling_check <- ms_data |>
  # high school enrollment 
  mutate(high_school_enroll = gr_9_enroll + gr_10_enroll + gr_11_enroll + gr_12_enroll, 
         
         # gifted enrollment 
         gifted_enroll = total_enroll * .05,

         # sparsity weight adjustment
         sparsity_weight_adj = case_when(st_per_sq_mile > sparsity_limit ~ 0,
                                         # if districts are between the max and min rates, the get a scaled
                                         # version of the concentrated poverty weight
                                         st_per_sq_mile <= sparsity_limit ~ (sparsity_limit - st_per_sq_mile) /
                                           100,
                                         # districts below the minimum threshold get nothing
                                         TRUE ~ 0),

         # # concentrated poverty weight adjustment
         # conc_pov_weight_adj = case_when(direct_cert_pct >= conc_pov_max_pct ~ 1,
         #                                 # if districts are between the max and min rates, the get a scaled
         #                                 # version of the concentrated poverty weight
         #                                 direct_cert_pct >= conc_pov_min_pct ~ (direct_cert_pct - conc_pov_min_pct) /
         #                                   (conc_pov_max_pct - conc_pov_min_pct),
         #                                 # districts below the minimum threshold get nothing
         #                                 TRUE ~ 0),
         
         
         # concentrated poverty weight adjustment 
         
         conc_pov_weight_adj = case_when(direct_cert_pct < direct_cert_cutoff ~ 0,
                                         direct_cert_pct >= direct_cert_cutoff ~ direct_cert_pct - direct_cert_cutoff,
                                         TRUE ~ 0), 
         
         conc_pov_adm = conc_pov_weight_adj * total_enroll, 
          
         #concentrated poverty funding
         conc_pov_funding = base_amount * conc_pov_adm * conc_pov_weight,
         

         # base funding 
         base_funding = total_enroll * base_amount,
         



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
         
         # ell funding
         ell_funding = ifelse(is.na(ell_enroll),
                              0, 
                              base_amount * ell_weight * ell_enroll),
         
         # high school funding 
         high_school_funding = ifelse(is.na(high_school_enroll),
                                      0, 
                                      base_amount * gr_9_12_weight * high_school_enroll),
         
         # gifted funding 
         gifted_funding = ifelse(is.na(gifted_enroll),
                                 0, 
                                 base_amount * gifted_weight * gifted_enroll),
         
         # Economically disadvantaged weight 
         ed_funding = ifelse(is.na(econ_dis_enroll),
                             0, 
                             base_amount * ed_weight * econ_dis_enroll),

         # sparsity funding
         sparsity_funding = sparsity_weight_adj * base_amount * total_enroll,
         
         # vocational funding 
         vocational_funding = ifelse(is.na(vocational_enroll),
                                     0, 
                                     base_amount * cte_weight * vocational_enroll),
         
         # total funding
         total_funding = base_funding + sped_tier_1_funding + sped_tier_2_funding + 
           sped_tier_3_funding + high_school_funding + ell_funding + gifted_funding + 
           ed_funding + vocational_funding + conc_pov_funding + sparsity_funding) |>
  select(dist_id, district, total_funding, base_funding, sped_tier_1_funding, 
         sped_tier_2_funding, sped_tier_3_funding, high_school_funding, 
         ell_funding, gifted_funding, ed_funding, vocational_funding, sparsity_funding, 
         conc_pov_funding, direct_cert_pct, conc_pov_weight_adj, conc_pov_adm)


# Write csv -----

write_csv(ms_modeling_check, "data/processed/ms_modeling_check.csv")

