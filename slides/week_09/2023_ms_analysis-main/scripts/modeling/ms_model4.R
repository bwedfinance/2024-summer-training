# ms_model_4.R
# last updated by Krista Kaput on 2023-09-14

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
ms_shp <- st_read(here::here("data/processed/ms_shapefile.shp"))

# hard code formula elements -----------

base_amount <- 6000

ed_weight <- .20

# conc_pov_weight <- .20
# direct_cert_cutoff <- .40

sped_tier_1_weight <- 0.6
sped_tier_2_weight <- 1.25
sped_tier_3_weight <- 1.7

sparsity_limit <- 5 

ell_weight <- .20

gifted_weight <- .05 

# gr_9_12_weight <- .05

cte_weight <- .10 

max_mills <- 28 

max_local_pct <- .27


# run model ----------------------------
ms_model4 <- ms_data |>
  # community calculations and enrollment 
  mutate(sparsity_weight_adj = case_when(st_per_sq_mile > sparsity_limit ~ 0,
                                         # if districts are between the max and min rates, the get a scaled
                                         # version of the concentrated poverty weight
                                         st_per_sq_mile <= sparsity_limit ~ (sparsity_limit - st_per_sq_mile) /
                                           100,
                                         # districts below the minimum threshold get nothing
                                         TRUE ~ 0),
         # 
         # # concentrated poverty
         # conc_pov_weight_adj = case_when(direct_cert_pct < direct_cert_cutoff ~ 0,
         #                                 # direct_cert_pct >= direct_cert_cutoff ~ direct_cert_pct - direct_cert_cutoff,
         #                                 # TRUE ~ 0), 
         # 
         # conc_pov_adm = conc_pov_weight_adj * total_enroll, 
         
         # # high school students enrollment
         # gr_9_12_enroll = gr_9_enroll + gr_10_enroll + gr_11_enroll + gr_12_enroll,   
         
         # gifted enrollment
         gifted_enroll = total_enroll * .05, 
         
         
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
         
         # # concentrated poverty funding
         # conc_pov_funding = ifelse(is.na(conc_pov_adm),
         #                           0, 
         #                           base_amount * conc_pov_adm * conc_pov_weight),
         # 
         # 
         # #high school funding 
         # gr_9_12_funding = ifelse(is.na(gr_9_12_enroll),
         #                          0, 
         #                          base_amount * gr_9_12_weight * gr_9_12_enroll),
         # 
         
         # total Model 4 funding 
         model_4_total_funding = base_funding + total_sped_funding + ell_funding + 
           ed_funding + gifted_funding + vocational_funding + sparsity_funding,
         
         model_4_pp_funding = model_4_total_funding / total_enroll, 
         
         # Model 4 local share with the mills 
         model_4_local_mills_total = mill_1_value * max_mills,
         
         # Model 4 local share with 28% 
         model_4_local_pct_total = model_4_total_funding * max_local_pct, 
         
         # Apply the if_else to calculate the local share 
         model_4_local_share_total = ifelse(model_4_local_mills_total < model_4_local_pct_total, 
                                            model_4_local_mills_total, model_4_local_pct_total),
         model_4_local_share_pp = model_4_local_share_total / total_enroll,
         
         # Model 4 State share
         model_4_state_share = model_4_total_funding  - model_4_local_share_total,
         model_4_state_share_pp = model_4_state_share / total_enroll) |> 
  
  # Model 4 and current funding differences 
  mutate(model_4_diff_total = model_4_total_funding - maep_expected_local_total, 
         model_4_diff_pp = model_4_pp_funding - maep_expected_local_pp, 
         
         model_4_local_diff_total = model_4_local_share_total - local_revenue, 
         current_local_pp = local_revenue / total_enroll, 
         model_4_local_diff_pp = model_4_local_share_pp - current_local_pp, 
         
         model_4_state_diff_total = model_4_state_share - maep_rev_full_funding_fy24, 
         model_4_state_diff_pp = model_4_state_share_pp - maep_full_pp_fy24) |>
  
  # Difference in per-pupil sped funding 
  mutate(sped_pp_funding = sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll),
         sped_model_4_pp = total_sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll),
         sped_pp_diff = sped_model_4_pp - sped_pp_funding) |>
  # Difference in per-pupil at risk funding 
  mutate(at_risk_pp_funding = at_risk_funding / econ_dis_enroll,
         ed_model_4_pp = ed_funding / econ_dis_enroll,
         ed_pp_diff = ed_model_4_pp - at_risk_pp_funding) |>
  # Difference in per-pupil vocational funding  
  mutate(cte_pp_funding = cte_funding / vocational_enroll,
         cte_model_4_pp = vocational_funding / vocational_enroll,
         cte_pp_diff = cte_model_4_pp - cte_pp_funding) |>
  
  
  # Take out Forest and charter schools 
  filter(mill_1_value_pp < 1000) |>
  filter(dist_id != "2545") |>
  filter(dist_id != "1425") |>
  filter(dist_id != "2525") |>
  filter(dist_id != "2515") |>
  filter(dist_id != "2505") |>
  filter(dist_id != "2535") |>
  filter(dist_id != "4225") |>
  
  select(dist_id, district, sped_pp_diff, model_4_diff_total, model_4_diff_pp, ell_funding, 
         sparsity_funding, model_4_total_funding, model_4_local_share_total, 
         model_4_state_share_pp, everything())


# SCATTERPLOT 1: Before and after -------

# plot before vs after

ggplot() +
  geom_point(data = ms_model4,
             aes(x = maep_expected_local_pp, y = model_4_pp_funding,
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = ms_model4 |>
                    filter(model_4_pp_funding > 8000),
                  aes(x = maep_expected_local_pp, y =  model_4_pp_funding,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = ms_model4 |>
                    filter(maep_expected_local_pp > 8500),
                  aes(x = maep_expected_local_pp, y =  model_4_pp_funding,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = ms_model4 |>
                    filter(model_4_pp_funding < 7100),
                  aes(x = maep_expected_local_pp, y =  model_4_pp_funding,
                      label = district),
                  size = text_repel_size) +
 geom_text_repel(data = ms_model4 |>
                   filter(maep_expected_local_pp > 7400 &
                            model_4_pp_funding < 7550),
                  aes(x = maep_expected_local_pp, y = model_4_pp_funding,
                       label = district),
                   size = text_repel_size) +
  geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10,
                  labels = comma_format()) +
  labs(x = "Total (MAEP + Expected Local) Funding Per-Pupil, FY24",
       y = "Model 4 Funding Per-Pupil, FY24",
       color = "Direct Cert %",
       size = "Enrollment",
       # shape = "LEA Type",
       title = "Change in FY24 Per-Pupil Funding, Model 4") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/model4_vs_maep_local_expected.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #2: Model 3 AND DIRECT CERT % -------
ggplot(ms_model4, aes(x = direct_cert_pct, y = model_4_pp_funding, size = total_enroll, color = urbanicity)) +
  geom_point(alpha = .8) +
  scale_color_manual(values = c("#6D1E4A", "#007786",
                                "#FFC762", "#212B46")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Direct Certification Percent, FY23",
       y = "Model 4 Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Urbanicity") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model4/model4_pp_direct_cert_pct.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #3: MODEL 3 + VALUE OF 1 MILL ----
ggplot(ms_model4, aes(x = mill_1_value_pp, y = model_4_pp_funding, size = total_enroll, color = direct_cert_pct)) +
  geom_point(alpha = .8) +
  scale_color_viridis(labels = percent_format()) +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Value of 1 Mill Per-Pupil, FY23",
       y = "Model 4 Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model4/model4_model_1_pp_value_1_mill.png", units = "in",
       height = 5.9, width = 10)

# Mapping --------

# merge model with map data
ms_model4_dist_map <- ms_shp |>
  left_join(ms_model4, by ="geoid") |>
  mutate(ell_pp = ell_funding / ell_enroll,
         sped_pp = total_sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll),
         sparsity_pp = sparsity_funding / total_enroll) |>
  select(dist_id, model_4_diff_pp, everything())


# Per Pupil funding diff map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = model_4_diff_pp),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in total\nper-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/pp_diff_map.png", units = "in",
       height = 5.9, width = 6)

# ELL funding diff map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = ell_pp),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "English Learner weight\nvalue per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/ell_pp_map.png", units = "in",
       height = 5.9, width = 6)


# Sparsity funding pp map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = sparsity_pp),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Sparsity weight\nvalue per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/sparsity_pp_map.png", units = "in",
       height = 5.9, width = 6)

# # concentrated poverty funding pp map
# ggplot(ms_model4_dist_map) +
#   geom_sf(aes(fill = conc_pov_pp),
#           color = "grey54",
#           size = .5) +
#   scale_fill_viridis(na.value = "grey84",
#                      labels = dollar_format()) +
#   theme_void() +
#   labs(fill = "Concentrated poverty weight\nvalue per-pupil funding") +
#   theme(text = element_text(family = "Avenir", size = 11),
#         plot.caption = element_text(hjust = 0))
# 
# ggsave("figures/ms_model4/conc_pov_pp_map.png", units = "in",
#        height = 5.9, width = 6)

# Sped funding diff map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = sped_pp_diff),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in special \neducation per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/sped_pp__diff_map.png", units = "in",
       height = 5.9, width = 6)


# At risk funding diff map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = ed_pp_diff),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in economically \ndisadvantaged per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/ed_pp_diff_map.png", units = "in",
       height = 5.9, width = 6)

# CTE funding diff map
ggplot(ms_model4_dist_map) +
  geom_sf(aes(fill = cte_pp_diff),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in vocational\neducation per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model4/cte_pp_diff_map.png", units = "in",
       height = 5.9, width = 6)


# state summary --------
model4_state_summary <- ms_model4 |>
  summarise(model_4_total_funding = sum(model_4_total_funding, na.rm = T),
            base_funding = sum(base_funding, na.rm = T),
            sped_tier_1_funding = sum(sped_tier_1_funding, na.rm = T),
            sped_tier_2_funding = sum(sped_tier_2_funding, na.rm = T),
            sped_tier_3_funding = sum(sped_tier_3_funding, na.rm = T), 
            total_sped_funding = sum(total_sped_funding, na.rm = T), 
            ell_funding = sum(ell_funding, na.rm = T),
            gifted_funding = sum(gifted_funding, na.rm = T),
            ed_funding = sum(ed_funding, na.rm = T), 
            sparsity_funding = sum(sparsity_funding, na.rm = T), 
            vocational_funding = sum(vocational_funding, na.rm = T),
            model_4_state_share = sum(model_4_state_share, na.rm = T)) 

# model 2 data for mississippi working session -----
ms_model4_working_session <- ms_model4 |>
  select(-geoid, -model_4_local_diff_pp, -model_4_state_diff_total, - shortName,
         -st_per_sq_mile, -gr_prek_enroll, -gr_1_enroll, -gr_2_enroll, -gr_3_enroll,
         -gr_4_enroll, -gr_5_enroll, -gr_6_enroll, -gr_12_enroll, -gr_8_enroll,
         -gr_9_enroll, -gr_10_enroll, -gr_11_enroll, -gr_7_enroll, -gr_k_enroll,
         -autism, - deaf_blind, -developmentally_delayed, - emotional_disability, 
         - hearing_impaired, -intellectual_disability, -language_speech_impaired, 
         -multiple_disabilities, -other_health_impairment, -orthopedic_impairment, 
         -specific_learning_disability, -traumatic_brain_injury, -visually_impaired,
         -sped_enroll, -sq_miles, -total_ada, -rev_total_local_state, -rev_pp_local_state,
         -local_revenue_total, -local_revenue_pp, - mpv, -mhi, -model_4_local_pct_total, 
         -current_local_pp, -ed_model_4_pp, -local_revenue, -federal_revenue,
         -local_share_pp_pct, -urbanicity, -at_risk_pp_funding, 
         -sped_pp_funding, -sped_model_4_pp, - model_4_state_diff_pp) |>
  select(dist_id, district, total_enroll, model_4_total_funding, model_4_pp_funding, 
         model_4_diff_total, model_4_diff_pp, model_4_state_share,  model_4_state_share_pp, 
         model_4_local_diff_total, model_4_local_share_total, model_4_local_share_pp, 
         model_4_local_diff_total, base_funding, sped_tier_1_funding, sped_tier_2_funding, 
         sped_tier_3_funding, total_sped_funding, ell_funding, ed_funding, gifted_funding, 
         vocational_funding, sparsity_funding, sped_pp_diff, ed_pp_diff, ell_enroll, 
         gifted_enroll, econ_dis_enroll, stpov_enroll,
         sped_tier_1_enroll, sped_tier_2_enroll, sped_tier_3_enroll,
         everything())


# export csv ------

write_csv(ms_model4, "data/processed/ms_model4_data.csv")

write_csv(ms_model4_working_session, "data/processed/ms_model4_2023-12-14_data.csv")

