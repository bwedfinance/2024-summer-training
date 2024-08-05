# ms_model_1.R
# last updated by Krista Kaput on 2023-08-23

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

sped_tier_1_weight <- 0.6
sped_tier_2_weight <- 1.25
sped_tier_3_weight <- 1.7

ell_weight <- .20

gifted_weight <- .05 

max_mills <- 28 

max_local_pct <- .27


# run model ----------------------------
ms_model1 <- ms_data |>
  # Mississippi student-based funding model 
  # base funding
  mutate(base_funding = total_enroll * base_amount, 
         
         gifted_enroll = total_enroll * .05, 
         
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
         
         # total model 1 funding 
         model_1_total_funding = base_funding + total_sped_funding + ell_funding + 
           ed_funding + gifted_funding,
         model_1_pp_funding = model_1_total_funding / total_enroll, 
         
         # model 1 local share with the mills 
         model_1_local_mills_total = mill_1_value * max_mills,

         # model 1 local share with 28% 
         model_1_local_pct_total = model_1_total_funding * max_local_pct, 

         # Apply the if_else to calculate the local share 
         model_1_local_share_total = ifelse(model_1_local_mills_total < model_1_local_pct_total, 
                                      model_1_local_mills_total, model_1_local_pct_total),
         model_1_local_share_pp = model_1_local_share_total / total_enroll,
         
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
  
  # Difference in per-pupil sped funding 
  mutate(sped_pp_funding = sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll),
         sped_model_1_pp = total_sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll),
         sped_pp_diff = sped_model_1_pp - sped_pp_funding) |>
  # Difference in per-pupil at risk funding 
  mutate(at_risk_pp_funding = at_risk_funding / econ_dis_enroll,
         ed_model_1_pp = ed_funding / econ_dis_enroll,
         ed_pp_diff = ed_model_1_pp - at_risk_pp_funding) |>
  
  filter(mill_1_value_pp < 1000) |>
  filter(dist_id != "2545") |>
  filter(dist_id != "1425") |>
  filter(dist_id != "2525") |>
  filter(dist_id != "2515") |>
  filter(dist_id != "2505") |>
  filter(dist_id != "2535") |>
  filter(dist_id != "4225") |>

           select(dist_id, district, model_1_diff_total, model_1_diff_pp, total_sped_funding, 
                  ed_funding, ed_pp_diff, model_1_state_local_total, model_1_state_local_pp, model_1_total_funding, 
                  model_1_pp_funding, model_1_local_share_total, model_1_state_local_pp, everything())

# # SCATTERPLOT 1: Before and after -------
# 
# # plot before vs after
# 
# ggplot() +
#   geom_point(data = ms_model1,
#              aes(x = rev_pp_local_state, y = model_1_state_local_pp,
#                  size = total_enroll, color = direct_cert_pct),
#              alpha = .8) +
#   # geom_text_repel(data = ms_model1 |>
#   #                   filter(rev_pp_local_state > 12000),
#   #                 aes(x = rev_pp_local_state, y = model_1_state_local_pp,
#   #                     label = district),
#   #                 size = text_repel_size) +
#   geom_text_repel(data = ms_model1 |>
#                     filter(rev_pp_local_state > 11000,
#                            model_1_state_local_pp > 9500),
#                   aes(x = rev_pp_local_state, y = model_1_state_local_pp,
#                       label = district),
#                   size = text_repel_size) +
#   geom_text_repel(data = ms_model1 |>
#                     filter(rev_pp_local_state < 9500,
#                            model_1_state_local_pp < 9550),
#                   aes(x = rev_pp_local_state, y = model_1_state_local_pp,
#                       label = district),
#                   size = text_repel_size) +
#   geom_abline(color = "black") +
#   scale_x_continuous(labels = dollar_format()) +
#   scale_y_continuous(labels = dollar_format()) +
#   scale_color_viridis(labels = percent_format()) +
#   scale_size_area(max_size = 10,
#                   labels = comma_format()) +
#   labs(x = "Total (MAEP + Local) Funding Per-Pupil, FY24",
#        y = "Model 1 (State + Local) Funding Per-Pupil, FY24",
#        color = "Direct Cert %",
#        size = "Enrollment",
#        shape = "LEA Type",
#        title = "Change in FY24 Total (State + Local) Per-Pupil Funding, Model 1") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 11),
#         plot.caption = element_text(hjust = 0))
# 
# ggsave("figures/ms_model1/model1_vs_current.png", units = "in",
#        height = 5.9, width = 10)
# 
# 
# 
# # scatterplot #1: Model 1 State Share PP and Direct Cert % ------
# 
# ggplot(ms_model1, aes(x = direct_cert_pct, y = model_1_state_share_pp, size = total_enroll, color = urbanicity)) +
#   geom_point(alpha = .8) +
#   scale_color_manual(values = c("#6D1E4A", "#007786",
#                                 "#FFC762", "#212B46")) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
#                      limits = c(5000, 8000)) +
#   # scale_size_area(labels = comma, max_size = 10) +
#   labs(x = "Direct Certification Percent, FY23",
#        y = "Model 1 State Share Per Pupil Funding, FY24",
#        # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
#        size = "Enrollment",
#        color = "Urbanicity") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 12),
#         plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))
# 
# ggsave("figures/ms_model1/model1_state_share_pp_direct_cert_pct.png", units = "in",
#        height = 5.9, width = 10)
# 
# 
# 
# # SCATTERPLOT #2: Model 1 and Direct Cert % ---------
# 
# ggplot(ms_model1, aes(x = direct_cert_pct, y = model_1_state_local_pp, size = total_enroll, color = urbanicity)) +
#   geom_point(alpha = .8) +
#   scale_color_manual(values = c("#6D1E4A", "#007786",
#                                 "#FFC762", "#212B46")) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
#                      limits = c(5000, 17000)) +
#   # scale_size_area(labels = comma, max_size = 10) +
#   labs(x = "Direct Certification Percent, FY23",
#        y = "Model 1 Per Pupil Funding, FY24",
#        # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
#        size = "Enrollment",
#        color = "Urbanicity") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 12),
#         plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))
# 
# ggsave("figures/ms_model1/model1_state_local_pp_direct_cert_pct.png", units = "in",
#        height = 5.9, width = 10)
# 
# # Scatterplot #3: Current MAEP PP and Direct Cert ----
# 
# 
# ggplot(ms_model1, aes(x = direct_cert_pct, y = maep_fy24_pp, size = total_enroll, color = urbanicity)) +
#   geom_point(alpha = .8) +
#   scale_color_manual(values = c("#6D1E4A", "#007786",
#                                 "#FFC762", "#212B46")) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
#                      limits = c(5000, 8000)) +
#   # scale_size_area(labels = comma, max_size = 10) +
#   labs(x = "Direct Certification Percent, FY23",
#        y = "MAEP Per Pupil Funding, FY24",
#        # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
#        size = "Enrollment",
#        color = "Urbanicity") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 12),
#         plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))
# 
# ggsave("figures/ms_model1/model1_maep_pp_direct_cert_pct.png", units = "in",
#        height = 5.9, width = 10)
# 
# # Scatterplot #4: Current MAEP + Local and Direct Cert ----
# 
# 
# ggplot(ms_model1, aes(x = direct_cert_pct, y = rev_pp_local_state, size = total_enroll, color = urbanicity)) +
#   geom_point(alpha = .8) +
#   scale_color_manual(values = c("#6D1E4A", "#007786",
#                                 "#FFC762", "#212B46")) +
#   scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
#   scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
#                      limits = c(5000, 17000)) +
#   # scale_size_area(labels = comma, max_size = 10) +
#   labs(x = "Direct Certification Percent, FY23",
#        y = "MAEP + Local Per Pupil Funding, FY24",
#        # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
#        size = "Enrollment",
#        color = "Urbanicity") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 12),
#         plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))
# 
# ggsave("figures/ms_model1/model1_maep_state_local_pp_direct_cert.png", units = "in",
#        height = 5.9, width = 10)
# 
# 
# # SCATTERPLOT 5: Before and after state share only -------
# 
# # plot before vs after
# 
# ggplot() +
#   geom_point(data = ms_model1,
#              aes(x = maep_fy24_pp, y = model_1_state_share_pp,
#                  size = total_enroll, color = direct_cert_pct),
#              alpha = .8) +
#   # geom_text_repel(data = ms_model1 |>
#   #                   filter(rev_pp_local_state > 12000),
#   #                 aes(x = rev_pp_local_state, y = model_1_state_local_pp,
#   #                     label = district),
#   #                 size = text_repel_size) +
#   geom_text_repel(data = ms_model1 |>
#                     filter(maep_fy24_pp > 6000,
#                            model_1_state_share_pp > 6000),
#                   aes(x = maep_fy24_pp, y = model_1_state_share_pp,
#                       label = district),
#                   size = text_repel_size) +
#   # geom_text_repel(data = ms_model1 |>
#   #                   filter(maep_fy24_pp > 6200),
#   #                 aes(x = maep_fy24_pp, y = model_1_state_share_pp,
#   #                     label = district),
#   #                 size = text_repel_size) +
#   # geom_text_repel(data = ms_model1 |>
#   #                   filter( model_1_state_share_pp > 6000),
#   #                 aes(x = maep_fy24_pp, y = model_1_state_share_pp,
#   #                     label = district),
#   #                 size = text_repel_size) +
#   geom_abline(color = "black") +
#   scale_x_continuous(labels = dollar_format()) +
#   scale_y_continuous(labels = dollar_format()) +
#   scale_color_viridis(labels = percent_format()) +
#   scale_size_area(max_size = 10,
#                   labels = comma_format()) +
#   labs(x = "MAEP Funding Per-Pupil, FY24",
#        y = "Model 1 State Funding Per-Pupil, FY24",
#        color = "Direct Cert %",
#        size = "Enrollment",
#        shape = "LEA Type",
#        title = "Change in FY24 State Per-Pupil Funding, Model 1") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 11),
#         plot.caption = element_text(hjust = 0))
# 
# ggsave("figures/ms_model1/model1_vs_current_state_share.png", units = "in",
#        height = 5.9, width = 10)

#SCATTERPLOT #6: MAEP + Expected Local PP v. Model 1 PP  -------

ggplot() +
  geom_point(data = ms_model1,
             aes(x = maep_expected_local_pp, y = model_1_pp_funding,
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = ms_model1 |>
                    filter(maep_expected_local_pp < 7500,
                           model_1_pp_funding > 7625),
                  aes(x = maep_expected_local_pp, y = model_1_pp_funding,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = ms_model1 |>
                    filter(maep_expected_local_pp < 8000,
                           maep_expected_local_pp > 7400,
                           model_1_pp_funding < 7300),
                  aes(x = maep_expected_local_pp, y = model_1_pp_funding,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = ms_model1 |>
                    filter(maep_expected_local_pp > 7800),
                  aes(x = maep_expected_local_pp, y = model_1_pp_funding,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = ms_model1 |>
                    filter(model_1_pp_funding < 6950),
                  aes(x = maep_expected_local_pp, y = model_1_pp_funding,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = ms_model1 |>
  #                   filter(maep_fy24_pp > 6200),
  #                 aes(x = maep_fy24_pp, y = model_1_state_share_pp,
  #                     label = district),
  #                 size = text_repel_size) +
  # geom_text_repel(data = ms_model1 |>
  #                   filter( model_1_state_share_pp > 6000),
  #                 aes(x = maep_fy24_pp, y = model_1_state_share_pp,
  #                     label = district),
  #                 size = text_repel_size) +
  geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10,
                  labels = comma_format()) +
  labs(x = "Total (MAEP + Expected Local) Funding Per-Pupil, FY24",
       y = "Model 1 Funding Per-Pupil, FY24",
       color = "Direct Cert %",
       size = "Enrollment",
       shape = "LEA Type",
       title = "Change in FY24 Per-Pupil Funding, Model 1") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model1/model1_vs_maep_local_expected.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #7: MAEP + EXPECTED LOCAL AND DIRECT CERT % -------
ggplot(ms_model1, aes(x = direct_cert_pct, y = maep_expected_local_pp, size = total_enroll, color = urbanicity)) +
  geom_point(alpha = .8) +
  scale_color_manual(values = c("#6D1E4A", "#007786",
                                "#FFC762", "#212B46")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Direct Certification Percent, FY23",
       y = "MAEP + Expected Local Share Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Urbanicity") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model1/model1_maep_expected_local_direct_cert_pct.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #8: MODEL 1 AND DIRECT CERT % -------
ggplot(ms_model1, aes(x = direct_cert_pct, y =  model_1_pp_funding, size = total_enroll, color = urbanicity)) +
  geom_point(alpha = .8) +
  scale_color_manual(values = c("#6D1E4A", "#007786",
                                "#FFC762", "#212B46")) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Direct Certification Percent, FY23",
       y = "Model 1 Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Urbanicity") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model1/model1_pp_direct_cert_pct.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #9: MAEP + EXPECTED LOCAL + VALUE OF 1 MILL ----
ggplot(ms_model1, aes(x = mill_1_value_pp, y = maep_expected_local_pp, size = total_enroll, color = direct_cert_pct)) +
  geom_point(alpha = .8) +
  scale_color_viridis(labels = percent_format()) +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Value of 1 Mill Per-Pupil, FY23",
       y = "MAEP + Expected Local Share Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model1/model1_maep_expected_local_value_1_mill.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #10: MODEL 1 + VALUE OF 1 MILL ----
ggplot(ms_model1, aes(x = mill_1_value_pp, y = model_1_pp_funding, size = total_enroll, color = direct_cert_pct)) +
  geom_point(alpha = .8) +
  scale_color_viridis(labels = percent_format()) +
  scale_x_continuous(labels = scales::dollar_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                     limits = c(5000, 10000)) +
  # scale_size_area(labels = comma, max_size = 10) +
  labs(x = "Value of 1 Mill Per-Pupil, FY23",
       y = "Model 1 Per-Pupil Funding, FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 12),
        plot.caption = element_text(hjust = 0, color = bw_primary[5], size = 10))

ggsave("figures/ms_model1/model1_model_1_pp_value_1_mill.png", units = "in",
       height = 5.9, width = 10)


# Mapping --------

# merge model with map data
ms_model1_dist_map <- ms_shp |>
  left_join(ms_model1, by ="geoid") |>
  mutate(ell_pp = ell_funding / ell_enroll,
         sped_pp = total_sped_funding / (sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll))

# Per Pupil funding diff map
ggplot(ms_model1_dist_map) +
  geom_sf(aes(fill = model_1_diff_pp),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in total\nper-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model1/pp_diff_map.png", units = "in",
       height = 5.9, width = 6)

# ELL funding diff map
ggplot(ms_model1_dist_map) +
  geom_sf(aes(fill = ell_pp),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "English Learner weight\nvalue per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model1/ell_pp_map.png", units = "in",
       height = 5.9, width = 6)

# Sped funding diff map
ggplot(ms_model1_dist_map) +
  geom_sf(aes(fill = sped_pp_diff),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in special \neducation per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model1/sped_pp__diff_map.png", units = "in",
       height = 5.9, width = 6)


# At risk funding diff map
ggplot(ms_model1_dist_map) +
  geom_sf(aes(fill = ed_pp_diff),
          color = "grey54",
          size = .5) +
  scale_fill_viridis(na.value = "grey84",
                     labels = dollar_format()) +
  theme_void() +
  labs(fill = "Difference in economically \ndisadvantaged per-pupil funding") +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/ms_model1/ed_pp_diff_map.png", units = "in",
       height = 5.9, width = 6)


# State summary --------

 model1_state_summary <- ms_model1 |>
  summarise(model_1_total_funding = sum(model_1_total_funding, na.rm = T),
             base_funding = sum(base_funding, na.rm = T),
             sped_tier_1_funding = sum(sped_tier_1_funding, na.rm = T),
             sped_tier_2_funding = sum(sped_tier_2_funding, na.rm = T),
             sped_tier_3_funding = sum(sped_tier_3_funding, na.rm = T),
             ell_funding = sum(ell_funding, na.rm = T),
             gifted_funding = sum(gifted_funding, na.rm = T),
             ed_funding = sum(ed_funding, na.rm = T),
             model_1_state_share = sum(model_1_state_share, na.rm = T),
             total_sped = sum(total_sped_funding, na.rm = T),
             ell_enroll = sum(ell_enroll, na.rm = T),
             sped_tier_1_enroll = sum(sped_tier_1_enroll, na.rm = T),
             sped_tier_2_enroll = sum(sped_tier_2_enroll, na.rm = T),
             sped_tier_3_enroll = sum(sped_tier_3_enroll, na.rm = T),
             econ_dis_enroll = sum(econ_dis_enroll, na.rm = T),
             total_enroll = sum(total_enroll, na.rm = T),
            econ_dis_enroll = sum(econ_dis_enroll, na.rm = T)) |>
   mutate(total_sped_funding = sped_tier_1_funding + sped_tier_2_funding +
            sped_tier_3_funding,
          total_sped_enroll = sped_tier_1_enroll + sped_tier_2_enroll + sped_tier_3_enroll,
          sped_pct = total_sped_enroll / total_enroll, 
          ell_pct = ell_enroll / total_enroll, 
          ed_pct = econ_dis_enroll / total_enroll)


# # model 1 data for mississippi working session -----
# ms_model1_working_session <- ms_model1 |>
#   select(-geoid, -model_1_local_diff_pp, -model_1_state_diff_total, - shortName,
#          -model_1_state_local_pp, -model_1_state_local_total, -st_per_sq_mile, 
#          -gr_prek_enroll, -gr_1_enroll, -gr_2_enroll, -gr_3_enroll,
#          -gr_4_enroll, -gr_5_enroll, -gr_6_enroll, -gr_12_enroll, -gr_8_enroll,
#          -gr_9_enroll, -gr_10_enroll, -gr_11_enroll, -gr_7_enroll, -gr_k_enroll,
#          -autism, - deaf_blind, -developmentally_delayed, - emotional_disability, 
#          - hearing_impaired, -intellectual_disability, -language_speech_impaired, 
#          -multiple_disabilities, -other_health_impairment, -orthopedic_impairment, 
#          -specific_learning_disability, -traumatic_brain_injury, -visually_impaired,
#          -sped_enroll, -sq_miles, -total_ada, -rev_total_local_state, -rev_pp_local_state,
#          -local_revenue_total, -local_revenue_pp, - mpv, -mhi, -model_1_local_pct_total, 
#          -current_local_pp, -ed_model_1_pp, -local_revenue, -federal_revenue,
#           -local_share_pp_pct, -urbanicity, -at_risk_pp_funding, 
#          -sped_pp_funding, -sped_model_1_pp) |>
#   select(dist_id, district, total_enroll, model_1_total_funding, model_1_pp_funding, 
#          model_1_diff_total, model_1_diff_pp, model_1_state_share,  model_1_state_share_pp, 
#          model_1_local_diff_total, model_1_local_share_total, model_1_local_share_pp, 
#          model_1_local_diff_total, base_funding, sped_tier_1_funding, sped_tier_2_funding, 
#          sped_tier_3_funding, total_sped_funding, ell_funding, ed_funding, gifted_funding, 
#          sped_pp_diff, ed_pp_diff, ell_enroll, gifted_enroll, econ_dis_enroll, stpov_enroll,
#          sped_tier_1_enroll, sped_tier_2_enroll, sped_tier_3_enroll, vocational_enroll, 
#          everything())


# export csv ------
# 
# write_csv(ms_model1, "data/processed/ms_model1_data.csv")
# 
# write_csv(ms_model1_working_session, "data/processed/ms_model1_working_session.csv")


