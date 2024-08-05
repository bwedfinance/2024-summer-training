# 1_clean_district_data.R
# last updated by Krista Kaput on 2023-08-23

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

# load in data from Matt

ms_district_raw <- read_excel("data/raw/workingData (1).xlsx")

# load in the maep local + state funding from Matt 

ms_simulator_raw <- read_csv("data/MS_FinanceSimulationData.csv", 
                                skip = 1)

# load in the edbuildr data to get the urbanicity and sparsity data 
ms_fy19_raw <- masterpull(data_year = "2019", 
                          data_type = "geo")  |>
  filter(State == "Mississippi") |> 
  rename_with(tolower)  

# load in raw special education data 
sped_funding_raw <- read_excel("data/raw/Jennifer Schiess Bellwether FY23 MAEP Allocations.xlsx", 
                          sheet = "Allocations by district", skip = 4)

# load in vocational data 
cte_funding_raw <- read_excel("data/raw/Jennifer Schiess Bellwether FY23 MAEP Allocations.xlsx", 
                               sheet = "Allocations by district", skip = 4)

# load in raw at risk funding 
at_risk_funding_raw <- read_excel("data/raw/Jennifer Schiess Bellwether FY23 MAEP At Risk Funding by District 6_22_23.xlsx")

# clean the data --------

# MS district data from Matt 
ms_district_clean <- ms_district_raw |>
  rename(dist_id = "ID",
         district = name, 
         gr_prek_enroll = gradePreK,
         gr_k_enroll = gradeK,
         gr_1_enroll = grade1,
         gr_2_enroll = grade2,
         gr_3_enroll = grade3,
         gr_4_enroll = grade4,
         gr_5_enroll = grade5,
         gr_6_enroll = grade6,
         gr_7_enroll = grade7,
         gr_8_enroll = grade8,
         gr_9_enroll = grade9,
         gr_10_enroll = grade10,
         gr_11_enroll = grade11,
         gr_12_enroll = grade12,
         total_enroll = totalEnrollment, 
         direct_cert_pct = ISP, 
         vocational_enroll = vocational, 
         ell_enroll = ELL, 
         sped_enroll = spedTotal, 
         assessed_value = assessedValue, 
         maep_rev_fy23 = maepRevenueFY23, 
         maep_rev_fy24 = maepRevenueFY24, 
         maep_rev_full_funding_fy24 = maepRevenueFY24FULLFUNDING, 
         local_revenue = localRevenue, 
         geoid = GEOID, 
         sq_miles = squareMiles, 
         stpov_rate = censusPoverty, 
         federal_revenue = federalRevenue,
         total_ada = totalADA,
         specific_learning_disability = spedSpecific, 
         language_speech_impaired = spedSpeach, 
         developmentally_delayed = spedDev, 
         autism = spedAutism, 
         hearing_impaired = spedHearing, 
         emotional_disability = spedEmotional, 
         orthopedic_impairment = spedOrtho, 
         intellectual_disability = spedIntel, 
         other_health_impairment = spedOHealth, 
         visually_impaired = spedVisual, 
         deaf_blind = spedDBlind, 
         multiple_disabilities = spedMultiple, 
         traumatic_brain_injury = spedTrauma)  |>
  mutate(sped_tier_1_enroll = specific_learning_disability + language_speech_impaired + 
                  developmentally_delayed, 
                sped_tier_2_enroll = autism + hearing_impaired + emotional_disability + orthopedic_impairment +
                  intellectual_disability + other_health_impairment, 
                sped_tier_3_enroll = visually_impaired + deaf_blind + multiple_disabilities + 
                  traumatic_brain_injury) |> 
  mutate(maep_fy23_pp = maep_rev_fy23/total_enroll, 
         maep_fy24_pp = maep_rev_fy24/total_enroll, 
         maep_full_pp_fy24 = maep_rev_full_funding_fy24/total_enroll,
         econ_dis_enroll = direct_cert_pct * total_enroll,
         stpov_enroll = stpov_rate * total_enroll,
         st_per_sq_mile = total_enroll / sq_miles) |>
  # State share and local mills 
  mutate(mill_1_value = assessed_value / 1000, 
         mill_1_value_pp = mill_1_value / total_enroll, 
         local_funding_28_mill = mill_1_value * 28, 
         local_share_mills = maep_rev_full_funding_fy24 - local_funding_28_mill,
         local_share_pp_mills = local_share_mills / total_enroll) |>
  # 28% of total funding to determine local revenue 
  mutate(local_funding_27_pct = maep_rev_full_funding_fy24 * .27,
         local_share_pct = maep_rev_full_funding_fy24 - local_funding_27_pct, 
         local_share_pp_pct = local_share_pct / total_enroll) |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, geoid, district, total_enroll,  st_per_sq_mile, maep_rev_fy23, 
         maep_fy23_pp, maep_rev_fy24, maep_fy24_pp, maep_rev_full_funding_fy24, 
         maep_full_pp_fy24, everything())


# MS simulator data from Matt 

ms_simulator <- ms_simulator_raw |>
  rename_with(tolower) |>
  rename(dist_id = id, 
         rev_total_local_state = "revenue: total (maep + local)",
         rev_pp_local_state = "revenue: total pp (maep + local)",
         local_revenue_total = "revenue: local (fy22)",
         local_revenue_pp = "revenue: local pp (fy22)") |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, rev_total_local_state, rev_pp_local_state, local_revenue_total,
         local_revenue_pp)

# Clean EdBuild data to get the poverty rate and the students per square mile 
ms_fy19 <- ms_fy19_raw |> 
  select(ncesid, state_id, name, county, durbanicity, stpov, stpovrate, sd_area,
         student_per_sq_mile, mpv, mhi) |>  
  # create collapsed urbanicity variable
  mutate(urbanicity = fct_collapse(as.factor(durbanicity),
                                   # no large cities in Alabama 
                                   # "11-City: Large", 
                                   City = c("12-City: Mid-size",
                                            "13-City: Small"),
                                   Suburb = c("21-Suburb: Large",
                                              "22-Suburb: Mid-size",
                                              "23-Suburb: Small"),
                                   Town = c("31-Town: Fringe",
                                            "32-Town: Distant",
                                            "33-Town: Remote"),
                                   Rural = c("41-Rural: Fringe",
                                             "42-Rural: Distant",
                                             "43-Rural: Remote"))) |>
  separate(state_id, c('state', 'dist_id')) |>
  select(dist_id, urbanicity, mpv, mhi)

# Clean the special education funding data 
sped_funding_clean <- sped_funding_raw |>
  rename_with(tolower) |>
  rename(dist_id = 'dist #',
         district = "district name",
         sped_funding = "special education") |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, sped_funding)

# Clean the at risk funding data 
at_risk_funding_clean <- at_risk_funding_raw |>
  rename_with(tolower) |>
  rename(dist_id = "dist  no",
         at_risk_funding = "fy23 at-risk amount") |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, at_risk_funding)

# Clean the CTE funding data 
cte_funding_clean <- cte_funding_raw |>
  rename_with(tolower) |>
  rename(dist_id = 'dist #',
         cte_funding = "vocational education") |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, cte_funding)

  
# Join ----------

current_modeling_data <- ms_district_clean |>
  left_join(ms_simulator, by = "dist_id") |>
  left_join(ms_fy19, by = "dist_id") |>
  left_join(sped_funding_clean, by = "dist_id") |>
  left_join(at_risk_funding_clean, by = "dist_id") |>
  left_join(cte_funding_clean, by = "dist_id") |>
  mutate(local_share_expected_total = ifelse(local_funding_28_mill < local_funding_27_pct, 
                                                 local_funding_28_mill, local_funding_27_pct),
         local_share_expected_pp = local_share_expected_total / total_enroll, 
         maep_expected_local_total = maep_rev_fy24 + local_share_expected_total, 
         maep_expected_local_pp = maep_expected_local_total / total_enroll) |>
  select(geoid, dist_id, district, maep_expected_local_total, maep_expected_local_pp, maep_rev_fy24, 
         local_share_expected_total, local_funding_28_mill, local_funding_27_pct, everything())

# # Scatterplot --------
# 
# ggplot() +
#   geom_point(data = current_modeling_data,
#              aes(x = direct_cert_pct, y = rev_pp_local_state,
#                  size = total_enroll, color = urbanicity),
#              alpha = .8) +
#   geom_text_repel(data = current_modeling_data |>
#                     filter(rev_pp_local_state > 11000),
#                   aes(x = direct_cert_pct, y =  rev_pp_local_state,
#                       label = district),
#                   size = text_repel_size) +
#   geom_text_repel(data = current_modeling_data |>
#                     filter(direct_cert_pct > .6),
#                   aes(x = direct_cert_pct, y =  rev_pp_local_state,
#                       label = district),
#                   size = text_repel_size) +
# geom_abline(color = "black") +
#   scale_x_continuous(labels = percent_format()) +
#   scale_y_continuous(labels = dollar_format()) +
#   scale_color_manual(values = c("#6D1E4A", "#007786",
#                                 "#FFC762", "#212B46")) +
#   scale_size_area(max_size = 10,
#                   labels = comma_format()) +
#   labs(x = "Direct Certification %, FY23",
#        y = "Total (MAEP + Actual Local) Funding Per-Pupil, FY24",
#        color = "Urbanicity",
#        size = "Enrollment",
#        # shape = "LEA Type",
#        title = "MS Per-Pupil Funding by District, FY24") +
#   theme_bw() +
#   theme(text = element_text(family = "Avenir", size = 11),
#         plot.caption = element_text(hjust = 0))
# 
# ggsave("figures/maep_actual_local_pp_v_direct_cert.png", units = "in",
#        height = 5.9, width = 10)


# Write csv -------

write_csv(current_modeling_data, "data/processed/current_modeling_data.csv")









