# matts_ms_model 
# Last updated on 2023-10-11 by Krista Kaput

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

# load in the simulator data from Matt with the data 

matt_model_raw <- read_csv("data/raw/FinanceSimulationData (7).csv")

# load in the edbuildr data to get the urbanicity and sparsity data 
ms_fy19_raw <- masterpull(data_year = "2019", 
                          data_type = "geo")  |>
  filter(State == "Mississippi") |> 
  rename_with(tolower)  

# Clean EdBuild data to get the poverty rate and the students per square mile ------
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
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, urbanicity, mpv, mhi)

# Clean Matt's data -------

matt_model <- matt_model_raw |>
  rename_with(tolower) |>
  rename(district = name,
         dist_id = id, 
         total_enroll = "total enrollment (fy23)",
         maep_rev_fy24 = "revenue: maep (fy24)",
         assessed_value = "assessed value (fy22)",
         matt_model_state_local_rev = "calculations: formula amount",
         direct_cert_pct = "direct certification % (fy24)",
         stpov_pct = "census poverty % (fy21)",
         st_sq_mile = "students per square mile") |>
  mutate(dist_id = str_pad(dist_id, 4, pad = "0", side = "left")) |>
  select(dist_id, district, total_enroll, maep_rev_fy24, assessed_value, matt_model_state_local_rev,
         direct_cert_pct, stpov_pct, st_sq_mile) |>
  # State share and local mills 
  mutate(mill_1_value = assessed_value / 1000, 
         mill_1_value_pp = mill_1_value / total_enroll, 
         local_funding_28_mill = mill_1_value * 28, 
         local_share_mills = maep_rev_fy24 - local_funding_28_mill,
         local_share_pp_mills = local_share_mills / total_enroll) |>
  # 28% of total funding to determine local revenue 
  mutate(local_funding_27_pct = maep_rev_fy24 * .27,
         local_share_pct = maep_rev_fy24 - local_funding_27_pct, 
         local_share_pp_pct = local_share_pct / total_enroll) |>
  mutate(local_share_expected_total = ifelse(local_funding_28_mill < local_funding_27_pct, 
                                             local_funding_28_mill, local_funding_27_pct),
         local_share_expected_pp = local_share_expected_total / total_enroll, 
         maep_expected_local_total = maep_rev_fy24 + local_share_expected_total, 
         maep_expected_local_pp = maep_expected_local_total / total_enroll) |>
  # Matt MS model calculations 
  mutate(matt_model_state_local_pp = matt_model_state_local_rev / total_enroll) |>
  # difference calculations 
  mutate(total_diff =  matt_model_state_local_rev - maep_expected_local_total) |>
  left_join(ms_fy19, by = "dist_id")

# SCATTERPLOT 1: Before and after -------

# plot before vs after

ggplot() +
  geom_point(data = matt_model,
             aes(x = maep_expected_local_pp, y = matt_model_state_local_pp,
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = matt_model |>
                    filter(matt_model_state_local_pp > 8900),
                  aes(x = maep_expected_local_pp, y =  matt_model_state_local_pp,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = matt_model |>
                    filter(maep_expected_local_pp > 8800,
                           matt_model_state_local_pp < 9000),
                  aes(x = maep_expected_local_pp, y =  matt_model_state_local_pp,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = matt_model |>
                    filter(matt_model_state_local_pp < 8300,
                           maep_expected_local_pp > 8350,),
                  aes(x = maep_expected_local_pp, y =  matt_model_state_local_pp,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = matt_model |>
  #                   filter(maep_expected_local_pp < 8500 &
  #                            matt_model_state_local_pp < 7550),
  #                 aes(x = maep_expected_local_pp, y = matt_model_state_local_pp,
  #                     label = district),
  #                 size = text_repel_size) +
  geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10,
                  labels = comma_format()) +
  labs(x = "Total (MAEP + Expected Local) Funding Per-Pupil, FY24",
       y = "Model 6 Funding Per-Pupil, FY24",
       color = "Direct Cert %",
       size = "Enrollment",
       # shape = "LEA Type",
       title = "Change in FY24 Per-Pupil Funding, Model 6") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))

ggsave("figures/model6_vs_maep_local_expected.png", units = "in",
       height = 5.9, width = 10)

# Write CSV ----

write_csv(matt_model, "data/matt_model.csv")




