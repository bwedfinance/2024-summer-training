# ms_mill calculations 
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

## clean and calculate 

local_share_modeling <- ms_data |>
  mutate(local_share_total = ifelse(local_funding_28_mill < local_funding_27_pct, 
                                            local_funding_28_mill, local_funding_27_pct)) |>
  mutate(actual_v_local_share_expectation = local_revenue_total - local_share_total,
         expected_local_share_pp = local_share_total / total_enroll, 
         actual_local_share_pp = local_revenue_total / total_enroll, 
         actual_local_pp_diff = actual_local_share_pp - expected_local_share_pp) |>
  mutate(mill_27_value = mill_1_value * 27,
         mill_27_pp = mill_27_value/total_enroll) |>
  mutate(mill_30_value = mill_1_value * 30, 
         mill_30_pp = mill_30_value / total_enroll) |>
  filter(mill_30_pp < 36760) |>
  filter(dist_id != "2545") |>
  filter(dist_id != "1425") |>
  filter(dist_id != "2525") |>
  filter(dist_id != "2515") |>
  filter(dist_id != "2505") |>
  filter(dist_id != "2535") |>
  filter(dist_id != "4225") |>
  
select(district, actual_local_share_pp, expected_local_share_pp, actual_local_pp_diff, 
       local_revenue_total, local_share_total, actual_v_local_share_expectation, everything())

# state summary 

state_summary <- local_share_modeling |>
  summarise(expected_local_share_total = sum(local_share_total, na.rm = T),
            actual_local_share_total = sum(local_revenue_total, na.rm = T)) |>
  mutate(local_diff = actual_local_share_total - expected_local_share_total)


# SCATTERPLOT #1: Differnce in actual local pp and expected local pp and census poverty rates ------

ggplot() +
  geom_point(data = local_share_modeling,
             aes(x = stpov_rate, y = actual_local_pp_diff, 
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = local_share_modeling |>
                    filter(actual_local_pp_diff > 5000),
                  aes(x = stpov_rate, y = actual_local_pp_diff,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = local_share_modeling |>
                    filter(stpov_rate < .15),
                  aes(x = stpov_rate, y = actual_local_pp_diff,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = local_share_modeling |>
                    filter(stpov_rate > .45),
                  aes(x = stpov_rate, y = actual_local_pp_diff,
                      label = district),
                  size = text_repel_size) +
  # geom_abline(color = "black") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10, 
                  labels = comma_format()) +
  labs(x = "Census Poverty Rate, FY21",
       y = "Per Pupil difference in actual FY22 revenue and local share expectation FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %",
       caption = "Sources: MS Department of Revenue, MS Department of Education, and U.S. Census Bureau") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))


ggsave("figures/ms_local_share_modeling/local_diff_pp_v_povrate.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #2: Difference in actual local pp and expected local pp and median household income ------

ggplot() +
  geom_point(data = local_share_modeling,
             aes(x = mhi, y = actual_local_pp_diff, 
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = local_share_modeling |>
                    filter(actual_local_pp_diff > 5000),
                  aes(x = mhi, y = actual_local_pp_diff,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = local_share_modeling |>
                    filter(mhi > 60000),
                  aes(x = mhi, y = actual_local_pp_diff,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(mhi > .45),
  #                 aes(x = mhi, y = actual_local_pp_diff,
  #                     label = district),
  #                 size = text_repel_size) +
  # geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10, 
                  labels = comma_format()) +
  labs(x = "Median household income, FY21",
       y = "Per Pupil difference in actual FY22 revenue and local share expectation FY24",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %",
       caption = "Sources: MS Department of Revenue, MS Department of Education, EdBuild, and U.S. Census Bureau") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))


ggsave("figures/ms_local_share_modeling/local_diff_pp_v_mhi.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #3: ACTUAL PP LOCAL REVENUE BY PER-PUPIL VALUE OF 30 MILLS -----------
ggplot() +
  geom_point(data = local_share_modeling,
             aes(x = mill_30_pp, y = actual_local_share_pp, 
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = local_share_modeling |>
                    filter(actual_local_share_pp > 6300),
                  aes(x = mill_30_pp, y = actual_local_share_pp,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(stpov_rate < .15),
  #                 aes(x = stpov_rate, y = actual_local_pp_diff,
  #                     label = district),
  #                 size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(stpov_rate > .45),
  #                 aes(x = stpov_rate, y = actual_local_pp_diff,
  #                     label = district),
  #                 size = text_repel_size) +
  geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10, 
                  labels = comma_format()) +
  labs(x = "Per-pupil value of 30 mills, FY24",
       y = "Actual per-pupil local revenue, FY22",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %",
       caption = "Sources: MS Department of Revenue, MS Department of Education, and U.S. Census Bureau") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))


ggsave("figures/ms_local_share_modeling/actual_local_pp_v_30_mills_pp.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #4: ACTUAL PP LOCAL REVENUE BY PER-PUPIL VALUE OF 27 MILLS -----------
ggplot() +
  geom_point(data = local_share_modeling,
             aes(x = mill_27_pp, y = actual_local_share_pp, 
                 size = total_enroll, color = direct_cert_pct),
             alpha = .8) +
  geom_text_repel(data = local_share_modeling |>
                    filter(actual_local_share_pp > 6300),
                  aes(x = mill_27_pp, y = actual_local_share_pp,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(stpov_rate < .15),
  #                 aes(x = stpov_rate, y = actual_local_pp_diff,
  #                     label = district),
  #                 size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(stpov_rate > .45),
  #                 aes(x = stpov_rate, y = actual_local_pp_diff,
  #                     label = district),
  #                 size = text_repel_size) +
  geom_abline(color = "black") +
  scale_x_continuous(labels = dollar_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10, 
                  labels = comma_format()) +
  labs(x = "Per-pupil value of 27 mills, FY24",
       y = "Actual per-pupil local revenue, FY22",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Direct Cert %",
       caption = "Sources: MS Department of Revenue, MS Department of Education, and U.S. Census Bureau") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))


ggsave("figures/ms_local_share_modeling/actual_local_pp_v_27_mills_pp.png", units = "in",
       height = 5.9, width = 10)

# SCATTERPLOT #5: ACTUAL PP LOCAL REVENUE BY DIRECT CERTIFICATION %  -----------
ggplot() +
  geom_point(data = local_share_modeling,
             aes(x = direct_cert_pct, y = actual_local_share_pp, 
                 size = total_enroll, color = stpov_rate),
             alpha = .8) +
  geom_text_repel(data = local_share_modeling |>
                    filter(actual_local_share_pp > 6180),
                  aes(x = direct_cert_pct, y = actual_local_share_pp,
                      label = district),
                  size = text_repel_size) +
  geom_text_repel(data = local_share_modeling |>
                    filter(direct_cert_pct > .6),
                  aes(x = direct_cert_pct, y = actual_local_share_pp,
                      label = district),
                  size = text_repel_size) +
  # geom_text_repel(data = local_share_modeling |>
  #                   filter(actual_local_share_pp < 2200),
  #                 aes(x = direct_cert_pct, y = actual_local_share_pp,
  #                     label = district),
  #                 size = text_repel_size) +
  # geom_abline(color = "black") +
  scale_x_continuous(labels = percent_format()) +
  scale_y_continuous(labels = dollar_format()) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 10, 
                  labels = comma_format()) +
  labs(x = "Direct Cert %",
       y = "Actual per-pupil local revenue, FY22",
       # title = "Difference in Per-Pupil Revenue (WSF and State/Local Foundation) by Student FRPL Rate",
       size = "Enrollment",
       color = "Student Poverty Rate",
       caption = "Sources: MS Department of Revenue, MS Department of Education, and U.S. Census Bureau") +
  theme_bw() +
  theme(text = element_text(family = "Avenir", size = 11),
        plot.caption = element_text(hjust = 0))


ggsave("figures/ms_local_share_modeling/actual_local_pp_v_direct_cert_pct.png", units = "in",
       height = 5.9, width = 10)


# Local share working session -----


local_share_actual_v_expected <- local_share_modeling |>
  select(district, actual_local_share_pp, expected_local_share_pp, actual_local_pp_diff, 
         local_revenue_total, local_share_total, actual_v_local_share_expectation,
         local_funding_28_mill, local_funding_27_pct) |>
  rename(expected_local_share_total = local_share_total)

# Export the data -------

write_csv(local_share_modeling, "data/processed/local_share_modeling.csv")

write_csv(local_share_actual_v_expected, "data/processed/local_share_actual_v_expected.csv")

