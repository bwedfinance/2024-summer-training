# class_5_example 
# last updated 2024-07-03 by Krista Kaput 


# load ------
options(scipen = 999)

library(tidyverse)
library(edbuildr)
library(tidycensus)
library(viridis)
library(scales)

# get your own api key from https://api.census.gov/data/key_signup.html
# only run this line of code once after you replace the text below
# with your API key
# census_api_key("YOUR API KEY GOES HERE", install = TRUE, overwrite = TRUE)

# get edbuild data
edbuild_fy19 <- masterpull(data_type = "geo")

# load census variables from 2019 acs 5-year estimates
v19 <- load_variables(2019, "acs5", cache = TRUE)

# get mortgage data for unified school districts
mortgage_unified_raw <- get_acs(variables = c("B25101_001", # total households
                                              "B25101_002", # total with a mortgage
                                              "B25101_024"), # total not mortgaged
                                geography = "school district (unified)",
                                state = "MN", 
                                year = 2019)

# get mortgage data for elementary school districts
mortgage_elementary_raw <- get_acs(variables = c("B25101_001", # total households
                                                 "B25101_002", # total with a mortgage
                                                 "B25101_024"), # total not mortgaged
                                   geography = "school district (elementary)",
                                   state = "MN", 
                                   year = 2019)

# get mortgage data for secondary school districts
mortgage_secondary_raw <- get_acs(variables = c("B25101_001", # total households
                                                "B25101_002", # total with a mortgage
                                                "B25101_024"), # total not mortgaged
                                  geography = "school district (secondary)",
                                  state = "MN", 
                                  year = 2019)


# clean ------------

# clean mortgage data for unified school districts
mortgage_pct_unified <- mortgage_unified_raw |> 
  # replace vars with more descriptive names
  mutate(variable = str_replace_all(variable, "B25101_001", "households"),
         variable = str_replace_all(variable, "B25101_002", "with_mortgage"),
         variable = str_replace_all(variable, "B25101_024", "no_mortgage")
  ) |>  # close mutate 
  # remove margin of error column
  select(-moe) |> 
  # pivot variable column into distinct columns
  pivot_wider(names_from = variable, values_from = estimate) |> 
  # calculate percent of households within a school district with a mortgage
  mutate(mortgage_pct = with_mortgage / households) 

# clean mortgage data for elementary school districts
mortgage_pct_elementary <- mortgage_elementary_raw |> 
  mutate(variable = str_replace_all(variable, "B25101_001", "households"),
         variable = str_replace_all(variable, "B25101_002", "with_mortgage"),
         variable = str_replace_all(variable, "B25101_024", "no_mortgage")
  ) |>  # close mutate 
  select(-moe) |> 
  pivot_wider(names_from = variable, values_from = estimate) |> 
  mutate(mortgage_pct = with_mortgage / households)

# clean mortgage data for secondary school districts
mortgage_pct_secondary <- mortgage_secondary_raw |> 
  mutate(variable = str_replace_all(variable, "B25101_001", "households"),
         variable = str_replace_all(variable, "B25101_002", "with_mortgage"),
         variable = str_replace_all(variable, "B25101_024", "no_mortgage")
  ) |>  # close mutate 
  select(-moe) |> 
  pivot_wider(names_from = variable, values_from = estimate) |> 
  mutate(mortgage_pct = with_mortgage / households)

# NOTE: this data isn't really that useful for mn!

# join ----------

# join unified and elementary data by binding rows
mortgage_pct_mn <- mortgage_pct_unified |> 
  bind_rows(mortgage_pct_elementary) |> 
  # filter out summary row
  filter(GEOID != "2199999") |> 
  # arrange from largest to smallest district
  arrange(-households)

# join edbuild and census data using left_join
edbuild_mortgage_mn <- edbuild_fy19 |> 
  filter(State == "Minnesota") |> 
  left_join(mortgage_pct_mn, by = c("NCESID" = "GEOID"))

# do the join again, but this time select for the columns we want to keep
# to avoid duplicates like district.x and district.y
edbuild_mortgage_mn <- edbuild_fy19 |> 
  filter(State == "Minnesota") |> 
  left_join(mortgage_pct_mn |> 
              select(GEOID, households, with_mortgage, mortgage_pct),
            by = c("NCESID" = "GEOID"))

# use anti_join() to check for districts with no mortgage data
edbuild_mortgage_mn_no_match <- edbuild_fy19 |> 
  filter(State == "Minnesota") |> 
  anti_join(mortgage_pct_mn |> 
              select(GEOID,households, with_mortgage, mortgage_pct),
            by = c("NCESID" = "GEOID"))

# run the reverse anti_join to see if the census data has 
#  districts not included in the edbuild table
mortgage_edbuild_mn_no_match <- mortgage_pct_mn |> 
  select(GEOID,households, with_mortgage, mortgage_pct) |> 
  anti_join(edbuild_fy19 |> 
              filter(State == "Minnesota") ,
            by = c("GEOID" = "NCESID"))

# plot -----------

# first plot of the joined dataset
ggplot(edbuild_mortgage_mn) +
  geom_point(aes(x = MHI, y = mortgage_pct, 
                 color = StPovRate, size = ENROLL),
             alpha = .7) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_viridis(labels = label_percent()) +
  labs(x = "MHI", y = "Percent of households with a mortgage",
       color = "Poverty rate", size = "Enrollment") +
  theme_bw()

# facet by sdType
ggplot(edbuild_mortgage_mn) +
  geom_point(aes(x = MHI, y = mortgage_pct, 
                 color = StPovRate, size = ENROLL),
             alpha = .7) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_viridis(labels = label_percent()) +
  labs(x = "MHI", y = "Percent of households with a mortgage",
       color = "Poverty rate", size = "Enrollment") +
  facet_wrap(~sdType) +
  theme_bw()

# filter out secondary districts and create better labels for 
# elementary and unified districts
ggplot(edbuild_mortgage_mn |> 
         filter(sdType != "secon") |> 
         mutate(sdType = case_when(sdType == "elem" ~ "Elementary",
                                   sdType == "uni" ~ "Unified"))) +
  geom_point(aes(x = MHI, y = mortgage_pct, 
                 color = StPovRate, size = ENROLL),
             alpha = .7) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_viridis(labels = label_percent()) +
  labs(x = "MHI", y = "Percent of households with a mortgage",
       color = "Poverty rate", size = "Enrollment") +
  facet_wrap(~sdType) +
  theme_bw()

# same chart, but filter  allow for free x+y axis scales
ggplot(edbuild_mortgage_mn |> 
         filter(sdType != "secon") |> 
         mutate(sdType = case_when(sdType == "elem" ~ "Elementary",
                                   sdType == "uni" ~ "Unified"))) +
  geom_point(aes(x = MHI, y = mortgage_pct, 
                 color = StPovRate, size = ENROLL),
             alpha = .7) +
  scale_size_area(max_size = 10, labels = label_comma()) +
  scale_x_continuous(labels = label_dollar()) +
  scale_y_continuous(labels = label_percent()) +
  scale_color_viridis(labels = label_percent()) +
  labs(x = "MHI", y = "Percent of households with a mortgage",
       color = "Poverty rate", size = "Enrollment") +
  facet_wrap(~sdType, scales = "free") +
  theme_bw()
