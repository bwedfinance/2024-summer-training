# class_5_summary_example 
# lasted updated 2024-05-22 by Krista Kaput

# load -----

library(tidyverse)
library(readxl)
library(dplyr)
library(edbuildr)

options(scipen = 999)

# load in the MCA school data 

mca_frpl_school_fy22_raw <- read_excel("slides/class_5/2022PublicMCAMTASReading.xlsx", 
                                       sheet = "School")

# Clean the MCA reading data -------

mca_frpl_school_fy22_clean <- mca_frpl_school_fy22_raw |>
  # Makes the variables in lowercase
  rename_with(tolower) |>
  # We are choosing the grade that is inclusive of all testing data 
  filter(grade == 0) |>
  # Renaming the variables in accordance with the tidy principles
  rename(dist_number = "district number",
         dist_type = "district type",
         district = "district name",
         total_tested = "total tested",
         does_not_meet_count = "count level d",
         partially_meets_count = "count level p",
         meets_count = "count level m",
         exceeds_count = "count level e") |>
  # We are converting the column values from a character to a number. We do this 
  # so we can calculate percentages for each proficiency level
  mutate(does_not_meet_count = as.numeric(does_not_meet_count, na.rm = T),
         partially_meets_count = as.numeric(partially_meets_count, na.rm = T),
         meets_count = as.numeric(meets_count, na.rm = T),
         exceeds_count = as.numeric(exceeds_count, na.rm = T)) |>
  # Creating the district ID so that we can summarize the data by district 
  mutate(dist_number = str_pad(dist_number, width = 4, pad = "0"),
         dist_id = paste(dist_type, dist_number, sep = "")) |>
  # We are selecting the variables that we will need to do the district summary
  select(dist_id, district, total_tested, does_not_meet_count,
         partially_meets_count, meets_count, exceeds_count)


# Summarize the school data to create a district summary -----

mca_frpl_district_summary <- mca_frpl_school_fy22_clean |>
  # We are going to group the data by the dist_id, which is the unique 
  # district identifier we created 
  group_by(dist_id) |>
  # The "first" indicates to keep the first value. So this will keep the 
  # identification for district and dist_id
  summarise(dist_id = first(dist_id), 
            district = first(district),
            # The "sum" totals the values of the columns we are specifying
            total_tested = sum(total_tested, na.rm = T),
            does_not_meet_count = sum(does_not_meet_count, na.rm =T),
            partially_meets_count = sum(partially_meets_count, na.rm = T),
            meets_count = sum(meets_count, na.rm = T),
            exceeds_count = sum(exceeds_count, na.rm = T))

# We can also make a state summary -----

mca_frpl_state_summary <- mca_frpl_district_summary |>
  # We do not need to use group_by() because we are only going to have one value
  summarise(total_tested = sum(total_tested, na.rm = T),
            does_not_meet_count = sum(does_not_meet_count, na.rm =T),
            partially_meets_count = sum(partially_meets_count, na.rm = T),
            meets_count = sum(meets_count, na.rm = T),
            exceeds_count = sum(exceeds_count, na.rm = T)) |>
  # We are adding a column to identify that the state is Minnesota
  mutate(state = "Minnesota") |>
  select(state, everything())

# tidy work place ----

rm(mca_frpl_school_fy22_raw)
  
  
  



