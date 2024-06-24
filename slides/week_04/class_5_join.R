# class_5_join_example 
# lasted updated 2024-05-22 by Krista Kaput

# load -----

library(tidyverse)
library(readxl)
library(dplyr)
library(edbuildr)

options(scipen = 999)

# load in the Minnesota 2022 MCA Reading data 

mn_mca_frpl_district_fy22_raw <- read_excel("slides/class_5/2022 MCA FRPL MN.xlsx", 
                                    skip = 1)

# load in the Minnesota free and reduced price (FRPL) count 

mn_frpl_district_raw <- read_csv("slides/class_5/SchoolDistrictsFreeReducedPriceLunchEligibility2017to18.csv")

# clean the MCA data ------

mn_mca_frpl_district_fy22 <- mn_mca_frpl_district_fy22_raw |>
  # This makes all of the column names lower cased
  rename_with(tolower) |>
  # We are choosing the grade that is inclusive of all the testing data 
  filter(grade == 0) |>
  # We are renmaing the variables in accordance with the tidy principles
  rename(dist_number = "district number",
         dist_type = "district type",
         district = "district name",
         total_tested = "total tested",
         does_not_meet_count = "count level d",
         partially_meets_count = "count level p",
         meets_count = "count level m",
         exceeds_count = "count level e") |>
  # Selecting the variables that we need 
  select(dist_number, dist_type, district, total_tested, does_not_meet_count,
         partially_meets_count, meets_count, exceeds_count)


# Clean the district FRPL data ------

mn_frpl_district_clean <- mn_frpl_district_raw |>
  rename_with(tolower) |>
  rename(dist_id = "districtnum",
         district = schooldistrict,
         total_enroll = totalstudents, 
         free_lunch_count = freeelignum,
         reduced_lunch_count = reduceelignum, 
         frpl_count = totalelignum) |>
  # We do not need to have district in this data frame because that already exists 
  # in the other data frame. If we kept it then the two columns would differentiated by 
  # "district.x" and "district.y" 
  select(dist_id, district, total_enroll, free_lunch_count, reduced_lunch_count,
         frpl_count)

# Create the unique identifier -----

# We cannot join the data right now because they do not have the same unique identifier. 
# However, we can create the identifier, which will allow us to join the data frames. 

# Create a unique identifer with paste0
mn_mca_frpl_district_fy22_clean <- mn_mca_frpl_district_fy22 |>
  # This tells us that we want the number to have 4 values 
  mutate(dist_number = str_pad(dist_number, width = 4, pad = "0"),
         dist_id = paste0(dist_type, dist_number))

# Another way to do this is with paste
mn_mca_frpl_district_fy22_other <- mn_mca_frpl_district_fy22 |>
  # This tells us that we want the number to have 4 values 
  mutate(dist_number = str_pad(dist_number, width = 4, pad = "0"),
         dist_id = paste(dist_type, dist_number, sep = ""))

# Join the MCA and FRPL data -----

# mn_mca_frpl_data <- mn_mca_frpl_district_fy22_clean |>
#   left_join(mn_frpl_district_clean, by = "dist_id") 
# Once we have shown this example, we will comment this out otherwise we will get 
# en error 

# We have an error because they are incompatible types. The "dist_id" in the 
# "mn_frpl_district_clean" data frame is a number and the "dist_id" variable in the 
# "mn_mca_frpl_district_fy22_clean" is a "character. We can convert either of them, 
# but for this example we will convert the id to a number

mn_mca_frpl_join <- mn_mca_frpl_district_fy22_clean|>
  mutate(dist_id = as.numeric(dist_id, na.rm = T)) |>
  # We join the two data frames together by their unique identifiders 
  left_join(mn_frpl_district_clean, by = "dist_id") 

# We notice that there are two districts. To rectify this we will remove the 
# extra district from one of the data frames. I will choose to remove the district 
# column in mn_frpl_district_clean because it's uppercase 

mn_frpl_district_clean <- mn_frpl_district_clean |>
  # We can use the subtract the sign in front of a column so that it won't be included in the data frame 
  select(-district)

# When we join the data frames there will only be one district column 
mn_mca_frpl_join_no_extra_district <- mn_mca_frpl_district_fy22_clean|>
  mutate(dist_id = as.numeric(dist_id, na.rm = T)) |> # This mean that anything with an NA is read as 0 
  left_join(mn_frpl_district_clean, by = "dist_id") |>
  # This removes the two columns we no longer want
  select(-dist_type, -dist_number) 

# Clean up the joined data frame ----

# We notice that there are no charter schools in the FRPL data, so we will drop that 
mn_mca_frpl_district <- mn_mca_frpl_join_no_extra_district |>
  filter(total_enroll > 0) |>
  # we also notice that the counts fo the MCA tests are characters, and not numbers
  # so we will convert them into numbers 
  mutate(does_not_meet_count = as.numeric(does_not_meet_count, na.rm = T),
         partially_meets_count = as.numeric(partially_meets_count, na.rm = T),
         meets_count = as.numeric(meets_count, na.rm = T),
         exceeds_count = as.numeric(exceeds_count, na.rm = T)) |>
  # This tells R to put dist_id, district, and total_enroll as the first three columns and 
  # everything() tells R to then list the rest of the variables
  select(dist_id, district, total_enroll, everything())

# Export the data -----


# Tidy work space -----

rm(mn_mca_frpl_district_fy22_raw, mn_frpl_district_raw)





