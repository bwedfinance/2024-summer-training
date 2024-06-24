# quant_plc_class_4.R
# lasted updated 2024-05-01 by Krista Kaput

# load -----

library(tidyverse)
library(readxl)
library(dplyr)
library(edbuildr)

options(scipen = 999)


# load in the raw IDEA Section 611 data 
section_611_raw <- read_excel("slides/class_4/IDEA Part B FY 2011 to FY 2022 .xlsx", 
                              sheet = "Grants to States")

# load in the 2023 Minnesota graduation rate data 
mn_graduation_fy23_raw <- read_excel("slides/class_4/2023 Graduation Indicators.xlsx", 
                           sheet = "District", skip = 4)

# load the 2023 Minnesota graduation count data 
mn_graduation_school_fy23_raw <- read_excel("slides/class_4/2022-23 Graduates.xlsx")

# load the 2023 Illinois finance data 
il_finance_fy23_raw <- read_excel("slides/class_4/Illinois FY23 Data.xlsx", 
                                 sheet = "Finance")

#  load the 2023 Illinois ELA, math, and science data 
il_test_data_fy23_raw  <- read_excel("slides/class_4/Illinois FY23 Data.xlsx", 
                                 sheet = "ELA Math Science")


# Use pivot_longer() function with the special education 611 funding data -------

section_611_longer <- section_611_raw |>
  # Makes all the letters in a column lowercase
  rename_with(tolower) |>
  # select the variables we want to do in the analysis 
  select(state, fy2016, fy2017, fy2018, fy2019, fy2020, fy2021, fy2022) |>
  pivot_longer(cols = starts_with("fy"), # The columns start with "fy"
               names_to = "fiscal_year", # the names of the columns will become a value in the "fiscal_year" column
               values_to = "idea_funding") |> # The values in the "fy" columns will be in the new column "idea_funding"
  # Use the gsub to remove the "fy" from the fiscal years so that the values are just the year
  mutate(fiscal_year = as.numeric(gsub("^fy", "", fiscal_year))) |>
  mutate(idea_category = "Part B, Grants to States")

# Use pivot_wider() function with the 2023 graduation rate data -----

mn_graduation_fy23_wider <- mn_graduation_fy23_raw |>
  # Makes all the letters in a column lowercase
  rename_with(tolower) |>
  # rename the columns so there isn't a space in-between the words
  rename(district = "district name",
         dist_id = "district number",
         dist_type = "district type",
         group_category = "group category",
         student_group = "student group",
         ending_status = "ending status",
         four_yr_grad_pct = "four year percent") |>
  # Select the columns we want to include in our dataframe for the pivot_wider
  select(district, dist_id, dist_type, group_category, student_group, ending_status, four_yr_grad_pct) |>
  # filter so that we are only looking at students who graduated in four years
  filter(ending_status == "Graduate") |>
  # filter the student demographics because we only want to look at race/ethnicity data
  filter(group_category == "Race/Ethnicity") |>
  # Pivot_wider! 
  pivot_wider(names_from = "student_group", # The names of the columns are from the student group column values
              values_from = "four_yr_grad_pct") |> # The four_yr_grad_pct values are the four-year graduation rates 
  # These categories are capitalized and have spaces, so we rename them!
  rename_with(tolower) |>
  rename(white_grad_pct = "white students",
         indigenous_grad_pct = "american indian students",
         asian_grad_pct = "asian students",
         black_grad_pct = "black or african american students",
         latino_grad_pct = "hispanic or latino students",
         multiracial_grad_pct = "two or more races students") |>
  mutate(year = "2023")

# Clean the Minnesota Fy23 graduation rate data by school ------

mn_graduation_school_fy23_clean <- mn_graduation_school_fy23_raw |>
  # using "tolower" to make all the values in the column lowercase
  mutate(dst_nam = tolower(dst_nam),
         sch_nam = tolower(sch_nam)) |>
  # using "str_to_title" makes the first letter of each word capitalized
  mutate(dst_nam = str_to_title(dst_nam),
         sch_nam = str_to_title(sch_nam)) |>
  # using "str_replace_all" turns "Dist" and "Dist."into "District"
  mutate(dst_nam = str_replace_all(dst_nam, c("Dist\\b" = "District", "Dist\\." = "District", "District\\." = "District")))

# Clean the IL data to do a left_join -----

# clean the illinois financial data 
il_test_data_fy23_clean <- il_test_data_fy23_raw |>
  rename_with(tolower) |>
  rename(dist_id = rcdts) 

# clean the illinois testing data - same id
il_finance_fy23_same_id <- il_finance_fy23_raw |>
  rename_with(tolower) |>
  rename(dist_id = rcdts) 

# clean the illinois testing data - different_id
il_finance_fy23_diff_id <- il_finance_fy23_raw |>
  rename_with(tolower) 

# Use left_join() function to join the the illinoi data -------

# EXAMPLE 1: Join them with the same "dist_id" name

example_1_il_finance_testing_fy23 <- il_test_data_fy23_clean |>
  left_join(il_finance_fy23_same_id, by = "dist_id")


# EXAMPLE 2: Join them with different column names, but they're the same values
example_2_il_finance_testing_fy23 <- il_test_data_fy23_clean |>
  left_join(il_finance_fy23_diff_id, by = c("dist_id" = "rcdts"))


# Tidy workplace ------

# This will remove extra dataframes that you don't need anymore
# from your environment to keep it tidy (e.g. raw data frames)
rm(section_611_raw, mn_graduation_school_fy23_raw, mn_graduation_fy23_raw,
   il_finance_fy23_raw, il_test_data_fy23_raw, il_finance_fy23_diff_id, 
   il_finance_fy23_same_id, il_test_data_fy23_clean)
