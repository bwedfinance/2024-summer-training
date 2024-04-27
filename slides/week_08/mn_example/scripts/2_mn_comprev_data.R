# 2_mn_comprev_data 
# Last edited by Krista Kaput on 2022-07-24

# load ------

library(tidyverse)
library(ggplot2)
library(scales) # formats text in charts
library(edbuildr) # data analysis and tables from EdBuild
library(plotly) # interactive charts
library(readxl)
library(viridis)
library(stringr)   

#Load in FRPL School data for 2020-21 from House Research
comprev_house_raw <- read_excel("slides/week_08/mn_example/mn_comp_rev-main/Kaput_Final_Bellwether_Project/comprev_fall2020.xlsx", 
                                sheet = "MDEFall2020", skip = 1)

#Load in the data that has the urbanicity by schools 
nces_urbanicity_raw <- read_excel("slides/week_08/mn_example/mn_comp_rev-main/Kaput_Final_Bellwether_Project/NCES_ID_Urbanicity.xls", 
                                  skip = 14)


#Load in the other student demographic data including race/ethnicity, students with disabilities, etc. and filter out extraneous rows
mn_enrollment_raw <- read_excel("slides/week_08/mn_example/mn_comp_rev-main/Kaput_Final_Bellwether_Project/MDE 2020-21 Enrollment.xlsx",
                                sheet = "School")

# clean ----- 

# add zeros and then combine the district and school numbers, as well as the district type together so that we can do a left join. We are combining all of these together so that we can ultimately combine them with urbanicity in the NCES data 
comprev_house <- comprev_house_raw %>% 
  mutate(type_new = str_pad(Type, 2, side = "left", pad = 0), # add a zero to the district type 
         school_new = str_pad(Sch, 4, side = "left", pad = 0), # add a zero to the school type
         district_new = str_pad(DstNum, 3, side = "left", pad = 0), # add a zero to the district number
         district_school_num = paste(type_new, district_new, school_new, sep = "_")) #combine the district_num_type with the school number - district type, district number, and then school number


mn_enrollment <- mn_enrollment_raw %>%
  filter(Grade == "All Grades") %>% # only keep total school enrollment
  rename(school = "School Name",
         district_num = "District Number",
         school_num = "School Number",
         district_type = "District Type") %>% 
  mutate_at(.vars = vars(school_num, district_type, district_num), .funs = as.numeric) %>%  # create unique school id
  mutate(school_num = str_pad(school_num, 4, side = "left", pad = 0),
         district_type = str_pad(district_type, 2, side = "left", pad = 0),
         district_num = str_pad(district_num, 3, side = "left", pad = 0),
         district_school_num = paste(district_type, district_num, school_num, sep = "_"))

# merge the comprev with the mn enrollment data by school name so that I have more demographic data with the enrollment numbers 
mn_comprev_joined <- comprev_house %>%
  left_join(mn_enrollment, by = c("district_school_num" = "district_school_num")) %>%
  rename(year = FY,
         enroll = "Total Enrollment", 
         free_lunch = Free,
         reduced_lunch = RedPr, 
         county_name = "County Name",
         county_num = "County Number",
         district = "District Name",
         ecdev_region = "Economic Development Region",
         bipoc_total = "Total Students of Color or American Indian Count",
         sped_total = "Total Students Receiving Special Education Services Count",
         el_total = "Total English Learner Identified Count") %>%
  mutate(bipoc_pct = bipoc_total/enroll,
         sped_pct = sped_total/enroll,
         el_pct = el_total/enroll,
         frpl_total = reduced_lunch + free_lunch,
         frpl_pct = frpl_total/enroll) %>%
  select(district, school, enroll, reduced_lunch, free_lunch, frpl_total, frpl_pct, district_type, district_school_num, ecdev_region, bipoc_total, sped_total, el_total, bipoc_pct, sped_pct, el_pct, county_name, county_num)


#Recode the nces dataframe so that I can split it up and then put it together again so that we can have an urbanicity column 

nces_urbanicity_raw <- nces_urbanicity_raw %>%
  rename(state_district_id = "State School ID")

nces_split <- nces_urbanicity_raw %>%
  separate(state_district_id, c("state", "district_id", "district_school_num"), "-") 

##I have to turn district_school_num into a number 
nces_split$district_school_num <- as.numeric(nces_split$district_school_num) 

mn_comprev_joined <- mn_comprev_joined %>%
  separate(district_school_num, c("combine", "this", "together"), "_")  

#I removed the zeroes so that I can combine the three rows so that I can do urbanicity
#I have to add a number infront of "this"
mn_comprev_joined$combine_use = str_remove(mn_comprev_joined$combine, "^0+")

mn_comprev_joined$together_use = str_remove(mn_comprev_joined$together, "^0+")

mn_comprev_joined$this_use <- str_pad(mn_comprev_joined$this, width=4, side="left", pad="0")

mn_comprev_joined$together_use <- str_pad(mn_comprev_joined$together_use, width=3, side="left", pad="0")

#Combine the three columns together so that I can combine the datasets

mn_comprev_joined$district_school_num <- paste0(mn_comprev_joined$combine_use, mn_comprev_joined$this_use, mn_comprev_joined$together_use)

mn_comprev_joined$district_school_num <- as.numeric(mn_comprev_joined$district_school_num) 

#Join mn_comprev_joined and the nces split data together 

mn_comprev_final <- mn_comprev_joined %>%
  left_join(nces_split, by = c("district_school_num" = "district_school_num")) %>%
  rename(urbanicity = "Locale*",
         urbanicity_num = "Locale Code*") %>%
  select(district, school, enroll, reduced_lunch, free_lunch, frpl_total, frpl_pct, district_school_num, urbanicity_num, urbanicity, bipoc_total, sped_total, el_total, bipoc_pct, sped_pct, el_pct)
