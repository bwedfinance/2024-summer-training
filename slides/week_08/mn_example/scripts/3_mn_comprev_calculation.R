# 3_mn_comprev_calculation
# Last edited by Krista Kaput on 2022-07-24


# load -------
library(tidyverse)
library(ggplot2)
library(scales) # formats text in charts
library(edbuildr) # data analysis and tables from EdBuild
library(plotly) # interactive charts
library(readxl)
library(viridis)
library(stringr)   


source("slides/week_08/mn_example/scripts/2_mn_comprev_data.R")

# Build Compensatory Revenue Formula ------

# hard code in the variables 

reduced_weight <- 0.5

funding_cap <- 0.8

weight_factor <- 0.6

mn_pp_base <- 6567

base_subtractor <- 839

##Calculate the compensatory revenue per school and rebuild the model
comprev_schools_model <- mn_comprev_final |>
        #Step 1: Split the reduced price student count in half because they are only worth .5 in the compensatory revenue formula 
  mutate(comprev_reduced_count = reduced_lunch * reduced_weight, 
         #Step 2:Add together the .5 reduced price count to the free count to get the corrected frpl number for the compensatory revenue formula 
         comprev_frpl_reduced_total = comprev_reduced_count + free_lunch, 
         comprev_frpl_reduced_pct = comprev_frpl_reduced_total/enroll, #This is the new frpl % with .5 reduced price count
         #Step 3: Divide by the weight factor by .8
         comprev_weight_factor_step = comprev_frpl_reduced_pct/funding_cap, 
         ### I'm keeping this interim step so that I can track it
         #Replace anything above 1 with 1 because of the formula 
         comprev_weight_factor = ifelse(comprev_weight_factor_step > 1, 
                                        1, comprev_weight_factor_step), 
         # determine the compensatory revenue pupil unit count 
         comprev_comp_pupil_unit = comprev_frpl_reduced_total* comprev_weight_factor* weight_factor , 
         # determine the amount of money that the school generates
         comprev_total = comprev_comp_pupil_unit* (mn_pp_base - base_subtractor), 
         # per-pupil amount for the school that generates the funds 
         comprev_pp = comprev_total/frpl_total)





