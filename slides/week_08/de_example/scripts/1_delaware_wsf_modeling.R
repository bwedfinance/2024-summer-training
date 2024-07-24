# 1_delaware_wsf_modeling
# Last edited by Krista Kaput on 2024-07-24


# load -------
library(tidyverse)
library(ggplot2)
library(scales) # formats text in charts
library(edbuildr) # data analysis and tables from EdBuild
library(plotly) # interactive charts
library(readxl)
library(viridis)
library(stringr)   


delaware_modeling_data <- read_csv("slides/week_08/de_example/data/processed/updated_delaware_app_data.csv") 

#Hard code the variables --------
  
  base <- 8000 # per-pupil base funding 
  
  ed_weight <- .5 # economically disadvantaged student weight 
  
  sped_no_complex_intense_weight <- 0.5 # special education weight for students who are not intense or complex 
  sped_complex_weight <- 1.4 # special education weight for complex students 
  sped_intense_weight <- 2.1 # special education weight for intense, which is the state's high level of support

  
  el_weight <- .3 # English learner weight 
  
  cte_weight <- .2 # weight for students enrolled in CTE courses 
  
  
  # Model the student-based funding formula  ------
  
 delaware_wsf_model <- delaware_modeling_data |>
    # Do the WSF modeling 
    # Base funding 
    mutate(wsf_base_funding = total_enroll * base,
           
           # FRPL Funding 
           wsf_frpl_funding = base * frpl_enroll * ed_weight,
           
           # EL Funding 
           wsf_el_funding = base * el_enroll * el_weight,
           
           # SPED Funding
           wsf_sped_no_complex_intense_funding = base * sped_enroll *  sped_no_complex_intense_weight,
           wsf_sped_complex_funding = base * complex_sped_enroll *  sped_complex_weight,
           wsf_sped_intense_funding = base * intense_sped_enroll *  sped_intense_weight,
           wsf_sped_total_funding = wsf_sped_no_complex_intense_funding + wsf_sped_complex_funding + wsf_sped_intense_funding, 
           
           # CTE Funding 
           wsf_cte_funding = base * voc_enroll * cte_weight,
           
           # Total WSF funding
           wsf_total_funding = wsf_base_funding + wsf_el_funding + wsf_sped_total_funding 
           + wsf_frpl_funding + wsf_cte_funding,
           # Calculate the per-pupil funding 
           wsf_pp_funding = wsf_total_funding / total_enroll) |>
    # Calculate the per-pupil differences 
    mutate(wsf_pp_diff = wsf_pp_funding - state_pp_revenue,
           wsf_total_diff = wsf_total_funding - state_revenue)
  
  
  # Plot the funding differences -----
  
  ggplot( delaware_wsf_model, aes(x = frpl_pct,
                                  y = wsf_pp_diff, 
                                  size = total_enroll,
                                  color = frpl_pct, group = 1)) + 
    geom_point(alpha = .8) +
    scale_color_viridis(end = .8, direction = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                       limits = c(-5000, 3000)) +
    labs(x = "Student FRPL Rate", 
         y = "Difference between current and model per-puil funding",
         title = "Delaware Per-Pupil Differences from Resource to Student-Based Funding Formula",
         size = "Enrollment", 
         color = "FRPL %") +
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 12),
          plot.caption = element_text(hjust = 0, size = 10))
  
  
  # Current formula -----
  
  ggplot(delaware_wsf_model, aes(x = frpl_pct,
                                  y = state_pp_revenue, 
                                  size = total_enroll,
                                  color = frpl_pct, group = 1)) + 
    geom_point(alpha = .8) +
    scale_color_viridis(end = .8, direction = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                       limits = c(5000, 15000)) +
    labs(x = "Student FRPL Rate", 
         y = "Current Per-Pupil State Funding",
         title = "Current Per-Pupil Funding in Delaware",
         size = "Enrollment", 
         color = "FRPL %") +
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 12),
          plot.caption = element_text(hjust = 0, size = 10))
  
  ggsave("slides/week_08/de_example/figures/plot1_current_pp_funding.png", units = "in", 
         height = 5, width = 8)
  
  
  # WSF model formula -----
  
  ggplot(delaware_wsf_model, aes(x = frpl_pct,
                                 y = wsf_pp_funding, 
                                 size = total_enroll,
                                 color = frpl_pct, group = 1)) + 
    geom_point(alpha = .8) +
    scale_color_viridis(end = .8, direction = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                       limits = c(5000, 15000)) +
    labs(x = "Student FRPL Rate", 
         y = "Student-Based Per-Pupil Funding",
         title = "Model 1: WSF Per-Pupil Funding in Delaware",
         size = "Enrollment", 
         color = "FRPL %") +
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 12),
          plot.caption = element_text(hjust = 0, size = 10))
  
  
  ggsave("slides/week_08/de_example/figures/plot2_wsf_pp_funding.png", units = "in", 
         height = 5, width = 8)
  
  # Plot the funding differences -----
  
  ggplot( delaware_wsf_model, aes(x = frpl_pct,
                                    y = wsf_pp_diff, 
                                    size = total_enroll,
                                    color = frpl_pct, group = 1)) + 
    geom_point(alpha = .8) +
    scale_color_viridis(end = .8, direction = -1) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::dollar_format(accuracy = 1),
                       limits = c(-5000, 3000)) +
    labs(x = "Student FRPL Rate", 
         y = "Difference between current and model per-puil funding",
         title = "Delaware Per-Pupil Differences from Resource to Student-Based Funding Formula",
         size = "Enrollment", 
         color = "FRPL %") +
    theme_bw() +
    theme(text = element_text(family = "Avenir", size = 12),
          plot.caption = element_text(hjust = 0, size = 10))
  
  ggsave("slides/week_08/de_example/figures/plot3_pp_diff.png", units = "in", 
         height = 5, width = 8)
  
  
  
  