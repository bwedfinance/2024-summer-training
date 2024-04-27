# sf_mapping_example
# 2023-07-18, Last updated by Krista Kaput

# Load -----
library(tidyverse)
library(edbuildmapr)
library(scales)
library(viridis)
library(devtools)
library(ggplot2)
library(sf)
library(edbuildr)
library(tmap)
library(tmaptools)
library(svglite)
library(leaflet)
library(tidycensus)
library(geojsonio)
library(rgdal)

# Let's load in sf! 
library(sf)

options(scipen = 999)

# Load in the Minnesota schools data 

mn_schools <- read_csv("slides/week_06/week_6_data/mn_schools_clean.csv")

# Convert to sf object for mapping! 

mn_schools_shp <- st_as_sf(mn_schools,
                           coords = c("long", "lat"),
                           crs = st_crs(4326))

# Check the projection of your objects using the st_crs() function 

# Why does this dataframe not have a coordinate system? 
st_crs(mn_schools)

st_crs(mn_schools_shp)


# Create a leaflet map with a baselayer 

leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") 


# Add Minnesota schools to the map and let the map define 
# the view by the extent of the point layer 

leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addCircleMarkers(data = mn_schools_shp)

# Define the size of the circles by enrollment 
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addCircleMarkers(data = mn_schools_shp,
                   radius = ~ total_enroll/300)

# Color the points by Title I school status 
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addCircleMarkers(data = mn_schools_shp,
                   radius = ~ total_enroll/300,
                   color = ~ case_when(title_1_school_status == "2-Title I targeted assistance school" ~ "#5ab4ac",
                                       title_1_school_status == "5-Title I schoolwide school" ~ "#3288bd",
                                       title_1_school_status == "6-Not a Title I school" ~ "#fc8d59",
                                       TRUE ~ "#e5e5e5"))



# Add a pop up to your leaflet ---
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addCircleMarkers(data = mn_schools_shp,
                   radius = ~ total_enroll/300,
                   popup = paste("School: ",
                                 mn_schools_shp$school, 
                                 "<br> Enrollment: ", 
                                 mn_schools_shp$total_enroll),
                   color = ~ case_when(title_1_school_status == "2-Title I targeted assistance school" ~ "#5ab4ac",
                                       title_1_school_status == "5-Title I schoolwide school" ~ "#3288bd",
                                       title_1_school_status == "6-Not a Title I school" ~ "#fc8d59",
                                       TRUE ~ "#e5e5e5"))


