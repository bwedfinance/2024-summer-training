# urban_ccd_clean.R
# last updated by Krista Kaput on 2023-08-31

# load --------
library(tidyverse)
library(edbuildr)
library(sf)
library(viridis)
library(scales)
library(sf)

#Read the Json file
ms_geojson_raw <- read_sf("data/processed/districtMapMS.geojson")

sd_map_raw <- edbuildmapr::sd_shapepull(data_year = "2019", with_data = TRUE)

# Pull out the MS mapping data 

ms_shp <- sd_map_raw |> 
  # tidy up colnames
  rename_with(tolower) |> 
  filter(state == "Mississippi") |>
  rename(ncesid = geoid) |>
  separate(state_id, c('state', 'dist_id')) |>
  select(ncesid, name, dist_id, geometry)

# Clean the MS mapping geojson 

ms_geojson <- ms_geojson_raw |>
  rename_with(tolower) |> 
  select(geoid, geometry)


# export map-----

st_write(ms_shp, "data/processed/ms_shp.shp")

# Write it as shape file
st_write(ms_geojson, "data/processed/ms_shapefile.shp")