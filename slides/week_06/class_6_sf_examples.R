# class_6_sf_example 
# last updated 2024-07-11 by Krista Kaput 


# load ------
options(scipen = 999)

library(tidyverse)
library(edbuildr)
library(tidycensus)
library(viridis)
library(scales)
library(sf)


# Step #1:  Load in the Minnesota schools data 

mn_schools <- read_csv(here::here("slides/week_06/data/mn_schools_clean.csv"))

# #2: Convert to sf object for mapping ---------
mn_schools_shp <- st_as_sf(mn_schools,
                           coords = c("long", "lat"),
                           crs = st_crs("EPSG:4326"))
# check ----------

# Check the projection of your objects using the st_crs() function 

# QUESTION: Why does this dataframe not have a coordinate system? 
st_crs(mn_schools)

st_crs(mn_schools_shp)

# plot ---------------

# simple plot of sf data
ggplot(mn_schools_shp) +
  geom_sf() +
  theme_void()

# get state outline from tigris ----------
library(tigris)

mn_outline <- states(cb = TRUE) |> 
  filter(NAME == "Minnesota")

# plotting multiple geom_sf layers ---------
ggplot() +
  geom_sf(data = mn_outline, color = "black") +
  geom_sf(data = mn_schools_shp, 
          # we can use aes() to map data just like with geom_point()
          aes(size = total_enroll,
              color = bipoc_pct),
          alpha = .5) +
  scale_color_viridis(labels = percent_format()) +
  scale_size_area(max_size = 5,
                  labels = comma_format()) +
  labs(color = "Percent BIPOC", 
       size = "Enrollment",
       title = "Minnesota School Size, Location, and Demographics") +
  theme_void()









