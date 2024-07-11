# class_6_leaflet_example 
# last updated 2024-07-11 by Krista Kaput 

# load -------------

library(tidyverse)
library(edbuildr)
library(edbuildmapr)
library(viridis)
library(scales)
library(sf)
library(leaflet)

# load in the school district mapping data
sd_map_raw <- sd_shapepull(data_year = "2019", with_data = TRUE)

dist_data_raw <- masterpull(data_type = "geo")

# Step #1: clean ----------------

# filter mapping data for your state
state_shp_raw <- sd_map_raw |> 
  # tidy up colnames
  rename_with(tolower) |> 
  filter(state == "Kentucky") |>
  rename(ncesid = geoid) |> 
  rename_with(tolower) |> 
  select(ncesid, geometry)

# filter finance data for your state
state_data <- dist_data_raw |> 
  rename_with(tolower) |> 
  filter(state == "Kentucky")

# join data
state_shp <- state_shp_raw |> 
  left_join(state_data, by = "ncesid") |> 
  mutate(pct_sr = srpp / slrpp) |> 
  st_transform(st_crs("EPSG:4326"))


# Step 2: Create leaflet map with a base layer -----
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") 


# Step #3: add district shapes -------
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(data = state_shp)


# Step #4: define the color and thickness of borders ------
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(data = state_shp,
              color = "#ababab",
              weight = .5)

# Step #5: add a popup layer --------

leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(data = state_shp,
              color = "#ababab",
              weight = .5,
              popup = paste0("District: ",
                             str_to_title(state_shp$name), 
                             "<br> Enrollment: ", 
                             comma(state_shp$enroll),
                             "<br> Percent state revenue: ",
                             percent(state_shp$pct_sr, accuracy = .1)))


# Step 6: Create a more advanced map with dynamic fill and a legend ------
# advanced leaflet example ------------------

bw_primary <- c("#6D1E4A", # 1 plum
                "#007786", # 2 teal
                "#0D525A", # 3 dark green
                "#212B46", # 4 navy
                "#5A6675", # 5 grey
                "#F0DEC1") # 6 cream

bw_secondary <- c("#FFC762", # 1 yellow
                  "#FFB653", # 2 orange
                  "#BEC6CE", # 3 light grey
                  "#2E1A4A", # 4 deep purple
                  "#7EA2D1", # 5 soft blue
                  "#CAD3FB", # 6 lavender
                  "#9CD4EA", # 7 sky
                  "#FFA497") # 8 peach

# define breaks for fill variable
state_rev_breaks <- c(0, .4, .6, .7, .8, 1)

# define custom color palette
bw_scale <- c(bw_primary[6],
              bw_secondary[1],
              bw_primary[2],
              bw_primary[3],
              bw_primary[4])

# create color palette object
state_rev_palette <- colorBin(palette = bw_scale,
                              domain = state_shp$pct_sr,
                              bins = state_rev_breaks,
                              na.color = bw_primary[5]) 

# create advanced leaflet map
leaflet() |>
  addProviderTiles(provider = "CartoDB.Positron") |>
  addPolygons(data = state_shp,
              color = "#ababab",
              weight = .5,
              popup = paste0("District: ",
                             str_to_title(state_shp$name), 
                             "<br> Enrollment: ", 
                             comma(state_shp$enroll),
                             "<br> Percent state revenue: ",
                             percent(state_shp$pct_sr, accuracy = .1)),
              fillColor = ~ state_rev_palette(pct_sr),
              fillOpacity = .8) |> 
  addLegend("topright", 
            opacity = .8,
            pal = state_rev_palette,
            values = state_shp$pct_sr,
            labFormat = labelFormat(
              suffix = "%", between = " - ",
              transform = function(x) 100 * x
            ),
            title = "Percent state revenue")





