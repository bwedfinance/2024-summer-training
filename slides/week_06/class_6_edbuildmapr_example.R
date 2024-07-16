# class_6_edbuildmapr_example 
# last updated 2024-07-11 by Krista Kaput 


# Step #1: load ------
options(scipen = 999)

library(edbuildr)
library(edbuildmapr)
library(viridis)
library(scales)

# load in the school district mapping data
sd_map_raw <- sd_shapepull(data_year = "2019", with_data = TRUE)

# load in raw district finance data
dist_data_raw <- masterpull(data_type = "geo")


# Step #2: filter mapping data for your state -------
state_shp_raw <- sd_map_raw |> 
  # tidy up colnames
  rename_with(tolower) |> 
  filter(state == "Texas") |>
  rename(ncesid = geoid) |> 
  rename_with(tolower) |> 
  select(ncesid, geometry)

# filter state finance data for your state
state_data <- dist_data_raw |> 
  rename_with(tolower) |> 
  filter(state == "Texas")

# join data
state_shp <- state_shp_raw |> 
  left_join(state_data, by = "ncesid") |> 
  mutate(pct_sr = srpp / slrpp)

# Step #3: plot a basic map ----
ggplot()  + 
  geom_sf(data = state_shp, aes(fill = pct_sr)) +
  theme_void() 

# Step #4: Make several aesthetic changes ------
ggplot() + 
  geom_sf(data = state_shp, 
          aes(fill = pct_sr),
          color = "#ffffff") +
  theme_void() +
  scale_fill_viridis(name = "Percent K-12 Budget\nfrom State Revenue (%)",
                     labels = percent_format(accuracy = 1), 
                     direction = -1) +
  labs(
    title = "School Districts' Reliance on State Revenue",
    subtitle = "Percent of District K-12 Revenue From State, 2018-19",
    caption = "Source: EdBuild")


# Step #5: Make your own color palette and create bins to more clearly break up your data!

# create custom color palette
bw_state_revenue <- c("#BEC6CE","#FFC762", "#007786", "#212B46", "#6D1E4A")

# plot another nicely-formatted map
ggplot()  + 
  geom_sf(data = state_shp,
          aes(fill = pct_sr),
          color = "#ffffff") +
  theme_void() +
  scale_fill_stepsn(breaks=c(0, .3, .5, .7, 1), # Breaks up the data
                    colors = bw_state_revenue, 
                    name="State K-12 Revenue (%)",
                    labels=percent_format(accuracy = 1L)) + 
  labs(
    title = "School Districts' Reliance on State Revenue",
    subtitle = "Percent of District K-12 Revenue From State (2019)",
    caption = "Source: EdBuildr Data, 2019")





