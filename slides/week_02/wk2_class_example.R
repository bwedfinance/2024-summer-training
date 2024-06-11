# class_2_example 
# Last updated by Krista Kaput, 2024-06-03

# Load ----------

# When we are printing out numbers that it isn't using scientific notation 
options(scipen = 999)

library(tidyverse)
library(edbuildr)
library(scales)

# Krista likes to denote which spreadsheet is raw for transparency 
dist_fy19_raw <- masterpull(data_type = "geo")

# Filter Minnesota data and clean -----

# filter, rename, and mutate data for Minnesota 
mn_ed_data <- dist_fy19_raw |>
  # filter function is used to choose all rows (districts) for Minnesota 
  filter(State == "Minnesota") |>
  # rename function is used to rename variables. The renamed variable is on the left
  # and the variable we are renaming is on the right 
  rename(district = NAME,
         county = County,
         enroll = ENROLL, 
         total_local_rev = LR,
         total_state_rev = SR,
         total_state_local_rev = SLR, 
         urbanicity = dUrbanicity,
         operational_schools = dOperational_schools, 
         district_type = dType, 
         white_enroll = dWhite, 
         sped_enroll = dIEP, 
         ell_enroll = dLEP, 
         econ_dis_enroll = StPov,
         bipoc_pct = pctNonwhite, 
         pov_pct = StPovRate,
         median_house_income = MHI, 
         median_prop_value = MPV) |>
  # The mutate functions creates new variables 
  mutate(bipoc_enroll = enroll - white_enroll,
         ell_pct = ell_enroll/enroll, 
         sped_pct = sped_enroll/enroll, 
         local_rev_pp = total_local_rev/enroll,
         state_rev_pp = total_state_rev/enroll,
         local_state_rev_pp = total_state_local_rev/enroll) |>
  # The select function chooses the columns that will be in our new dataframe 
  select(district, county, enroll, local_rev_pp, state_rev_pp, local_state_rev_pp, 
         total_local_rev, total_state_rev, total_state_local_rev, urbanicity, 
         operational_schools, district_type, pov_pct, bipoc_pct, ell_pct, sped_pct)


# Step #1: Create a basic scatter plot for Minnesota ----------
ggplot(mn_ed_data, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point()

# Step #2: Address the missing Minnesota values ----------

mn_na_dist <- mn_ed_data |>
  # The "filter" function is used to select rows. the "is.na" is telling R to choose the following rows that have an NA. 
  filter(is.na(enroll)) 

# The data frame will automatically open in a new tab 
view(mn_na_dist)

# The missing data makes sense. All eight of the rows don't have any available data.

# Step #3: Clean up formatting of the scatter plot elements -------------

# We should also check to see the districts that have no enrollment 

mn_no_enroll <- mn_ed_data |> 
  filter(enroll == 0)


# Drop the districts that have no enrollment or their enrollment is NA 
mn_ed_clean <- mn_ed_data |>
  filter(enroll > 0)

# first Minnesota plot w/ clean data
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point()

# We see some overlap in the points. Reducing the opacity of the points can be 
# accomplished by setting the `alpha` parameter in geom_point() to a value less than 1. 
# Setting it to .5 will make data points 50% translucent.

ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point(alpha = .5)


# Let's take care of some formatting issues. Our axes don't look great - the decimals 
# ought to be percentages and the vertical axis represents dollars. Here, the `scales` package provides some help.

# format axes 
# install.packages("scales")
library(scales)

# format the x and y axes
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point(alpha = .5) + 
  # make sure you have the `scales` package loaded!
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar())

# Next, we should add some labels to our axes that make sense, along with a title for 
# our plot and a caption that details our data sources.

# add data labels 
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point(alpha = .5) + 
  # make sure you have the `scales` package loaded!
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Poverty Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019") 

# Themes can be used to change the appearance of elements in your plot. 
# There are many stock options, but I prefer `theme_bw()` for its clean appearance and helpful and unobtrusive grid lines.

# change theme
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Poverty Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019") +
  theme_bw()


#  Step #4: Add a new layer of data ----------

# Now that we have a decent-looking graph, let's add in a new data element to vary point size by enrollment.
# add size element 
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019") +
  theme_bw()

# Step #5: Tidy up the formatting -----------------

# Adding a new variable for size creates a legend. We need to tidy the legend's labels and the title.

# clean up the legend
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  # change legend label formatting
  scale_size(labels = comma) +
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       # add nice label for size element
       size = "Enrollment") +
  theme_bw()

# We can also adjust some paramenters to allow for more visual contrast in size. 
# By default, `ggplot2` will adjust points' radii based on the size variable. 
# Using area is a more visually honest way to represent the data, so let's make that change.

# create more contrast in size
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) + 
  # change size scaling to vary by area, not radius + change max size
  scale_size_area(labels = label_comma(), max_size = 10) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       # add nice label for size element
       size = "Enrollment") +
  theme_bw()

# Step #6: Repeat steps 4-5 as needed -------------------------

# add in color based on urbanicity
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  scale_size_area(labels = comma, max_size = 10) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment") +
  theme_bw()

# That doesn't look great. Let's use some more functions from the `tidyverse` 
# to clean up the data a little more to reduce 11 urbanicity levels to four, then tidy up the legend label.

# The `as.factor()` function is used to convert a vector or a column in a data frame into a factor. 
# Factors are used to represent categorical data, where the possible values of the variable are limited and known in advance.

# The `fct_collapse` function is used to collapse or group levels of a factor into new levels

# clean up the unicorn vomit
mn_ed_clean <- mn_ed_clean |>
  mutate(urbanicity = fct_collapse(as.factor(urbanicity),
                                   City = c("11-City: Large", 
                                            "12-City: Mid-size",
                                            "13-City: Small"),
                                   Suburb = c("21-Suburb: Large",
                                              "22-Suburb: Mid-size",
                                              "23-Suburb: Small"),
                                   Town = c("31-Town: Fringe",
                                            "32-Town: Distant",
                                            "33-Town: Remote"),
                                   Rural = c("41-Rural: Fringe",
                                             "42-Rural: Distant",
                                             "43-Rural: Remote")))

# We can now use urbanicity variable and it will include only 4 categories: City, Suburb, Town, and Rural
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_size_area(labels = label_comma(), max_size = 10) +  
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment") +
  theme_bw()



# We can and should adjust the colors used. R recognizes some pretty funky color 
# names, which can be found in this helpful cheat sheet.

# adjust colors manually
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  # create manual color palette
  # color names pulled from a pdf y'all should bookmark
  # http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
  scale_color_manual(values = c("tomato3", "steelblue2",
                                "seagreen3", "orchid1")) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_size_area(labels = label_comma(), max_size = 10) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment") +
  theme_bw()

# We should strive to make our analyses as accessible as possible. 
# The `viridis` package includes some color palettes that are friendly for folks 
# with color blindness, which affects 5-10 percent of the US population.

# use colors better for visual impairments
# install.packages("viridis")
library(viridis)

# adjust colors manually
ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  # use a colorblind-friendly palette
  scale_color_viridis_d() +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_size_area(labels = label_comma(), max_size = 10) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment") +
  theme_bw()

# Let's adjust the range of colors used to exclude that hard-to-see yellow.
# that yellow is hard to see - let's adjust the range

ggplot(mn_ed_clean, aes(x = pov_pct, y = local_rev_pp, size = enroll,
                        color = urbanicity)) +
  geom_point(alpha = .5) + 
  # adjust color range
  scale_color_viridis_d(end = .8) +
  scale_x_continuous(labels = label_percent()) +
  scale_y_continuous(labels = label_dollar()) +
  scale_size_area(labels = label_comma(), max_size = 10) + 
  labs(x = "Student Poverty Rate", y = "Local Per-Pupil Revenue",
       title = "Local Per-Pupil Revenue by Student Povert Rate in Minnesota School Districts",
       caption = "Source: Edbuild Data, 2019",
       size = "Enrollment", color = "Urbanicity") +
  theme_bw()

# Summarize  ------------

# create a state summary data frame
state_summary <- dist_fy19_raw |> 
  group_by(State) |> 
  summarise(enroll_med = median(ENROLL, na.rm = T),
            n_schools_med = median(dOperational_schools, na.rm = T),
            nonwhite_pct_med = median(pctNonwhite, na.rm = T),
            st_pov_rate_med = median(StPovRate, na.rm = T),
            sd_area_med = median(sd_area, na.rm = T),
            student_per_sq_mile_med = median(student_per_sq_mile, na.rm = T),
            mhi_med = median(MHI, na.rm = T),
            mpv_med = median(MPV, na.rm = T),
            state_loc_rev_pp_med = median(SLRPP, na.rm = T))

# Create histograms -----

# histogram of median property value

ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = mpv_med), binwidth = 40000)

# histogram of median household income
ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = mhi_med), binwidth = 3000)

# histogram of state + local pp rev
ggplot(data = state_summary) +
  geom_histogram(mapping = aes(x = state_loc_rev_pp_med), binwidth = 2000)
