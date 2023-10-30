###
# Cyclistic bike-share analysis case study!
# Author: Linda Perez Jaracuaro
# Date: October 18th 2023
###

# 1. Ask ------------------------------------------------------------------

# Exploring User Behavior: Annual Members vs. Casual Riders -
# A Comparative Analysis of Cyclistic Bike Trips to discover how annual members
# and casuals use Cyclistic bikes differently

# 2. Prepare -----------------------------------------------------------------

# Data Source: Ride data

# Type of Data: Time-series data
# Format: CSV file
# Collection Period: August 2022 to July 2023
# Description: This data source provides a comprehensive daily history of how
# different user types utilize our bicycle rental services. It includes detailed
# information on ride dates, bicycle types used, and the geographical coordinates
# of both the starting and ending points of each ride. The data was meticulously
# recorded and collected from our internal system, which serves as a valuable
# resource for understanding user preferences.


# Include libraries
library(DescTools)
library("geosphere")
library(hydroTSM)
library(lubridate)
library(scales)
library(tidyverse)

# Unify in one data frame called df, data cvs available from one year
files <- list.files(path = "./Data files",
                    pattern = ".*-divvy-tripdata\\.csv$",
                    full.names = TRUE)
df_list <- lapply(files, read_csv)
df <- bind_rows(df_list)

# Remove unnecessary variables
rm(files, df_list)


# 3. Process --------------------------------------------------------------

#### Integrity data check -------------------------------------------------
# Create a data frame that contains all observations with one or more missing values
missing_values <- df %>%
  filter_all(any_vars(is.na(.)))
# Calculate the number of missing values in each column of missing_values
missing_values_count <- colSums(is.na(missing_values))
# Identify columns with missing values
columns_with_missing_values <- names(missing_values_count[missing_values_count > 0])
# Find percentage of missing data from df
missing_percent <- nrow(missing_values) / nrow(df)

# After an integrity data check it was found that a total of 2.4% observations 
# contains missing values for start_station_id, start_station_name, 
#end_station_id, end_station_name, end_lat and end_lng

# Each variable will undergo an individualized analysis to unveil its specific 
# relationship with 'member_casual.' during Analysis phase. Consequently, this 
# analysis will guide the decision of whether to include or exclude data with 
# missing values for that variable. Our overarching goal is to comprehensively 
# analyze all available data without compromising the integrity of the research.

#### day_of_week -----------------------------------------------

# Calculate day_of_week
df <- df %>%
  mutate(
    day_of_week = wday(started_at)
  )

#### distance --------------------------------------------------

# Create distance variable from latitude and longitude values 
# distance is measured by meters
# distance will not consider 0 values for computing

df <- df %>%
  mutate(distance = distVincentySphere(
    cbind(start_lng, start_lat),
    cbind(end_lng, end_lat)
  )
  ) %>%
  filter(distance > 0)
#### month -----------------------------------------------------

# Calculate month
df <- df %>%
  mutate(month = month(started_at,  label = TRUE, abbr = TRUE))


#### ride_length (time) ----------------------------------------
# Calculate time used
# Convert variable into numeric type for computing and store it as minutes
df <- df %>%
  mutate(ride_length = as.numeric(ended_at - started_at) /60) 

# Look to the data
min(df$ride_length)
max(df$ride_length)

# Ride length goes from -10353.35 mins to 51461.4 mins, in other words 35.73 days
# These outlines are potential error of data entry
# Remove rides shorter than one minute
df <- df %>%
  filter(ride_length >= 1 )
#### seasons ---------------------------------------------------

# Calculate season
df <- df %>%
  mutate(season = time2season(started_at, out.fmt = "seasons"))

#### time_of_day -----------------------------------------------

# Calculate day time of the day 
# create breaks
day_timing_breaks <- hour(hm("00:00", "6:00", "12:00", "18:00", "23:59"))
# labels for the breaks
day_timing_lst <- c("Night", "Morning", "Afternoon", "Evening")

df$time_of_day <- cut(x=hour(df$started_at), 
                      breaks = day_timing_breaks, 
                      labels = day_timing_lst, 
                      include.lowest=TRUE)





# 4. Analyze --------------------------------------------------------------

# Generate comprehensive variable summaries to gain deeper insights into their 
# relationships with the members in your data analysis.

#### day_of_week ----------------------------------------------------------
# Summary of trips by day of week
weekdays_lst <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat", "Sun")
sum_day_of_week <- df %>%
  group_by(member_casual, day_of_week) %>%
  summarize(num_trips = n()) %>%
  mutate( day_of_week = weekdays_lst[day_of_week],
          total = sum(num_trips),
          perc = round( ((num_trips / total) * 100), 2 )) %>%
  select(member_casual, day_of_week, perc, num_trips)
sum_day_of_week$day_of_week <- factor(sum_day_of_week$day_of_week, levels = weekdays_lst)
sum_day_of_week
#### distance -------------------------------------------------------------
# Summary of trips by distance measured in meters
sum_distance <- df %>%
  filter(distance > 0) %>%
  group_by(member_casual) %>%
  summarize(mean = mean(distance, na.rm = TRUE),
            median = median(distance, na.rm = TRUE),
            mode = Mode(distance, na.rm = TRUE))
sum_distance
#### member_casual -------------------------------------------------------
# Compute cyclist type percentage
sum_member <- df %>%
  group_by(member_casual) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count))
#### month ----------------------------------------------------------------
# Summary of trips by month
sum_months <- df %>%
  group_by(member_casual, month) %>%
  summarize(num_trips = n()) %>%
  mutate( total = sum(num_trips),
          perc = round( ((num_trips / total) * 100), 2 )) %>%
  select(member_casual, month, perc, num_trips)
#### ride_length (time)----------------------------------------------------
# Summary of trips by ride length, in other words ride time
sum_time <- df %>%
  group_by(member_casual) %>%
  summarize(mean = mean(ride_length, na.rm = TRUE),
            median = median(ride_length, na.rm = TRUE),
            mode = Mode(ride_length, na.rm = TRUE))
#### rideable_bike -------------------------------------------------------
# Summary of bike types used by each type of member
sum_bike <- df %>%
  group_by(member_casual, rideable_type) %>%
  summarize(count = n()) %>%
  mutate(perc = count / sum(count),
         rideable_type = sub("_.*", "", rideable_type))

#### seasons --------------------------------------------------------------
# Summary of trips by season
# Season list, it will be used for factor
season_lst <- c('spring', 'summer', 'autumm', 'winter')
sum_season <- df %>%
  group_by(member_casual, season) %>%
  summarize(num_trips = n()) %>%
  mutate(total = sum(num_trips),
         perc = round( ((num_trips / total) * 100), 2 )) %>%
  select(member_casual, season, num_trips, perc)

sum_season$season <- factor(sum_season$season, levels = season_lst)
sum_season
#### time_of_day ----------------------------------------------------------
# Summary of time of the day
sum_day_timing <- df %>%
  group_by(member_casual, time_of_day) %>%
  summarize(num_trips = n()) %>%
  mutate( total = sum(num_trips),
          perc = round( ((num_trips / total) * 100), 2 )) %>%
  select(member_casual, time_of_day, perc, num_trips)

#### top_stations ---------------------------------------------------------
# Rank of most common start station for each type of member

# For start stations
# For casual
top_start_station_casual <- df %>%
  filter(!is.na(start_station_id) & member_casual == 'casual') %>%
  mutate(id = start_station_id,
         name = start_station_name) %>%
  group_by(id, name) %>%
  summarize(member_casual = "casual",
            type = "start",
            count = n())
top_start_station_casual$rank <- min_rank( desc(top_start_station_casual$count) )
top_start_station_casual <- top_start_station_casual %>%
  filter(rank <= 10) %>%
  arrange(rank)
# For member
top_start_station_member <- df %>%
  filter(!is.na(start_station_id) & member_casual == 'member') %>%
  mutate(id = start_station_id,
         name = start_station_name) %>%
  group_by(id, name) %>%
  summarize(member_casual = "member",
            type = "start",
            count = n())
top_start_station_member$rank <- min_rank( desc(top_start_station_member$count) )
top_start_station_member <- top_start_station_member %>%
  filter(rank <= 10) %>%
  arrange(rank)

# For casual
top_end_station_casual <- df %>%
  filter(!is.na(end_station_id) & member_casual == 'casual') %>%
  mutate(id = end_station_id,
         name = end_station_name) %>%
  group_by(id, name) %>%
  summarize(member_casual = "casual",
            type = "end",
            count = n())
top_end_station_casual$rank <- min_rank( desc(top_end_station_casual$count) )
top_end_station_casual <- top_end_station_casual %>%
  filter(rank <= 10) %>%
  arrange(rank)
# For member
top_end_station_member <- df %>%
  filter(!is.na(end_station_id) & member_casual == 'member') %>%
  mutate(id = end_station_id,
         name = end_station_name) %>%
  group_by(id, name) %>%
  summarize(member_casual = "member",
            type = "end",
            count = n())
top_end_station_member$rank <- min_rank( desc(top_end_station_member$count) )
top_end_station_member <- top_end_station_member %>%
  filter(rank <= 10) %>%
  arrange(rank)


# Summary top stations both start and end
sum_top_stations <-rbind(top_start_station_casual,
                         top_start_station_member,
                         top_end_station_casual,
                         top_end_station_member)


#Remove variables
rm(top_start_station_casual,
    top_start_station_member,
    top_end_station_casual,
    top_end_station_member)

#### stations_location -------------------------------------------------------

top_station_id <- sum_top_stations$id %>%
  unique()

start_stations_location <- df %>%
  mutate(id = start_station_id,
         name = start_station_name) %>%
  group_by(id, name) %>%
  summarize(latitude = mean(start_lat),
            longitude = mean(start_lng))

end_stations_location <- df %>%
  mutate(id = end_station_id,
         name = end_station_name) %>%
  group_by(id, name) %>%
  summarize(latitude = mean(end_lat),
            longitude = mean(end_lng))

stations_location <- rbind(start_stations_location,
                           end_stations_location) %>%
  group_by(id, name) %>%
  filter(id %in% top_station_id) %>%
  summarize(latitude = mean(latitude),
            longitude = mean(longitude))

# Merge both stations df
sum_top_stations <- sum_top_stations %>%
  left_join(stations_location) %>%
  mutate(city = "Chicago")

top_station_member <- sum_top_stations %>%
  filter(id %in% top_station_id & member_casual == "member") %>%
  select(-type) %>%
  group_by(id)
top_station_casual <- sum_top_stations %>%
  filter(id %in% top_station_id & member_casual == "casual") %>%
  select(-type) %>%
  group_by(id)

write.csv(top_station_member, "Top stations - member.csv", row.names=TRUE)
write.csv(top_station_casual, "Top stations - casual.csv", row.names=TRUE)

# Export sum_top_stations to csv format for latter analysis in Tableau
write.csv(sum_top_stations, "Top stations.csv", row.names=TRUE)  

rm(start_stations_location, end_stations_location, stations_location)
# 5. Share ----------------------------------------------------------------

# Declare colors for cyclist type
colors_lst <- c("#3f94a3", "gray")

# Define two colors you to interpolate between and create a palette for other 
# groups, like monts, weekday, time_of_day
color_start <- "#d6eaee"
color_end <- "#3f94a3"

# Open a pdf file
pdf("Media files\\Cyclistic_models.pdf")

#### day_of_week ------------------------------------------------

# Summary day_of_week
# Option 1: Line graph - Trips percentage
ggplot() + 
  geom_col(data = sum_day_of_week,
           mapping = aes(x = member_casual,
                         y = perc,
                         fill = day_of_week),
           show.legend = TRUE,
           color = "white") +
  scale_fill_manual(values = c( "gray", "gray","gray", "gray","gray", "#3f94a3","#3f94a3")) + 
  labs(title = "Weekday breakdown",
       subtitle = "Past 12 months",
       y = "Rides  (%)",
       x = "",
       fill = "") +
  theme_classic()

# Option 1: Line graph - Trips total
ggplot(data = sum_day_of_week,
       mapping = aes(x = day_of_week,
                     y = num_trips,
                     group = member_casual,
                     color = member_casual)) + 
  geom_line(show.legend = TRUE) + 
  geom_point() +
  scale_color_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  labs(title = "Weekday breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       color = "Cyclist  type") +
  theme_classic()

# Option 1: Line graph - Trips total
ggplot(data = sum_day_of_week,
       mapping = aes(x = day_of_week,
                     y = num_trips,
                     group = member_casual,
                     fill = member_casual)) + 
  geom_col(position = "dodge",
           width = 0.7,
           show.legend = TRUE) +
  coord_flip()+
  scale_fill_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  labs(title = "Weekday breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

#### distance ---------------------------------------------------

# Mean distance by each type of member
ggplot(data = sum_distance) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = mean,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 2500)) +
  scale_fill_manual(values = colors_lst) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) + 
  labs(title = "Ride distance - mean",
       subtitle = "Past 12 months",
       y = "kilometers",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

# Median distance by each type of member
ggplot(data = sum_distance) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = median,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 2500)) +
  scale_fill_manual(values = colors_lst) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) + 
  labs(title = "Ride distance - median",
       subtitle = "Past 12 months",
       y = "kilometers",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

# Mode distance by each type of member
ggplot(data = sum_distance) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = mode,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 2500)) +
  scale_fill_manual(values = colors_lst) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) + 
  labs(title = "Ride distance - mode",
       subtitle = "Past 12 months",
       y = "kilometers",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

#### member_casual ----------------------------------------------
# Plot member type difference
ggplot() + 
  geom_col(data = sum_member, 
           mapping = aes(x = member_casual,
                         y = count,
                         fill = member_casual), 
           show.legend = FALSE,
           width = 0.7) + 
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-6)) +
  scale_fill_manual(values = colors_lst) +
  coord_flip() +
  labs(title = "Rides by customer",
       subtitle = "Past 12 months",
       y = "Rides (millions)",
       x = "") +
  theme_classic()

#### month ------------------------------------------------------

# Define color palette to use
months_palette <- colorRampPalette(c(color_start, color_end))(12)

# Option 1: Bar chart - Trips percentage
ggplot() + 
  geom_col(data = sum_months,
           mapping = aes(x = member_casual,
                         y = perc,
                         fill = month),
           show.legend = TRUE,
           color = "white") +
  scale_fill_manual(values = months_palette) +
  labs(title = "Month breakdown",
       subtitle = "Past 12 months",
       y = "Rides  (%)",
       x = "",
       fill =  "") +
  theme_classic()

# Option 2: Line graph - Trips total
ggplot(data = sum_months,
       mapping = aes(x = month,
                     y = num_trips,
                     group = member_casual,
                     color = member_casual)) + 
  geom_line(show.legend = TRUE) + 
  geom_point() +
  scale_color_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  labs(title = "Month breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       color = "Cyclist  type") +
  theme_classic()

# Option 3: Horizontal bar chart - Trips total
ggplot(data = sum_months,
       mapping = aes(x = month,
                     y = num_trips,
                     fill = member_casual
                     )) + 
  geom_col(  width = 0.7,
             position = "dodge",
             show.legend = TRUE) + 
  scale_fill_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  coord_flip() +
  labs(title = "Month breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

#### ride_length (time) -----------------------------------------

# Normal distribution
# Plot ride length for each type of user (time)
ggplot() + 
  geom_line(
    data = df,
    mapping = aes(x = ride_length,
                  group = member_casual,
                  color = member_casual), 
    stat = "count",
    show.legend = TRUE) + 
  scale_color_manual(values = colors_lst)+
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  coord_cartesian(xlim = c(1, 60)) +  
  labs(title = "Ride length",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "minutes",
       color = "Cyclist  type") +
  theme_classic()

# Mean time by each type of member
ggplot(data = sum_time) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = mean,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 20)) +
  scale_fill_manual(values = colors_lst) +
  labs(title = "Ride length - mean",
       subtitle = "Past 12 months",
       y = "minutes",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

# Median time by each type of member
ggplot(data = sum_time) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = median,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 20)) +
  scale_fill_manual(values = colors_lst) +
  labs(title = "Ride length - median",
       subtitle = "Past 12 months",
       y = "minutes",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()

# Common ride length time by each type of member
ggplot(data = sum_time) + 
  geom_col(
    mapping = aes(x = member_casual,
                  y = mode,
                  fill = member_casual),
    show.legend = FALSE,
    width = 0.7) +
  coord_flip(ylim = c(0, 20)) +
  scale_fill_manual(values = colors_lst) +
  labs(title = "Ride length - mode",
       subtitle = "Past 12 months",
       y = "minutes",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()


#### rideable_bike ------------------------------------------------------
# Define color palette to use
rideable_bike_palette <- colorRampPalette(c(color_start, color_end))(3)

# Plot rideable_bike type for each type of member
ggplot() + 
  geom_col(data = sum_bike, 
           mapping = aes(x = rideable_type,
                         y = count,
                         fill = member_casual),
           position = "dodge",
           show.legend = FALSE,
           width = 0.7) + 
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  scale_fill_manual(values = colors_lst) +
  coord_flip() +
  labs(title = "Favorite bikes overview",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "") +
  theme_classic()

# Favorite bikes breakdown by seasons
ggplot() + 
  geom_col(data = sum_bike,
           mapping = aes(x = member_casual,
                         y = perc,
                         fill = rideable_type),
           color = "white",
           show.legend = TRUE) + 
  scale_fill_manual(values = rideable_bike_palette)+
  labs(title = "Favorite bikes overview",
       subtitle = "Past 12 months",
       y = "Rides (%)",
       x = "",
       fill = "Type")+
  theme_classic()


#### seasons ----------------------------------------------------

# Define color palette to use
seasons_palette <- colorRampPalette(c(color_start, color_end))(4)

# Plot seasons
ggplot(data = sum_season,
       mapping = aes(x = season,
                     y = num_trips,
                     group = member_casual,
                     color = member_casual), 
       show.legend = TRUE) + 
  geom_line() + 
  geom_point() +
  scale_color_manual(values = colors_lst) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) + 
  labs(title = "Seasons breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       color = "Cyclist  type")+
  theme_classic()

# Plot seasons
ggplot(data = sum_season,
       mapping = aes(x = season,
                     y = num_trips,
                     group = member_casual,
                     fill = member_casual), 
       show.legend = TRUE) + 
  geom_col(position = "dodge") + 
  coord_flip() +
  scale_fill_manual(values = colors_lst) +
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) + 
  labs(title = "Seasons breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       fill = "Cyclist  type")+
  theme_classic()


# Ride breakdown by seasons proportion
ggplot() + 
  geom_col(data = sum_season,
            mapping = aes(x = member_casual,
                          y = perc,
                          fill = season),
           color = "white",
           show.legend = TRUE) + 
  scale_fill_manual(values = seasons_palette) +
  labs(title = "Seasons breakdown",
       subtitle = "Past 12 months",
       y = "Rides (%)",
       x = "",
       fill =  "")+
  theme_classic()

#### time_of_day ------------------------------------------------------

# Define color palette to use
time_of_day_palette <- colorRampPalette(c(color_start, color_end))(4)

# Summary time_of_day
# Option 1: Line graph - Trips percentage
ggplot() + 
  geom_col(data = sum_day_timing,
           mapping = aes(x = member_casual,
                         y = perc,
                         fill = time_of_day),
           show.legend = TRUE,
           color = "white") +
  scale_fill_manual(values = time_of_day_palette)+
  labs(title = "Time of day breakdown",
       subtitle = "Past 12 months",
       y = "Rides  (%)",
       x = "",
       fill = "") +
  theme_classic()
# Option 1: Line graph - Trips total
ggplot(data = sum_day_timing,
       mapping = aes(x = time_of_day,
                     y = num_trips,
                     group = member_casual,
                     color = member_casual)) + 
  geom_line(show.legend = TRUE) + 
  geom_point() +
  scale_color_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  labs(title = "Time of day breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       color = "Cyclist  type") +
  theme_classic()

# Option 1: Line graph - Trips total
ggplot(data = sum_day_timing,
       mapping = aes(x = time_of_day,
                     y = num_trips,
                     fill = member_casual
       )) + 
  geom_col(  width = 0.7,
             position = "dodge",
             show.legend = TRUE) + 
  scale_fill_manual(values = colors_lst) +  
  scale_y_continuous(labels = unit_format(unit = "", scale = 1e-3)) +
  coord_flip() +
  labs(title = "Time of day breakdown",
       subtitle = "Past 12 months",
       y = "Rides (thousands)",
       x = "",
       fill = "Cyclist  type") +
  theme_classic()


# Close the pdf file
dev.off()















