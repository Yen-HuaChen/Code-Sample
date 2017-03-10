spark_home = "/data/spark/spark-2.0.1-bin-hadoop2.7/"
Sys.setenv(SPARK_HOME=spark_home)

library(sparklyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(magrittr)
sc = spark_connect(master = "local", version="2.0.1")

#########Task 1

# Read NYC Taxi data
green  = spark_read_csv(sc, "green", "/data/nyc-taxi-data/data/green_tripdata_2016-06.csv")
yellow = spark_read_csv(sc, "yellow", "/data/nyc-taxi-data/data/yellow_tripdata_2016-06.csv")
uber = spark_read_csv(sc, "uber", "/data/nyc-taxi-data/data/uber-raw-data-jun14.csv")


# add column names and convert the column names from capital letters into lower letters
fix_names = function(df)
{
  df %>%
    setNames(
      colnames(df) %>% 
        tolower() %>% 
        sub("[tl]pep_","",.) 
    )
}

# Set the latitudes and longitudes which depicts the shape of New York City
filter_location = function(df)
{
  df %>%
    # filter the points within NY boudaries
    filter(pickup_latitude > 40.477) %>%
    filter(pickup_latitude < 40.917) %>%
    filter(pickup_longitude > -74.259) %>%
    filter(pickup_longitude < -73.700) %>%
    filter(dropoff_latitude > 40.477) %>%
    filter(dropoff_latitude < 40.917) %>%
    filter(dropoff_longitude > -74.259) %>%
    filter(dropoff_longitude < -73.700) %>%
    mutate(pickup_lat = round(pickup_latitude, 3),
           pickup_long = round(pickup_longitude, 3),
           dropoff_lat = round(dropoff_latitude, 3),
           dropoff_long = round(dropoff_longitude, 3)
    ) %>%
    select(pickup_datetime, dropoff_datetime, trip_distance, fare_amount, tip_amount,
           pickup_lat, pickup_long, dropoff_lat, dropoff_long)
}

# filter function for uber is written separately because its has diffenrent col names
uber_filter = function(df) {
  df %>%
    filter(Lat > 40.477) %>%
    filter(Lat < 40.917) %>%
    filter(Lon > -74.259) %>%
    filter(Lon < -73.700) %>%
    mutate(
      lati = round(Lat, 3),
      longi = round(Lon, 3)
    ) %>%
    select(DateTime, lati, longi)
}

# Filter the data frame
green  = green  %>% fix_names() %>% filter_location()
yellow = yellow %>% fix_names() %>% filter_location()
uber = uber %>% uber_filter

# Count the number of pick ups and drop offs
pickup_location_count = function(df, label = "") {
  df %>%
    group_by(pickup_lat, pickup_long) %>%
    summarise(pickup_count = count()) %>%
    mutate(taxi = label) %>%
    collect()
}

dropoff_location_count = function(df, label = "") {
  df %>%
    group_by(dropoff_lat, dropoff_long) %>%
    summarise(dropoff_count = count()) %>%
    mutate(taxi = label) %>%
    collect()
}

# obtain data frames of uber
taxi_pickup = rbind(
  green  %>% pickup_location_count("green"),
  yellow %>% pickup_location_count("yellow")
)

taxi_dropoff = rbind(
  green  %>% dropoff_location_count("green"),
  yellow %>% dropoff_location_count("yellow")
)

# count the number of pickup for uber
uber_pickup = uber %>%
  group_by(lati, longi) %>%
  summarise(pickup_count = count()) %>%
  collect()

save(taxi_pickup, file = "taxi_pickup.RData")
save(taxi_dropoff, file = "taxi_dropoff.RData")
save(uber_pickup, file = "uber_pickup.RData")


# this chunck of code for plotting is also in hw7.Rmd file
## plot
#pickup
ggplot(taxi_pickup, aes(x = pickup_lat, y = pickup_long, col = taxi, alpha = log(pickup_count))) + 
  geom_point(size = 0.05) +
  scale_color_manual(values = c("#009E73", "#F0E442")) +
  labs(x = "Pickup Latitude", y = "Pickup Longitude")

#dropoff
ggplot(taxi_dropoff, aes(x = dropoff_lat, y = dropoff_long, col = taxi, 
                         alpha = log(dropoff_count))) +
  geom_point(size = 0.1) +
  scale_color_manual(values = c("#009E73", "#F0E442")) +
  labs(x = "Pickup Latitude", y = "Pickup Longitude")

#uber 
ggplot(uber_pickup, aes(x = lati, y = longi, alpha = log(pickup_count))) +
  geom_point(size = 0.05, color = "blue") + 
  labs(x = "Pickup Latitude", y = "Pickup Longitude")



#########Task2

# Construct the function of adding rush colomn
add_rush = function(df, label = "") {
  df %>%
    mutate(
      pickup_hour = hour(pickup_datetime),
      dropoff_hour = hour(dropoff_datetime),
      rush = ifelse(pickup_hour %in% c(7:10) | dropoff_hour %in% c(7:10),
                    "TRUE", "FALSE")
    )
}

# rush data frame for green cab
green_rush = green %>% 
  add_rush() %>%
  filter(rush == "TRUE")

# rush data frame for yellow cab
yellow_rush = yellow %>% 
  add_rush() %>%
  filter(rush == "TRUE")

# norush data frame for green cab
green_norush = green %>% 
  add_rush() %>%
  filter(rush == "FALSE")

# norush data frame for yellow cab
yellow_norush = yellow %>% 
  add_rush() %>%
  filter(rush == "FALSE")

## rush df for pickup
pickup_rush = rbind(
  green_rush %>% pickup_location_count("green"),
  yellow_rush %>% pickup_location_count("yellow")
)

## norush df for pickup
pickup_norush = rbind(
  green_norush %>% pickup_location_count("green"),
  yellow_norush %>% pickup_location_count("yellow")
)

## rush df for dropoff
dropoff_rush = rbind(
  green_rush %>% dropoff_location_count("green"),
  yellow_rush %>% dropoff_location_count("yellow")
)

## norush df for dropoff
dropoff_norush = rbind(
  green_norush %>% dropoff_location_count("green"),
  yellow_norush %>% dropoff_location_count("yellow")
)

# Add rush for UBER
uber_pickupHour = uber %>% 
  mutate(
    # extract hour from the string
    pickup_hour = regexp_extract(DateTime, "([0-9]+):([0-9]+)", 1),
    rush = ifelse(pickup_hour %in% c(7:10), "TRUE","FALSE") 
  ) 

## rush df for uber pickup
uber_pickup_rush = uber_pickupHour %>%
  filter(rush == "TRUE") %>%
  group_by(lati, longi) %>%
  summarise(count = count()) %>%
  collect()

## norush df for uber pickup
uber_pickup_norush = uber_pickupHour %>%
  filter(rush == "FALSE") %>%
  group_by(lati, longi) %>%
  summarise(count = count()) %>%
  collect()

# combine all 6 df to generate a df in right format for ggplot
dropoff_norush %<>% mutate(time = "Non-rush", type = "dropoff") 
dropoff_rush %<>% mutate(time = "Rush",type = "dropoff") 
drop_off = rbind(dropoff_norush, dropoff_rush)
pickup_norush %<>% mutate(time = "Non-rush", type = "pickup") 
pickup_rush %<>% mutate(time = "Rush",type = "pickup")  
pick_up = rbind(pickup_norush, pickup_rush)
uber_pickup_norush %<>% mutate(taxi = "uber", time = "Non-rush", type = "uberpickup")
uber_pickup_rush %<>% mutate(taxi = "uber",time = "Rush", type = "uberpickup")
uber_pick = rbind(uber_pickup_norush, uber_pickup_rush)

# Obtain the final data frame
all = rbind(drop_off,setNames(pick_up,names(drop_off)))
all = rbind(all,setNames(uber_pick,names(all)))
names(all) = c("lat","long","count","taxi","time","type")

save(all, file = "all.RData")

# Plot
ggplot(all, aes(x = lat, y = long, color = taxi,alpha = log(count))) +
  geom_point(size = 0.05) + 
  scale_colour_manual(values=c("#009E73","#000000","#F0E442")) + 
  theme_bw() +
  facet_grid(type~time, scales="free") +
  ylab("")



