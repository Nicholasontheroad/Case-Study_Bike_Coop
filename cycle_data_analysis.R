# Author:Nicholas Donald Goray Github name: Nicholasontheroad 
# This is the code for my Google capstone project for my Google Data Analytics Career Certifcate
# Completed 9/4/2022
# Using data from : https://divvy-tripdata.s3.amazonaws.com/index.html License provided at : https://ride.divvybikes.com/data-license-agreement

# Data Preparation Phase


# first install required packages
install.packages("tidyverse") #helps wrangle data
install.packages("lubridate") #helps wrangle date attributes
install.packages("ggplot2") #helps visualize data

# use packages we installed
library(tidyverse)  
library(lubridate)  
library(ggplot2) 

# set working directory and get working directory
getwd() #displays your working directory
setwd("C:/Users/your user name /the folder of your project/csv") #sets your working directory

# collect data, for this you want to use the last 12 months of data
# data source: https://divvy-tripdata.s3.amazonaws.com/index.html

# this uploads the csv files from the working directory, if you receive errors recheck 

# if prior step is successful, move to next step
#wranngle data, now please following code to put data into one file 
#we need to compare colomn names as we are going to fuse them into on file
#they have to match perfectly in order for this to work


tripdata_202108 <- read.csv("C:/Users/your user name /the folder of your project/csv/202108-divvy-tripdata.csv")
tripdata_202109 <- read.csv("C:/Users/your user name /the folder of your project/csv/202109-divvy-tripdata.csv")
tripdata_202110 <- read.csv("C:/Users/your user name /the folder of your project/csv/202110-divvy-tripdata.csv")
tripdata_202111 <- read.csv("C:/Users/your user name /the folder of your project/csv/202111-divvy-tripdata.csv")
tripdata_202112 <- read.csv("C:/Users/your user name /the folder of your project/csv/202112-divvy-tripdata.csv")
tripdata_202201 <- read.csv("C:/Users/your user name /the folder of your project/csv/202201-divvy-tripdata.csv")
tripdata_202202 <- read.csv("C:/Users/your user name /the folder of your project/csv/202202-divvy-tripdata.csv")
tripdata_202203 <- read.csv("C:/Users/your user name /the folder of your project/csv/202203-divvy-tripdata.csv")
tripdata_202204 <- read.csv("C:/Users/your user name /the folder of your project/csv/202204-divvy-tripdata.csv")
tripdata_202205 <- read.csv("C:/Users/your user name /the folder of your project/csv/202205-divvy-tripdata.csv")
tripdata_202206 <- read.csv("C:/Users/your user name /the folder of your project/csv/202206-divvy-tripdata.csv")
tripdata_202207 <- read.csv("C:/Users/your user name /the folder of your project/csv/202207-divvy-tripdata.csv")



#we need to compare colomn names as we are going to fuse them into on file

colnames(tripdata_202108)
colnames(tripdata_202109)
colnames(tripdata_202110)
colnames(tripdata_202111)
colnames(tripdata_202112)
colnames(tripdata_202201)
colnames(tripdata_202202)
colnames(tripdata_202203)
colnames(tripdata_202204)
colnames(tripdata_202205)
colnames(tripdata_202206)
colnames(tripdata_202207)

# Inspect the dataframes and look for incongruencies
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)
str(tripdata_202207)

# Convert ride_id and rideable_type to character so that they can stack correctly
tripdata_202108 <-  mutate(tripdata_202108, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202109 <-  mutate(tripdata_202109, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202110 <-  mutate(tripdata_202110, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202111 <-  mutate(tripdata_202111, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202112 <-  mutate(tripdata_202112, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202201 <-  mutate(tripdata_202201, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202202 <-  mutate(tripdata_202202, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202203 <-  mutate(tripdata_202203, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202204 <-  mutate(tripdata_202204, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202205 <-  mutate(tripdata_202205, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202206 <-  mutate(tripdata_202206, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 
tripdata_202207 <-  mutate(tripdata_202207, ride_id = as.character(ride_id)
                   ,rideable_type = as.character(rideable_type)) 




# Stack individual quarter's data frames into one big data frame
all_trips <- bind_rows(tripdata_202207, tripdata_202206, tripdata_202205, tripdata_202204, tripdata_202203, tripdata_202202,
                       tripdata_202201, tripdata_202112, tripdata_202111, tripdata_202110, tripdata_202109, tripdata_202108)

#step 3 data cleaning phase

# Inspect the new table that has been created
colnames(all_trips)  #List of column names
nrow(all_trips)  #How many rows are in data frame?
dim(all_trips)  #Dimensions of the data frame?
head(all_trips)  #See the first 6 rows of data frame.  Also tail(all_trips)
str(all_trips)  #See list of columns and data types (numeric, character, etc)
summary(all_trips)  #Statistical summary of data. Mainly for numerics

# There are a few problems we will need to fix:
# (1) In the "member_casual" column, there are two names for members ("member" and "Subscriber") and two names for casual riders ("Customer" and "casual"). We will need to consolidate that from four to two labels.
# (2) The data can only be aggregated at the ride-level, which is too granular. We will want to add some additional columns of data -- such as day, month, year -- that provide additional opportunities to aggregate the data.
# (3) We will want to add a calculated field for length of ride since the 2020Q1 data did not have the "tripduration" column. We will add "ride_length" to the entire dataframe for consistency.
# (4) There are some rides where tripduration shows up as negative, including several hundred rides where Divvy took bikes out of circulation for Quality Control reasons. We will want to delete these rides.

# In the "member_casual" column, replace "Subscriber" with "member" and "Customer" with "casual"
# Before 2020, Divvy used different labels for these two types of riders ... we will want to make our dataframe consistent with their current nomenclature
# N.B.: "Level" is a special property of a column that is retained even if a subset does not contain any values from a specific level
# Begin by seeing how many observations fall under each usertype
table(all_trips$member_casual)

# Reassign to the desired values (we will go with the current 2022 labels)
all_trips <-  all_trips %>% 
  mutate(member_casual = recode(member_casual
                           ,"Subscriber" = "member"
                           ,"Customer" = "casual"))

# Check to make sure the proper number of observations were reassigned
table(all_trips$member_casual)

# Add columns that list the date, month, day, and year of each ride
# This will allow us to aggregate ride data for each month, day, or year ... before completing these operations we could only aggregate at the ride level
# https://www.statmethods.net/input/dates.html more on date formats in R found at that link
all_trips$date <- as.Date(all_trips$started_at) #The default format is yyyy-mm-dd
all_trips$month <- format(as.Date(all_trips$date), "%m")
all_trips$day <- format(as.Date(all_trips$date), "%d")
all_trips$year <- format(as.Date(all_trips$date), "%Y")
all_trips$day_of_week <- format(as.Date(all_trips$date), "%A")

# Add a "ride_length" calculation to all_trips (in seconds)
# https://stat.ethz.ch/R-manual/R-devel/library/base/html/difftime.html
all_trips$ride_length <- difftime(all_trips$ended_at,all_trips$started_at)

# Inspect the structure of the columns
str(all_trips)

# Convert "ride_length" from Factor to numeric so we can run calculations on the data
is.factor(all_trips$ride_length)
all_trips$ride_length <- as.numeric(as.character(all_trips$ride_length))
is.numeric(all_trips$ride_length)

# Remove "bad" data
# The dataframe includes a few hundred entries when bikes were taken out of docks and checked for quality by Divvy or ride_length was negative
# We will create a new version of the dataframe (v2) since data is being removed
# https://www.datasciencemadesimple.com/delete-or-drop-rows-in-r-with-conditions-2/
all_trips_v2 <- all_trips[!(all_trips$start_station_name == "HQ QR" | all_trips$ride_length<0),]

#=====================================
# STEP 4: CONDUCT DESCRIPTIVE ANALYSIS
#=====================================
# Descriptive analysis on ride_length (all figures in seconds)
mean(all_trips_v2$ride_length) #straight average (total ride length / rides)
median(all_trips_v2$ride_length) #midpoint number in the ascending array of ride lengths
max(all_trips_v2$ride_length) #longest ride
min(all_trips_v2$ride_length) #shortest ride

# You can condense the four lines above to one line using summary() on the specific attribute
summary(all_trips_v2$ride_length)

# Compare members and casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = mean)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = median)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = max)
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual, FUN = min)

# See the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# Notice that the days of the week are out of order. Let's fix that.
all_trips_v2$day_of_week <- ordered(all_trips_v2$day_of_week, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

# Now, let's run the average ride time by each day for members vs casual users
aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)

# analyze ridership data by type and weekday
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>%  #creates weekday field using wday()
  group_by(member_casual, weekday) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
  ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(member_casual, weekday)								# sorts

# Let's visualize the number of rides by rider type
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = number_of_rides, fill = member_casual)) +
  geom_col(position = "dodge")

# Let's create a visualization for average duration
all_trips_v2 %>% 
  mutate(weekday = wday(started_at, label = TRUE)) %>% 
  group_by(member_casual, weekday) %>% 
  summarise(number_of_rides = n()
            ,average_duration = mean(ride_length)) %>% 
  arrange(member_casual, weekday)  %>% 
  ggplot(aes(x = weekday, y = average_duration, fill = member_casual)) +
  geom_col(position = "dodge")

#=================================================
# STEP 5: EXPORT SUMMARY FILE FOR FURTHER ANALYSIS
#=================================================


counts <- aggregate(all_trips_v2$ride_length ~ all_trips_v2$member_casual + all_trips_v2$day_of_week, FUN = mean)
write.csv(counts, file = "C:/Users/your user name /the folder of your project/avg_ride_length.csv")

# now you can export your csv file into a tool for vizualization. just as Google sheets or excel or Tablelu 











