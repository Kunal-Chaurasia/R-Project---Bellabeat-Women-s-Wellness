library(tidyverse)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)

daily_activities <- read.csv("Daily Activities.csv")
hourly_intensity <- read.csv("Hourly Intensity.csv")
hourly_calories <- read.csv("Hourly Calories.csv")
sleep_data <- read.csv("Sleep Data.csv")
weight_log <- read.csv("Weight Log.csv")

head(daily_activities)
head(hourly_intensity)
head(hourly_calories)
head(sleep_data)
head(weight_log)

# Daily Activities
daily_activities$ActivityDate = as.POSIXct(daily_activities$ActivityDate, format = "%m-%d-%Y", tz=Sys.timezone())
daily_activities$Date = format(daily_activities$ActivityDate, format = "%m/%d/%Y")

# Hourly Intensity
hourly_intensity$ActivityHour = as.POSIXct(hourly_intensity$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_intensity$Date = format(hourly_intensity$ActivityHour, format = "%m/%d/%Y")
hourly_intensity$Time = format(hourly_intensity$ActivityHour, format = "%H:%M:%S")

# Hourly Calorie
hourly_calories$ActivityHour = as.POSIXct(hourly_calories$ActivityHour, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
hourly_calories$Date = format(hourly_calories$ActivityHour, format = "%m/%d/%Y")
hourly_calories$Time = format(hourly_calories$ActivityHour, format = "%H:%M:%S")

# Sleep Data
sleep_data$SleepDay = as.POSIXct(sleep_data$SleepDay, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
sleep_data$Date = format(sleep_data$SleepDay, format = "%m/%d/%Y")

# Weight Log
weight_log$Date = as.POSIXct(weight_log$Date, format = "%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
weight_log$Date = format(weight_log$Date, format = "%m/%d/%Y")

n_distinct(daily_activities$Id)
n_distinct(hourly_calories$Id)
n_distinct(hourly_intensity$Id)
n_distinct(sleep_data$Id)
n_distinct(weight_log$Id)

nrow(daily_activities)
nrow(hourly_calories)
nrow(hourly_intensity)
nrow(sleep_data)
nrow(weight_log)

daily_activities %>%
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes) %>%
  summary()

hourly_calories %>%
  select(Calories) %>%
  summary()

hourly_intensity %>%
  select(TotalIntensity) %>%
  summary()

sleep_data %>% 
  select(TotalMinutesAsleep,
         TotalTimeInBed,
         TotalSleepRecords) %>%
  summary()

weight_log %>%
  select(WeightKg,
         BMI) %>%
  summary()

ggplot(data=daily_activities, aes(x=TotalSteps, y=SedentaryMinutes)) + geom_point(color="blue") + geom_smooth() + labs(title="Total Steps vs Sedentary Minutes")

ggplot(data=daily_activities, aes(x=TotalSteps, y=Calories, color=Id)) + geom_point() + geom_smooth() +  labs(title="Total Steps vs Calories Burned") + scale_color_viridis_c()

ggplot(data=sleep_data) +   geom_point(mapping = aes(x=TotalMinutesAsleep, y=TotalTimeInBed, color=TotalSleepRecords)) +  labs(title="Total Time Asleep vs Total Time in Bed in Minutes") +  scale_color_viridis_c()

ggplot(data=daily_activities, aes(x=VeryActiveMinutes, y=Calories, color=TotalSteps)) + geom_point() + geom_smooth() +  labs(title="Total Steps vs Calories Burned") + scale_color_viridis_c()

ggplot(data=daily_activities, aes(x=SedentaryMinutes, y=Calories, color=TotalSteps)) + geom_point() + geom_smooth() +  labs(title="Total Steps vs Calories Burned") + scale_color_viridis_c()

intensity_by_hour <- hourly_intensity %>%
  group_by(Time) %>%
  drop_na() %>%
  summarize(mean_intensity = mean(TotalIntensity))

ggplot(data=intensity_by_hour, aes(x=Time, y=mean_intensity)) + geom_histogram(stat = "identity", fill="pink") + theme(axis.text.x = element_text(angle = 90)) + labs(title = "Mean intensity by Hour")

combined_activities_sleep <- merge(daily_activities, sleep_data, by= c("Id", "Date"))
View(combined_activities_sleep)

ggplot(data=combined_activities_sleep, aes(x=VeryActiveMinutes, y=TotalMinutesAsleep, color=Calories)) + geom_point() + geom_smooth() + labs(title = "Very Active Duration vs Sleep Time") + scale_color_viridis_c()

ggplot(data=combined_activities_sleep, aes(x=SedentaryMinutes, y=TotalMinutesAsleep, color=TotalSteps)) + geom_point() + geom_smooth() + labs(title = "Sedentary Duration vs Sleep Time") + scale_color_viridis_c()

combined_activities_weight <- merge(daily_activities, weight_log, by= "Id")
View(combined_activities_weight)

ggplot(data=combined_activities_weight, aes(x=TotalSteps, y=Calories, color=WeightKg)) + geom_point() + geom_smooth() + labs(title="Total Steps vs Calories Burned")
