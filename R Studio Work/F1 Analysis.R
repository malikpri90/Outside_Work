##Installing Packages
install.packages("sqldf")

library(dplyr)
library(ggplot2)
library(shiny)
library(caret)
library(glmnet)
library(randomForest)
library(tidymodels)
library(tidyverse)
library(sqldf)

## Setting up A Working Directory 

getwd()

##Getting the Files in R

constructors_df <- read.csv("constructors.csv")
circuits_df <- read.csv("circuits.csv")
constructors_results_df <- read.csv("constructor_results.csv")
constructors_standings_df <- read.csv("constructor_standings.csv")
drivers_standings_df <- read.csv("driver_standings.csv")
drivers_df <- read.csv("drivers.csv")
race_df <- read.csv("races.csv")
results_df <- read.csv("results.csv")

## PreProcessing, Cleaning the Data and EDA

# Print the first few rows of the results data frame
print("Driver result of a race")
print(head(results_df))

# Print the first few rows of the race data frame
print("Race_df dataframe")
print(head(race_df))

# Sort the race data frame by year and round
race_df <- race_df[order(race_df$year, race_df$round),]

# Filter the race data frame to only include years greater than or equal to 1982
race_df <- subset(race_df, year >= 1982)

print(race_df)

# Create a new data frame with specific columns from the results data frame
res_df <- results_df[, c('raceId', 'driverId', 'constructorId', 'grid', 'positionOrder')]
print(res_df)

# Check for duplicate rows in the race data frame
duplicates <- duplicated(race_df)
num_duplicates <- sum(duplicates)
print(paste("Number of duplicate rows: ", num_duplicates))

print(race_df)

# Merge the race and results data frames on the 'raceId' column
df <- merge(race_df, res_df, by = 'raceId')
print(df)


print(str(df))
print(summary(df))

#Year and RaceId BarPlot
races_per_year <- table(df$year)
barplot(races_per_year, main = "Number of Formula 1 Races per Year", xlab = "Year", ylab = "Number of Races")

#ConstructorId to Name

constructor_name_dict <- setNames(as.list(constructors_df$name), 
                                  constructors_df$constructorId)
df$constructorId <- as.character(df$constructorId)

races_per_team <- table(df$constructorId)
races_per_team_sorted <- sort(races_per_team, decreasing = TRUE)



# Sort the data from most races to least races and select top 15
races_per_team_sorted <- races_per_team %>% arrange(desc(n)) %>% head(15)

# Create a bar plot
ggplot(races_per_team_sorted, aes(x = reorder(driverId, n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Top 15 Formula 1 Drivers by Number of Races since 1982",
       x = "Team Name",
       y = "Number of Races") +
  theme_minimal()


#Actual Cool Part: Engineering 

#Adding a top 3 Finish

df$'Top 3 Finish' <- as.integer(df$positionOrder <= 3)
print(head(df))

numberrace <- length(unique(df$raceId))
print(paste("Number of unique drivers:", numberrace))

#Driver
#% of Top 3 Finishes 2022

# Calculating the total number of races and top 3 finishes for each driver in each year
driver_yearly_stats <- df %>%
  group_by(year, driverId) %>%
  summarise(Total_Races = n_distinct(raceId),
            Top_3_Finishes = sum(`Top 3 Finish`)) %>%
  ungroup()

print("Driver annual stats")
print(head(driver_yearly_stats))

# Calculating the percentage of top 3 finishes for each driver in each year
driver_yearly_stats <- driver_yearly_stats %>%
  mutate(`Driver Top 3 Finish Percentage (This Year)` = (Top_3_Finishes / Total_Races) * 100)
driver_yearly_stats

# Shifting the driver percentages to the next year for last year's data
driver_last_year_stats <- driver_yearly_stats
driver_last_year_stats$year <- driver_last_year_stats$year + 1
names(driver_last_year_stats)[names(driver_last_year_stats) == "Driver Top 3 Finish Percentage (This Year)"] <- "Driver Top 3 Finish Percentage (Last Year)"

df <- merge(df, driver_last_year_stats[c("year", "driverId", "Driver Top 3 Finish Percentage (Last Year)")], by = c("year", "driverId"), all.x = TRUE)

print(df[df$year >= 1983, ])


#% Constructor of top 3 finishes last year

constructor_last_year_stats <- df %>%
  group_by(year, constructorId, round) %>%
  summarise(Sum_Top_3_Finishes_Last_Year = sum(`Driver Top 3 Finish Percentage (Last Year)`, na.rm = TRUE)) %>%
  ungroup()

print(head(constructor_last_year_stats))

constructor_last_year_stats <- constructor_last_year_stats %>%
  mutate(`Constructor Top 3 Finish Percentage (Last Year)` = Sum_Top_3_Finishes_Last_Year / 2)

df <- merge(df, constructor_last_year_stats[c("year", "constructorId", "round", "Constructor Top 3 Finish Percentage (Last Year)")], by = c("year", "constructorId", "round"), all.x = TRUE)


print(df[df$year >= 1983, ])

#% Driver of top 3 finishes this year up till previous race
calculate_driver_top_3_percentage_before_round <- function(year, driverId, round, df) {
  # Filter for races in the same year, for the same driver, but in earlier rounds
  previous_races <- df[df$year == year & df$driverId == driverId & df$round < round,]
  if (nrow(previous_races) == 0) {
    return(NA)
  }
  
  total_races <- length(unique(previous_races$raceId))
  top_3_finishes <- sum(previous_races$`Top 3 Finish`)
  
  # Calculate the percentage
  if (total_races > 0) {
    return((top_3_finishes / total_races) * 100)
  } else {
    return(NA)
  }
}

# Apply the function to each row in the DataFrame
df <- df %>%
  rowwise() %>%
  mutate(`Driver Top 3 Finish Percentage (This Year till last race)` = calculate_driver_top_3_percentage_before_round(year, driverId, round, df))


# Calculating mean of top 3 finishes percentages for the two drivers in each constructor this year
constructor_this_year_stats <- df %>%
  group_by(year, constructorId, round) %>%
  summarise(Sum_Top_3_Finishes_This_Year = sum(`Driver Top 3 Finish Percentage (This Year till last race)`)) %>%
  data.frame()

print("Constructor annual stats")
print(constructor_this_year_stats)

# Calculating the percentage of top 3 finishes for each constructor this year
constructor_this_year_stats$`Constructor Top 3 Finish Percentage (This Year till last race)` <- constructor_this_year_stats$Sum_Top_3_Finishes_This_Year / 2

# Merging the data
df <- merge(df, constructor_this_year_stats[c('year', 'constructorId', 'round', 'Constructor Top 3 Finish Percentage (This Year till last race)')], by=c('year', 'constructorId', 'round'), all.x=TRUE)

# Checking the merged data
print("New dataframe")
print(df[df$year >= 1983,])

#% Constructor of top 3 finishes this year up till previous race

#Driver: Average Finishing Position for Past Year

driver_yearly_stats <- df %>%
  group_by(year, driverId) %>%
  summarise(Total_Races = n_distinct(raceId),
            Avg_position = mean(positionOrder, na.rm = TRUE)) %>%
  ungroup()

print("Driver annual stats")
print(driver_yearly_stats)

# Calculating the percentage of top 3 finishes for each driver in each year
driver_yearly_stats$`Driver Avg position (This Year)` <- driver_yearly_stats$Avg_position

# Shifting the driver percentages to the next year for last year's data
driver_last_year_stats <- driver_yearly_stats
driver_last_year_stats$year <- driver_last_year_stats$year + 1
names(driver_last_year_stats)[names(driver_last_year_stats) == "Driver Avg position (This Year)"] <- "Driver Avg position (Last Year)"

# Merging the data
df <- merge(df, driver_last_year_stats[c('year', 'driverId', 'Driver Avg position (Last Year)')], by=c('year', 'driverId'), all.x=TRUE)

# Checking the merged data
print("New dataframe")
print(df[df$year >= 1983,])

library(dplyr)

# Calculating sum of average positions for the two drivers in each constructor last year
constructor_last_year_stats <- df %>%
  group_by(year, constructorId, round) %>%
  summarise(sum_position_last_year = sum(`Driver Avg position (Last Year)`, na.rm = TRUE)) %>%
  ungroup()

print("Constructor annual stats")
print(constructor_last_year_stats)

# Calculating the average position for each constructor last year
constructor_last_year_stats$`Constructor Avg position (Last Year)` <- constructor_last_year_stats$sum_position_last_year / 2

# Merging the data
df <- merge(df, constructor_last_year_stats[c('year', 'constructorId', 'round', 'Constructor Avg position (Last Year)')], by=c('year', 'constructorId', 'round'), all.x=TRUE)

# Checking the merged data
print("New dataframe")
print(df[df$year >= 1983,])


# Creating a function to calculate the average driver position before the current round
calculate_driver_avg_position_before_round <- function(year, driverId, round, df) {
  # Filter for races in the same year, for the same driver, but in earlier rounds
  previous_races <- df[df$year == year & df$driverId == driverId & df$round < round,]
  if (nrow(previous_races) == 0) {
    return(NA)
  }
  
  # Calculate the total races and sum of positions
  total_races <- length(unique(previous_races$raceId))
  positionSum <- sum(previous_races$positionOrder, na.rm = TRUE)
  
  # Calculate average position
  if (total_races > 0) {
    return(positionSum / total_races)
  } else {
    return(NA)
  }
}

# Apply the function to each row in the DataFrame
df <- df %>%
  rowwise() %>%
  mutate(`Driver Average Position (This Year till last race)` = calculate_driver_avg_position_before_round(year, driverId, round, df))

# Calculating sum of average positions for the two drivers in each constructor this year
constructor_this_year_stats <- df %>%
  group_by(year, constructorId, round) %>%
  summarise(sum_Position_Constructor = sum(`Driver Average Position (This Year till last race)`, na.rm = TRUE)) %>%
  ungroup()

print("Constructor annual stats")
print(constructor_this_year_stats)

# Calculating the average position for each constructor this year
constructor_this_year_stats$`Constructor Average Position (This Year till last race)` <- constructor_this_year_stats$sum_Position_Constructor / 2

# Merging the data
df <- merge(df, constructor_this_year_stats[c('year', 'constructorId', 'round', 'Constructor Average Position (This Year till last race)')], by=c('year', 'constructorId', 'round'), all.x=TRUE)

# Checking the merged data
print("New dataframe")
print(df[df$year >= 1983,])

print(df[df$year == 2023 & df$round > 3, ])

top3_finish_freq <- as.data.frame(table(df$`Top 3 Finish`))
names(top3_finish_freq) <- c("Top 3 Finish", "Frequency")

#Histogram for Top 3 Finishes

#Feature for Training Purposes

# Remove 'raceId' column
df_final <- df[, !(names(df) %in% "raceId")]

# Print number of rows in total
print(paste("Number of rows in total:", nrow(df_final)))

# Count rows where 'year' is not 1982 before dropping NA values
initial_count <- nrow(df_final[df_final$year != 1982, ])

# Drop rows with NA values
df_final <- na.omit(df_final)

# Count rows where 'year' is not 1982 after dropping NA values
final_count <- nrow(df_final[df_final$year != 1982, ])

# Calculate the number of rows dropped
rows_dropped <- initial_count - final_count

print(paste("Number of rows dropped where year is not 1982:", rows_dropped))

# Copy df_final to df_final_keepPositionOrder
df_final_keepPositionOrder <- df_final

#Here we dropped 4078 rows out of 17321 rows that we have. 
#Therefore, we will not have these data for our model prediction. 
#Thus, a limitation of our model is that it cannot make prediction 
#for drivers/teams that did no participate 
#in the year before, and it cannot predict the 
#first race of the year

# Remove 'positionOrder' column from df_final
df_final <- df_final[, !(names(df_final) %in% "positionOrder")]

# Print df_final
print(df_final)


#Exploratory Data Analysis

# Convert columns to numeric
df_final$`Driver Top 3 Finish Percentage (This Year till last race)` <- as.numeric(df_final$`Driver Top 3 Finish Percentage (This Year till last race)`)
df_final$`Constructor Top 3 Finish Percentage (This Year till last race)` <- as.numeric(df_final$`Constructor Top 3 Finish Percentage (This Year till last race)`)
df_final$`Driver Average Position (This Year till last race)` <- as.numeric(df_final$`Driver Average Position (This Year till last race)`)
df_final$`Constructor Average Position (This Year till last race)` <- as.numeric(df_final$`Constructor Average Position (This Year till last race)`)

# Using summary() and selecting specific rows
description <- summary(df_final)
selected_description <- rbind(description[1,], description[4:6,])

# Print selected_description
print(selected_description)

# Calculate average finish position per year
avg_finish_per_year <- df %>% 
  group_by(year) %>% 
  summarise(Avg_Position = mean(positionOrder, na.rm = TRUE))

# Plot using ggplot2
ggplot(avg_finish_per_year, aes(x = year, y = Avg_Position)) +
  geom_line() +
  labs(title = "Average Finish Position per Year", x = "Year", y = "Average Finish Position")

