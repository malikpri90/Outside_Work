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

#% of TOp 3 FInishes till the data



#Constructor 


#% of top 3 finishes last year


#% of top 3 finishes this year up till previous race

