######
## PART TWO
######

library(readr)
library(dplyr)

# list the dive sites of interest
site_list <- c("Mixing_Bowl", "The_Great_Wall", "Lea_Lea", "Marilynns_Cut", "Randys_Gazebo")

# Identify the headings for all data files
headings <- c("Rank", "Spec_Num", "Common_Name", "T_SF", "T_Den", "E_SF", "E_Den", "N_SF", "N_Den")

## Here I create two functions that will be used to modify imported files and append the data to the "sighting_frequency" and "density" data frames
SF_modification <- function(survey, current_file, sighting_frequency){
  sf_series <- survey[, c("Spec_Num", "Common_Name", "T_SF")]
  colnames(sf_series)[3] <- current_file
  sighting_frequency <- sighting_frequency %>%
    full_join(sf_series, by=NULL)
}

Den_modification <- function(survey, current_file, density){
  den_series <- survey[, c("Spec_Num", "Common_Name", "T_Den")]
  colnames(den_series)[3] <- current_file
  density <- density %>%
    full_join(den_series, by=NULL)
}

##### Importing Data
# set the working directory (for current dive site)
setwd("/Users/Kevin/Documents/Mixing_Bowl/")

# Create a character vector of the files that I need to import
file_list <- list.files("/Users/Kevin/Documents/Mixing_Bowl")

# Create a list of the file names (including the unwanted .csv)
file_list2 <- strsplit(file_list[c(1:length(file_list))], "\\.")

# Create a new vector for the list of files to read (without the .csv)
list_no_csv <- rep(NA, times=length(file_list2))
# Add values to the new vector (without the .csv)
for (i in 1:length(file_list2)) {
  list_no_csv[i] <- file_list2[[i]][1]
}

# Here I read in the first file in my file_list.
survey <- read_csv(file = file_list[1], skip = 2, col_names=headings)

## I now create two new data frames: one to identify the sighting frequency of each species and one to identify the density of each species.  I first remove all unneeded rows and columns.  Then, a new vector is created with proper header names and applied to the data frame.

sf_series <- survey[, c("Spec_Num", "Common_Name", "T_SF")]
colnames(sf_series)[3] <- list_no_csv[1]
sighting_frequency <- sf_series[order(as.numeric(sf_series$Spec_Num), decreasing=FALSE), ]

den_series <- survey[, c("Spec_Num", "Common_Name", "T_Den")]
colnames(den_series)[3] <- list_no_csv[1]
density <- den_series[order(as.numeric(den_series$Spec_Num), decreasing=FALSE), ]

for(i in 2:length(file_list)){
  survey <- read_csv(file = file_list[i], skip = 2, col_names = headings)
  current_file <- list_no_csv[i]
  sighting_frequency <- SF_modification(survey, current_file, sighting_frequency)
  density <- Den_modification(survey, current_file, density)
}

## Here I create data frames for the current dive site and order the data by species ID
MB_SF <- sighting_frequency[order(as.numeric(sighting_frequency$Spec_Num), decreasing=FALSE), ]
MB_DEN <- density[order(as.numeric(density$Spec_Num), decreasing=FALSE), ]

write_csv(MB_SF, "MB_SF.csv")
write_csv(MB_DEN, "MB_DEN.csv")

# Return working directory to Documents
setwd("/Users/Kevin/Documents/")
