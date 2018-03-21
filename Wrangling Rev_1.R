######
## PART TWO
######

library(readr)
library(dplyr)

## First I read in the data for February 2009 at the Mixing Bowl in Little Cayman
headings <- c("Rank", "Spec_Num", "Common_Name", "T_SF", "T_Den", "E_SF", "E_Den", "N_SF", "N_Den")
survey <- read_csv(file = "2009-02.csv", skip = 2, col_names = headings)

## I now create two new data frames: one to identify the sighting frequency of each species and one to identify the density of each species.  I first remove all unneeded rows and columns.  Then, a new vector is created with proper header names and applied to the data frame.

sf_series <- survey[, c(2:4)]
colnames(sf_series)[3] <- "2009-02"
sighting_frequency <- sf_series[order(as.numeric(sf_series$Spec_Num), decreasing=TRUE), ]

den_series <- survey[, c(2, 3, 5)]
colnames(den_series)[3] <- "2009-02"
density <- den_series[order(as.numeric(den_series$Spec_Num), decreasing=TRUE), ]


## Here I create two functions that will be used to modify all future imported files and append the data to the "sighting_frequency" and "density" data frames created previously
SF_modification <- function(survey, month, sighting_frequency){
  #sf_series <- survey[-1, c(2:4)]
  sf_series <- survey[, c(2:4)]
  colnames(sf_series)[3] <- month
  sighting_frequency <- sighting_frequency %>%
    full_join(sf_series, by=NULL)
}

Den_modification <- function(survey, month, density){
  #den_series <- survey[-1, c(2, 3, 5)]
  den_series <- survey[, c(2, 3, 5)]
  colnames(den_series)[3] <- month
  density <- density %>%
    full_join(den_series, by=NULL)
}

## Here I begin importing the data from individual csv files.

## The second file
survey <- read_csv(file = "2009-06.csv", skip = 2, col_names = headings)
month <- "2009-06"

sighting_frequency <- SF_modification(survey, month, sighting_frequency)
density <- Den_modification(survey, month, density)

## The third file
survey <- read_csv(file = "2009-08.csv", skip = 2, col_names = headings)
month <- "2009-08"

sighting_frequency <- SF_modification(survey, month, sighting_frequency)
density <- Den_modification(survey, month, density)

## The fourth file
survey <- read_csv(file = "2009-09.csv", skip = 2, col_names = headings)
month <- "2009-09"

sighting_frequency <- SF_modification(survey, month, sighting_frequency)
density <- Den_modification(survey, month, density)

## The fifth file
survey <- read_csv(file = "2009-12.csv", skip = 2, col_names = headings)
month <- "2009-12"

sighting_frequency <- SF_modification(survey, month, sighting_frequency)
density <- Den_modification(survey, month, density)


## Here I reorganize the two data frames by ascending order of species ID
sighting_frequency <- sighting_frequency[order(as.numeric(sighting_frequency$Spec_Num), decreasing=FALSE), ]
density <- density[order(as.numeric(density$Spec_Num), decreasing=FALSE), ]

