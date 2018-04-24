### Springboard
### Introduction to Data Science
### Capstone Project
### Data Wrangling

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)

# Read in survey data and replace certain column headings
Survey_Data <- read_tsv("littlecayman_040918.txt")
Survey_Data <- rename(Survey_Data, Species_ID = Species, Family_ID = Family)

# Changing the date to a different format
Survey_Data$Date <- mdy(Survey_Data$Date)
# Replacing all NAs with 0
Survey_Data$MaxDepth[which(is.na(Survey_Data$MaxDepth))] <- 0
# Remove the two observations with Species_ID and Abundance = 0
Survey_Data <- Survey_Data %>% filter(Abundance != 0)

# Read in species data
S_Heading <- c("Species_ID", "English_Name", "Scientific", "Family_ID", "Family_Name")
Species <- read_tsv("REEF_TWAspecies.txt", skip=1, col_names = S_Heading)
Species <- Species %>% select(-Family_ID) # Remove the redundant Family_ID

# Read in zone code data
Zone_Heading <- c("None", "Geozone", "Site_Name", "Lat", "Long")
Zone_Codes <- read_delim("LC_Zonecodes.txt", skip = 8, delim = "|", col_names = Zone_Heading)
Lat_Deg <- as.numeric(word(Zone_Codes$Lat, 1, sep = " "))
Lat_Min <- as.numeric(word(Zone_Codes$Lat, -1, sep = " "))
Long_Deg <- as.numeric(word(Zone_Codes$Long, 1, sep = " "))
Long_Min <- as.numeric(word(Zone_Codes$Long, -1, sep = " "))

# Convert the latitude and longitude measures from degrees and minutes to degrees
Latitude <- vector("double", nrow(Zone_Codes))
Longitude <- vector("double", nrow(Zone_Codes))
for(i in seq_along(Latitude)) {
  Latitude[i] <- Lat_Deg[i] + Lat_Min[i]/60
  Longitude[i] <- Long_Deg[i] - Long_Min[i]/60
}
Zone_Codes <- cbind(Zone_Codes, Latitude, Longitude)
Zone_Codes <- Zone_Codes %>% select(-None, -Lat, -Long) # Remove unnecessary columns

Little_Cayman <- Survey_Data %>% left_join(Species, by = "Species_ID")
Little_Cayman <- Little_Cayman %>% left_join(Zone_Codes, by = "Geozone")

write_csv(Little_Cayman, "Little_Cayman.csv")

Sample <- Little_Cayman %>% filter(Family_Name == "Angelfish")
write_csv(Sample, "Angelfish_Sample.csv")

