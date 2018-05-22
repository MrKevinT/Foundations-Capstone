### Springboard
### Introduction to Data Science
### Capstone Project
### Data Wrangling

library(readr)
library(dplyr)
library(stringr)
library(ggplot2)
library(lubridate)


############
# READ IN SURVEY DATA AND REPLACE CERTAIN COLUMN HEADINGS
############
Survey_Data <- read_tsv("littlecayman_040918.txt")
Survey_Data <- rename(Survey_Data, Species_ID = Species, Family_ID = Family)

############
# CLEAN UP THE DATA
############
# Change the date to a different format
Survey_Data$Date <- mdy(Survey_Data$Date)
# Replace all NAs with 0 in the MaxDepth column
Survey_Data$MaxDepth[which(is.na(Survey_Data$MaxDepth))] <- 0
# Remove the two observations with Species_ID, Family_ID and Abundance = 0
Survey_Data <- Survey_Data %>% filter(Abundance != 0)

############
# READ IN SPECIES DATA
############
S_Heading <- c("Species_ID", "English_Name", "Scientific", "Family_ID", "Family_Name")
Species <- read_tsv("REEF_TWAspecies.txt", skip=1, col_names = S_Heading)
# Remove the redundant Family_ID
Species <- Species %>% select(-Family_ID)

############
# READ IN AND MANIPULATE ZONE CODE DATA
############
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
# Remove unnecessary columns
Zone_Codes <- Zone_Codes %>% select(-None, -Lat, -Long)

# Create a plot for dive site visualization
ggplot(Zone_Codes, aes(Longitude, Latitude, labels = Site_Name)) +
  geom_point(na.rm = TRUE)  +
  ggtitle("Coordinate Map of Little Cayman Island Dive Sites") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

############
# JOIN THE THREE DATA FILES
############
Little_Cayman <- Survey_Data %>% left_join(Species, by = "Species_ID")
Little_Cayman <- Little_Cayman %>% left_join(Zone_Codes, by = "Geozone")

write_csv(Little_Cayman, "Little_Cayman.csv")
Sample <- Little_Cayman %>% filter(Family_Name == "Angelfish")
write_csv(Sample, "Angelfish_Sample.csv")

