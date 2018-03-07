
## First I will read in the file with the number of surveys per dive site in Little Cayman
mydata <- read.csv(file = "/Users/Kevin/Documents/Dive Site Data.csv", header=TRUE, stringsAsFactors = FALSE)
head(mydata) # observation only
tail(mydata) # observation only

## I now create a vector of proper header titles and add the titles to the data frame
## SA is Species and Abundance, SO is Species Only, BT is Bottom Time
## Expert and Novice are the two levels of surveyors
header <- c("Site ID", "Site_Name", "Expert_SA", "Expert_SO", "Novice_SA", "Novice_SO", "BT")
colnames(mydata) <- header

## I now create a new data frame after removing excess rows and unneeded columns
dive.sites <- mydata[-c(1:6, 91),]
dive.sites <- dive.sites[,c(1:3, 5)]

## I now create a new column adding the SA survey numbers from Expert and Novice together
dive.sites$Total_SA <- as.numeric(dive.sites$Expert_SA) + as.numeric(dive.sites$Novice_SA)

## For plotting
max(dive.sites$Total_SA)
hist(dive.sites$Total_SA, breaks = seq(0, 300, 25), col="blue", main = "Little Cayman\n Dive Site Survey Count\n (1993 to present)", xlab="Number of Surveys", ylab = "Number of Dive Sites")
abline(v=c(mean(dive.sites$Total_SA), median(dive.sites$Total_SA)), lty=c(2,3), lwd=2)
legend("topright", legend=c("mean Total_SA", "median Total_SA"), lty = c(2,3), lwd=2)

## Now I find all sites with 100 or more surveys and sort the data frame
final.sites <- dive.sites[dive.sites$Total_SA>=100,]
final.sites <- final.sites[order(final.sites$Total_SA, decreasing=TRUE), ]

## I can now pick the top 5 sites
study.sites <- final.sites[c(1:5),]

## Print the data frame of the top 5 sites to analyze
study.sites


