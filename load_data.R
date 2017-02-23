
#Temporary code to retrieve zip codes from Google API
library(ggmap)
crime_data <- read.csv("Seattle_Police_Department_Police_Report_Incident.csv")

retrieveZipCodes <- function(longitude, latitude){
  locationVar <- as.numeric(c(longitude,latitude))
  res <- revgeocode(location = as.numeric(locationVar))
  print(res)
}


#editedCrimeData <- crime_data[456:500]

#for(i in 1:nrow(editedCrimeData)){
  
#  editedCrimeData[i,]$zipCode <- retrieveZipCodes(editedCrimeData[i,]$Longitude, 
#                                                  editedCrimeData[i,]$Latitude)
#}

library(dplyr)
cleaned_crime_data <- crime_data %>%
              filter(Year == 2016)
cleaned_crime_data <- cleaned_crime_data %>%
       filter(Offense.Code != "X")
cleaned_crime_data <- cleaned_crime_data %>%
    filter(District.Sector != "NULL")
cleaned_crime_data <- cleaned_crime_data %>%
  filter(Zone.Beat != "NULL")
cleaned_crime_data <- cleaned_crime_data %>%
  filter(Census.Tract.2000 != "NULL")
cleaned_crime_data <- cleaned_crime_data %>%
       filter(Summary.Offense.Code != "X") # Filtering out records with invalid Summary Offense Codes

cleaned_crime_data$Occurred.Time <- substr(x=cleaned_crime_data$Occurred.Date.or.Date.Range.Start, 
                                                      nchar(as.character(cleaned_crime_data$Occurred.Date.or.Date.Range.Start)) - 10,
                                                      nchar(as.character(cleaned_crime_data$Occurred.Date.or.Date.Range.Start)))

# cleaned_crime_data$Occurred.Time <- strptime(cleaned_crime_data$Occurred.Time, "%H:%M:%S")

crime_count_offensecode <- cleaned_crime_data %>%
  group_by(Summary.Offense.Code) %>%
  summarise(count= n()) %>%
  arrange(desc(count))
# Counting the total number of crimes for each Summary Offense Code


crime_by_month<-cleaned_crime_data %>%
  group_by(Month) %>%
  summarise(count=n())
# Counting the total number of crimes according to the month


#Finding top three Offense using summary offense code occuring in Seattle in 2016
cleaned_crime_data %>%
  group_by(Summary.Offense.Code) %>%
  summarise(count_offense = n()) %>%
  arrange(desc(count_offense))


# Finding Min/max/mean/sd of top three types of Crimes

min.max.offense2300 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2300) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2300$count_minmax)
sd(min.max.offense2300$count_minmax)

min.max.offense2200 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2200) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2200$count_minmax)
sd(min.max.offense2200$count_minmax)

min.max.offense2400 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2400) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2400$count_minmax)
sd(min.max.offense2400$count_minmax)

#Plotting histogram for the top three crimes in Seattle for 2016

hist.code.2300 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2300)
hist(hist.code.2300$Month, xlab = "Months", main = "Histogram of Crime - ?? for year 2016")

hist.code.2200 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2200)
hist(hist.code.2200$Month, xlab = "Months", main = "Histogram of Crime - Burglary for year 2016")

hist.code.2400 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2400)
hist(hist.code.2400$Month, xlab = "Months", main = "Histogram of Crime - Theft for year 2016")




