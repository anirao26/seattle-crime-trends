
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

cleaned_crime_data$Occurred.Time <- substr(x=cleaned_crime_data$Occurred.Date.or.Date.Range.Start, 
                                                      nchar(as.character(cleaned_crime_data$Occurred.Date.or.Date.Range.Start)) - 10,
                                                      nchar(as.character(cleaned_crime_data$Occurred.Date.or.Date.Range.Start)))

# cleaned_crime_data$Occurred.Time <- strptime(cleaned_crime_data$Occurred.Time, "%H:%M:%S")
