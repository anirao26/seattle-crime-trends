library(sp)
library(maps)
library(maptools)
library(rgdal)
library(dplyr)
crime_data <- read.csv("seattle-crime-trends/Seattle_Police_Department_Police_Report_Incident.csv")
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

neighborhoods_map <- readOGR(dsn = "Neighborhoods/StatePlane", layer = "Neighborhoods")

#Converting columns Longitude and Latitude into spatial coordinates.
coordinates(cleaned_crime_data) <- ~Longitude+Latitude
#spplot(seattle_crime_data)

#Function to set the co-ordinate system
proj4string(cleaned_crime_data)<-CRS("+proj=longlat +datum=NAD83")
cleaned_crime_data <- spTransform(cleaned_crime_data, CRS(proj4string(neighborhoods_map)))

neighborhood_details <- over(cleaned_crime_data, neighborhoods_map)

cleaned_crime_data$L_HOOD <- neighborhood_details$L_HOOD
cleaned_crime_data$S_HOOD <- neighborhood_details$S_HOOD

cleaned_crime_data <- as.data.frame(cleaned_crime_data)

cleaned_crime_data <- cleaned_crime_data %>%
  filter(cleaned_crime_data$L_HOOD != "NA")

typeof(cleaned_crime_data$L_HOOD)

cleaned_crime_data$L_HOOD <- as.character(cleaned_crime_data$L_HOOD)
cleaned_crime_data$S_HOOD <- as.character(cleaned_crime_data$S_HOOD)

cleaned_crime_data[cleaned_crime_data$L_HOOD == "NO BROADER TERM",]$L_HOOD <- 
  toupper(cleaned_crime_data[cleaned_crime_data$L_HOOD == "NO BROADER TERM",]$S_HOOD)

unique(cleaned_crime_data$L_HOOD)
crime_by_neighborhood <- cleaned_crime_data %>%
  group_by(L_HOOD) %>%
  summarise(crime_count = n()) %>%
  arrange(desc(crime_count))

