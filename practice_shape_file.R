library(sp)
library(maps)
library(maptools)
library(rgdal)
library(dplyr)
crime_data <- read.csv("Data/Seattle_Police_Department_Police_Report_Incident.csv")
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

neighborhoods_map <- readOGR(dsn = "Neighborhoods", layer = "Neighborhoods")

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

zillow_housing_data <- read.csv("Data/seattle-wa.csv")

zillow_housing_data$Region.Name <- as.character(zillow_housing_data$Region.Name)
zillow_housing_data$Region.Name[which(zillow_housing_data$Region.Name == "Mt. Baker")] <- "Mount Baker"
zillow_housing_data$Region.Name[which(zillow_housing_data$Region.Name == "Ballard")] <- "Adams"


cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD %in% c("Broadway", "Stevens"))] <- "Capitol Hill"
cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD %in% c("Central Business District", "Pike-Market", "Yesler Terrace", 
                                                                 "Pioneer Square", "International District"))] <- "Downtown"
cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD %in% c("Atlantic","Harrison/Denny-Blaine","Mann"))] <- "Central"
cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD %in% c("Lawton Park","Briarcliff","Southeast Magnolia"))] <- "Magnolia"
cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD == "Mid-Beacon Hill")] <- "Beacon Hill"
cleaned_crime_data$S_HOOD[which(cleaned_crime_data$S_HOOD == "North Beach/Blue Ridge")] <- "North Beach"

cleaned_crime_data <- cleaned_crime_data %>%
                      rename(Neighborhood = S_HOOD)
zillow_housing_data <-  zillow_housing_data %>% 
  rename(Neighborhood = Region.Name)



crime_zillow_merged_df <- inner_join(x = cleaned_crime_data, y = zillow_housing_data, by = c("Neighborhood"))

crime_zillow_merged_df <- crime_zillow_merged_df %>%
                          select(Offense.Code, Offense.Type, Summary.Offense.Code, Summarized.Offense.Description,
                                 Occurred.Date.or.Date.Range.Start, Zone.Beat, Month, Year, Occurred.Time, 
                                 Neighborhood, Current)

crime_zillow_merged_df <- crime_zillow_merged_df %>%
                          rename(Current.Value = Current)

crime_zillow_merged_df$Current.Value <- as.numeric(gsub("\\,", "", 
                                                          gsub("\\$", "", 
                                                               crime_zillow_merged_df$Current.Value)), na.rm=TRUE)

#Removing NAs
crime_zillow_merged_df <- crime_zillow_merged_df %>%
                            filter(!is.na(crime_zillow_merged_df$Current.Value))

unique_merges <- data_frame(unique(crime_zillow_merged_df$Neighborhood))
property_value_by_neighborhood <- crime_zillow_merged_df %>%
                                  select(Neighborhood, Current.Value) %>%
                                  arrange(Neighborhood) %>%
                                  unique()

crime_by_neighborhood <- crime_zillow_merged_df %>%
  group_by(Neighborhood) %>%
  summarise(crime_count = n()) %>%
  arrange(Neighborhood)

library(AUC)
crime_zillow_merged_df$Indicator <- 0
crime_zillow_merged_df$Indicator[which(crime_zillow_merged_df$Summary.Offense.Code %in% c(2300))] <- 1


log.mod <- glm(formula = Indicator ~ Current.Value, family = "binomial", data = crime_zillow_merged_df)
summary(log.mod)

plot(y = crime_zillow_merged_df$Indicator, x = crime_zillow_merged_df$Current.Value)
fits <- fitted(log.mod)
curve(predict(log.mod,data.frame(Current.Value=x),type="resp"),add=TRUE)
points(crime_zillow_merged_df$Current.Value, fits, pch=20)

blah <- data.frame(Current.Value = c(5000000, 450000))

y <- factor(crime_zillow_merged_df$Indicator)
rr <- roc(fits, y)
plot(rr)
auc(rr)
#curve(predict(log.mod,data.frame(Current.Value=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
#points(crime_zillow_merged_df$Current.Value,fitted(log.mod),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.
