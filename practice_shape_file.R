library(sp)
library(maps)
library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggmap)
library(AUC)
library(randomForest)
library(sqldf)
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

cleaned_crime_data$Long <- cleaned_crime_data$Longitude
cleaned_crime_data$Lat <- cleaned_crime_data$Latitude

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
         Occurred.Date.or.Date.Range.Start, Zone.Beat, Location, Month, Year, Occurred.Time, 
         Neighborhood, Current, Long, Lat)

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

#Analysis of the presence of schools on the incidence of crimes using Linear regression--------

#Reading school data
school_data <- read.csv("Data/Public_Schools.csv")

#transforming the shape variable into coordinates into separate columns (Latitude, Longitude)
school_lat_lon_coord_obj <- read.table(text=gsub('[()]', '', school_data$Shape), 
                                       sep=",", col.names=c('Latitude', 'Longitude'))

school_lat_lon_coord_obj$OBJECTID <- school_data$OBJECTID
school_data <- merge(school_data, school_lat_lon_coord_obj, by = "OBJECTID")

#converting to a spatial dataframe
coordinates(school_data) <- ~Longitude+Latitude

proj4string(school_data)<-CRS("+proj=longlat +datum=NAD83")
school_data <- spTransform(school_data, CRS(proj4string(neighborhoods_map)))

#Projecting onto shape file
neighborhood_details <- over(school_data, neighborhoods_map)

#Adding columns consistent with the shape file details
school_data$L_HOOD <- neighborhood_details$L_HOOD
school_data$S_HOOD <- neighborhood_details$S_HOOD
school_data <- as.data.frame(school_data)


school_data <- school_data %>%
  rename(Neighborhood = S_HOOD)
unique(school_data$Neighborhood)

school_data$Neighborhood <- as.character(school_data$Neighborhood)

#Checking for discrepancy. Cleaning up neighborhood data
school_data$Neighborhood[which(school_data$Neighborhood %in% c("Broadway", "Stevens"))] <- "Capitol Hill"
school_data$Neighborhood[which(school_data$Neighborhood %in% c("Central Business District", "Pike-Market", "Yesler Terrace", 
                                                               "Pioneer Square", "International District"))] <- "Downtown"
school_data$Neighborhood[which(school_data$Neighborhood %in% c("Atlantic","Harrison/Denny-Blaine","Mann"))] <- "Central"
school_data$Neighborhood[which(school_data$Neighborhood %in% c("Lawton Park","Briarcliff","Southeast Magnolia"))] <- "Magnolia"
school_data$Neighborhood[which(school_data$Neighborhood == "Mid-Beacon Hill")] <- "Beacon Hill"
school_data$Neighborhood[which(school_data$Neighborhood == "North Beach/Blue Ridge")] <- "North Beach"

#Joining school data and crime data by neighborhood
crime_by_school_area <- inner_join(x= school_data, y = cleaned_crime_data, by = c("Neighborhood"))

#Joining above dataset with the Zillow property values dataset
property_value_by_school_area <- inner_join(x= crime_by_school_area,y = property_value_by_neighborhood, by = c("Neighborhood"))

property_value_by_school_area$Neighborhood

area_wise_property <- property_value_by_school_area %>%
  group_by(Neighborhood) %>%
  select(Current.Value, Neighborhood) %>%
  unique() %>%
  arrange(Neighborhood)

school_per_area <- school_data %>%
  group_by(Neighborhood) %>%
  summarise(count_school = n())

area_wise_school_crime <- crime_by_school_area %>%
  group_by(Neighborhood) %>%
  summarise(count_crime = n()) 

#Checking for mismatches in neighborhood wise school count and crime count
anti_join(school_per_area, area_wise_school_crime, by = c("Neighborhood"))

#merging
area_wise_school_crime$Total_Schools <- school_per_area$count_school

#Repeating the merge process for the top 3 crimes 
area_wise_school_crime_2300 <- crime_by_school_area %>%
  filter(Summary.Offense.Code == 2300) %>%
  group_by(Neighborhood) %>%
  summarise(count_crime = n()) 
area_wise_school_crime_2300$Total_Schools <- school_per_area$count_school

area_wise_school_crime_2200 <- crime_by_school_area %>%
  filter(Summary.Offense.Code == 2200) %>%
  group_by(Neighborhood) %>%
  summarise(count_crime = n()) 
area_wise_school_crime_2200$Total_Schools <- school_per_area$count_school

area_wise_school_crime_2400 <- crime_by_school_area %>%
  filter(Summary.Offense.Code == 2400) %>%
  group_by(Neighborhood) %>%
  summarise(count_crime = n()) 
area_wise_school_crime_2400$Total_Schools <- school_per_area$count_school


#Building the linear regression models for all crimes and school count by neighborhoods
mod <- lm(area_wise_school_crime$count_crime ~ area_wise_school_crime$Total_Schools)

#Checking for statistical significance
summary(mod)

#Visualizing the linear relationship
ggplot(area_wise_school_crime, aes(x = Total_Schools, y = count_crime)) + 
  geom_point() +
  stat_smooth(method = "lm", col = "blue") +
  xlab("Total number of schools") +
  ylab("Total number of crimes") +
  ggtitle("Number of schools vs Total crimes")

#Building the linear regression models for top 3 crimes and school count by neighborhoods

mod_2200 <- lm(area_wise_school_crime_2200$count_crime ~ area_wise_school_crime_2200$Total_Schools)
summary(mod_2200)

mod_2300 <- lm(area_wise_school_crime_2300$count_crime ~ area_wise_school_crime_2300$Total_Schools)
summary(mod_2300)

mod_2400 <- lm(area_wise_school_crime_2400$count_crime ~ area_wise_school_crime_2400$Total_Schools)
summary(mod_2400)


#Multiple regression model

#Checking for mismatches in neighborhood names
anti_join(area_wise_school_crime, area_wise_property, by = c("Neighborhood"))

#Analyzing only matched neighborhoods

area_wise_school_crime_ml <- crime_by_school_area %>%
  filter(!(Neighborhood %in% c("Industrial District", "North Admiral"))) %>%
  group_by(Neighborhood) %>%
  summarise(count_crime = n())

school_per_area_ml <- school_data %>%
  filter(!is.na(school_data$Neighborhood) & !(Neighborhood %in% c("Industrial District", "North Admiral"))) %>%
  group_by(Neighborhood) %>%
  summarise(count_school = n())

ml_school_propertyValueObj <- inner_join(x= area_wise_school_crime_ml, y = area_wise_property, by = c("Neighborhood"))
ml_school_propertyValueObj <- inner_join(x= ml_school_propertyValueObj,y = school_per_area_ml, by = c("Neighborhood"))

#Converting the property values from charaters to numbers
ml_school_propertyValueObj$Current <- as.numeric(gsub("\\,", "", gsub("\\$", "", ml_school_propertyValueObj$Current.Value)), na.rm=TRUE)

#Building the ML model with property values
mod_property_crime_school <- lm(ml_school_propertyValueObj$count_crime ~ ml_school_propertyValueObj$Current + ml_school_propertyValueObj$count_school)

#Checking for statistical significance
summary(mod_property_crime_school)

#----------------------------------------End of School data analysis-------------------------------------

# Reading a new csv file which contains hourly timeslots, such as 3PM-4PM, 4PM-5PM etc. 
time_zone_data <- read.csv("timeZones2.csv")
formatted_times <- format(strptime(crime_zillow_merged_df$Occurred.Time, "%I:%M:%S %p"), format = "%H:%M:%S")
df <- as.data.frame(formatted_times)
df$formatted_times_numeric <- as.numeric(as.POSIXct(formatted_times, format = "%H:%M:%S"))
time_zone_data$start_time <- as.POSIXct(as.character(time_zone_data$start_time), format = "%H:%M:%S")
time_zone_data$end_time <- as.POSIXct(as.character(time_zone_data$end_time), format = "%H:%M:%S")

time_zone_data$start_time_numeric <- as.numeric(time_zone_data$start_time)
time_zone_data$end_time_numeric <- as.numeric(time_zone_data$end_time)

time_join_df <- sqldf("select * from df
                      inner join time_zone_data 
                      on df.formatted_times_numeric >= time_zone_data.start_time_numeric
                      and df.formatted_times_numeric < time_zone_data.end_time_numeric")

time_join_df_clean <- time_join_df %>%
  select(formatted_times, periodId, start_time, end_time, Desc)

#creating a new column in the dataframe that contains the hourly timeslots
crime_zillow_merged_df$time_slots<-time_join_df_clean$Desc

#Creating a new column in the dataframe, such that if an Assault occurred, a 1 is entered in the column for that particular record,
# and a 0 is entered for all other forms of crime.
crime_zillow_merged_df$Crime.Response <- rep(0, nrow(crime_zillow_merged_df))
crime_zillow_merged_df$Crime.Response[crime_zillow_merged_df$Summary.Offense.Code %in% c(1300)] <- 1 #If the Summary.Offense.Code is 
#1300, which stands for Assault, the Crime.Response column value will be 1.

logmod<-glm(formula = crime_zillow_merged_df$Crime.Response ~ crime_zillow_merged_df$time_slots,  
            data=crime_zillow_merged_df, family="binomial") #The logistic regression model
summary(logmod)
fits_assault<-fitted(logmod)
tab <- table(crime_zillow_merged_df$Crime.Response, fits_assault>=0.50) #creating the confusion matrix
tab
y <- factor(crime_zillow_merged_df$Crime.Response) 
rr_assault <- roc(fits_assault, y) #The ROC curve
plot(rr_assault) #Plotting the ROC curve
auc(rr_assault) #The Area Under the Curve value

#Multiple Logistic Regression model by considering an additional predictor - Neighborhood
logmod_new<-glm(formula = crime_zillow_merged_df$Crime.Response ~ crime_zillow_merged_df$time_slots + crime_zillow_merged_df$Neighborhood,  
                data=crime_zillow_merged_df, family="binomial")

summary(logmod_new)
fits_new<-fitted(logmod_new)
tab <- table(crime_zillow_merged_df$Crime.Response, fits_new>=0.50) 
tab # Gives the confusion matrix
y <- factor(crime_zillow_merged_df$Crime.Response) 
rr_new <- roc(fits_new, y) # The ROC curve
plot(rr_new) 
auc(rr_new) #The area under the ROC curve


crime_zillow_merged_df$Indicator <- 0
crime_zillow_merged_df$Indicator[which(crime_zillow_merged_df$Summary.Offense.Code %in% c(2300, 2200, 2400))] <- 1
set.seed(2)
train <- sample(1:nrow(crime_zillow_merged_df), 3/4 * nrow(crime_zillow_merged_df))
test <- -train
training_data <- crime_zillow_merged_df[train,]
testing_data <- crime_zillow_merged_df[test,]
log.mod <- glm(formula = Indicator ~ Current.Value, family = "binomial", data = training_data)
summary(log.mod)

predicted_Prob <- predict(log.mod, testing_data, type = "response")
predictedIndicators = rep(0, nrow(testing_data))
predictedIndicators[predicted_Prob > 0.60] <- 1
table(predicted = predictedIndicators, actual = testing_data$Indicator)
mean(predictedIndicators==testing_data$Indicator)

rm(predicted_Prob)
rm(predictedIndicators)

y <- factor(training_data$Indicator)
fits <- fitted(log.mod)
rr <- roc(fits, factor(training_data$Indicator))
plot(rr)
auc(rr)




log.mod <- glm(formula = Indicator ~ Current.Value, family = "binomial", 
               data = crime_zillow_merged_df)
summary(log.mod)

plot(y = crime_zillow_merged_df$Indicator, x = crime_zillow_merged_df$Current.Value)
fits <- fitted(log.mod)
curve(predict(log.mod,data.frame(Current.Value=x),type="resp"),add=TRUE)
points(crime_zillow_merged_df$Current.Value, fits, pch=20)

blah <- data.frame(Current.Value = c(5000000, 450000))

y <- factor(crime_zillow_merged_df$Indicator)
fits <- fitted(log.mod)
rr <- roc(fits, y)
plot(rr)
auc(rr)
#curve(predict(log.mod,data.frame(Current.Value=x),type="resp"),add=TRUE) # draws a curve based on prediction from logistic regression model
#points(crime_zillow_merged_df$Current.Value,fitted(log.mod),pch=20) # optional: you could skip this draws an invisible set of points of body size survival based on a 'fit' to glm model. pch= changes type of dots.

crime_zillow_merged_df$Property.Status <- "Less than 550k"
crime_zillow_merged_df$Property.Status[which(crime_zillow_merged_df$Current.Value >= 550000)] <- "Greater than 550k"

barplot(table(crime_zillow_merged_df$Property.Status))
ggplot(data = crime_zillow_merged_df) + geom_bar(mapping = aes(Property.Status)) +
  ggtitle("Crime across the two property value categories") +
  xlab("Property Value Categories") +
  ylab("Crime Count")

#Random Forest Implementation
rf_model <- randomForest(Indicator~Current.Value, ntree=20, training_data)
predicted_Prob <- predict(rf_model, testing_data, type = "response")
predictedIndicators = rep(0, nrow(testing_data))
predictedIndicators[predicted_Prob > 0.60] <- 1
confusion_matrix <- table(predictions = predictedIndicators, actual = testing_data$Indicator)
mean(predictedIndicators==testing_data$Indicator)
rm(predicted_Prob)
rm(predictedIndicators)

fits <- fitted(rf_model)
rr <- roc(fits, factor(training_data$Indicator))
plot(rr)
auc(rr)

importance(rf_model)
varImpPlot(rf_model)


top3_crimes_df <- crime_zillow_merged_df %>%
  filter(Summary.Offense.Code %in% c(2200, 2300, 2400))

map.seattle_city <- get_map('Seattle', zoom=11, maptype = "roadmap", crop = FALSE)
ggmap(map.seattle_city) +
  geom_point(data=top3_crimes_df, aes(x=Long, y=Lat),  color="red", alpha=.03, size=1., pch=21) +
  ggtitle("Distribution of burglary, theft, and vehicle theft") +
  xlab("Longitude") +
  ylab("Latitude")

high_property_areas <- crime_zillow_merged_df %>%
  filter(Property.Status == "Greater than 550k")
ggmap(map.seattle_city) +
  geom_point(data=high_property_areas, aes(x=Long, y=Lat),  color="red", alpha=.03, size=1., pch=21) +
  ggtitle("Neighborhoods with property value greater than $550000") +
  xlab("Longitude") +
  ylab("Latitude")
