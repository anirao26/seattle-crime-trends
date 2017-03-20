library(sp)
library(maps)
library(maptools)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggmap)
library(AUC)
library(randomForest)
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
