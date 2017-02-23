
#Temporary code to retrieve zip codes from Google API
library(ggmap)
library(sqldf)
library(ggplot2)
library(dplyr)
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


# The following code finds the total number of crimes pertaining to each Summary 
# Offense Code:
crime_count_offensecode <- cleaned_crime_data %>%
  group_by(Summary.Offense.Code) %>%
  summarise(count= n()) %>%
  arrange(desc(count))

print(crime_count_offensecode)
# We get 27 unique crimes as per the Summary Offense Code. The top 5 crimes according to the number of incidence rates are as follows: 
#1  2300 21254
#2  2200 10374
#3  2400  7054
#4  1300  5758

# The top 5 crimes have been committed 48127 times which is roughly thrice the number of incidences of the remaining 22 crimes, 
# which total 16194.


# The following code finds the total number of crimes per month:
crime_by_month<-cleaned_crime_data %>%
  group_by(Month) %>%
  summarise(count=n())

print(crime_by_month)

# November has the highest number of crimes: 14762 whereas, January has the lowest number of crimes: 248. From the month-wise 
# crime distribution, we can see that there is a drastic change in the number of crimes from August to September. It's apparent
# that the number of crimes increase towards the end of the year, with a peak in November.

# The following code finds the total number of crimes in each zone:
crime_by_zone<-cleaned_crime_data %>%
  group_by(Zone.Beat) %>%
  summarise(count=n()) %>%
  arrange(desc(count))

print(crime_by_zone)
# We find that Zone M2 has the highest occurences of crimes(2427) and Zone 99 has the lowest number of crimes(1)


# Finding Min/max/mean/sd of top three types of Crimes

min.max.offense2300 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2300) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2300$count_minmax)
# In 2016, 1771.167 thefts including car prowl, shoplift, building, etc. occurred in Seattle per month. 
sd(min.max.offense2300$count_minmax)

min.max.offense2200 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2200) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2200$count_minmax)
# In 2016, 864.5 burglaries occurred in Seattle per month. 
sd(min.max.offense2200$count_minmax)

min.max.offense2400 <- cleaned_crime_data %>%
  group_by(Month) %>%
  filter(Summary.Offense.Code == 2400) %>%
  summarise(count_minmax = n()) %>%
  arrange(desc(count_minmax))

mean(min.max.offense2400$count_minmax)
# In 2016, 587.8 vehicle thefts occurred in Seattle per month. 
sd(min.max.offense2400$count_minmax)

#Plotting histogram for the top three crimes in Seattle for 2016

crime.data.2300 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2300)
hist(crime.data.2300$Month, xlab = "Months", main = "Histogram of Crime - Theft (Car prowl, shoplift, building, etc.) for year 2016")

plot(y = min.max.offense2300$count_minmax, x = min.max.offense2300$Month, xlab = "Month", ylab = "Count of Thefts", 
     main = "Number of Thefts vs Month")
mod.2300.month <- lm(formula = min.max.offense2300$count_minmax ~ min.max.offense2300$Month, data = min.max.offense2300)
summary(mod.2300.month)
# p-value = 0.00031 implies statistical significance

crime.data.2200 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2200)
hist(crime.data.2200$Month, xlab = "Months", main = "Histogram of Crime - Burglary for year 2016")

plot(y = min.max.offense2200$count_minmax, x = min.max.offense2200$Month, xlab = "Month", ylab = "Count of Burglaries", 
     main = "Number of Burglaries vs Month")
mod.2200.month <- lm(formula = min.max.offense2200$count_minmax ~ min.max.offense2200$Month, data = min.max.offense2200)
summary(mod.2200.month)
# p-value = 0.000323 implies statistical significance


crime.data.2400 <- cleaned_crime_data %>%
  filter(Summary.Offense.Code == 2400)
hist(crime.data.2400$Month, xlab = "Months", main = "Histogram of Crime - Vehicle Theft for year 2016")

plot(y = min.max.offense2400$count_minmax, x = min.max.offense2400$Month, xlab = "Month", ylab = "Count of Vehicle Thefts", 
     main = "Number of Vehicle Thefts vs Month")
mod.2400.month <- lm(formula = min.max.offense2400$count_minmax ~ min.max.offense2400$Month, data = min.max.offense2400)
summary(mod.2400.month)
# p-value = 0.000527 implies statistical significance



#Zone
ggplot(data=cleaned_crime_data, aes(cleaned_crime_data$Zone.Beat)) + geom_histogram(stat = "count")

mean(crime_by_zone$count)
sd(crime_by_zone$count)

time_zone_data <- read.csv("timeZones.csv")

#Time of the day
formatted_times <- format(strptime(cleaned_crime_data$Occurred.Time, "%I:%M:%S %p"), format = "%H:%M:%S")
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

ggplot(data=time_join_df_clean, aes(periodId)) + geom_histogram()

time_wise_crime_count <- time_join_df_clean %>%
  group_by(periodId) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  inner_join(time_zone_data, by = "periodId")

mean(time_wise_crime_count$count)
sd(time_wise_crime_count$count)
plot(y = time_wise_crime_count$count, x= time_wise_crime_count$periodId, xlab = "Time of day",
     ylab = "Number of criminal activities", main = "Number of criminal activities vs time of the day")
mod.crime.time <- lm (formula = time_wise_crime_count$count ~ time_wise_crime_count$periodId, 
                      data = time_wise_crime_count)
summary(mod.crime.time)
# Most of the criminal activities occur during the latter half of the day (12 p.m. to around 2 a.m)
# p-value = 0.00618 implies statistical significance
