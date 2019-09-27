library(lattice)

setwd("C:/Users/christopher.e.martin/Desktop")

#Read in data
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")

#Format date accordingly
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

na_index <- is.na(as.character(data$steps))
data_no_na <- data[!na_index,]

#Create data frame with steps taken each day
steps_each_day <- aggregate(steps ~ date, data = data_no_na, sum)

#Add column names to data frame
colnames(steps_each_day) <- c("date", "steps")

#Display histogram of Number of Steps Taken Each Day
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "green", xlab = "Number of Steps", main= "Histogram of the Total Number of Steps Taken Each Day")

#Output mean of the total number of steps taken per day
mean(steps_each_day$steps)

#Output median of the total number of steps taken per day
median(steps_each_day$steps)

#Calculate the average
steps_per_interval <- aggregate(data_no_na$steps, by=list(interval=data_no_na$interval), FUN=mean)

#Add column names
colnames(steps_per_interval) <- c("interval", "average_steps")

#Plot average daily activity pattern using type "l"
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")

#The maximum number of average steps
max_steps <- max(steps_per_interval$average_steps)
max_steps

#Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
intervale_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
intervale_max_steps

#Calculate and report the total number of missing values in the dataset
sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))
sum(is.na(as.character(data$interval)))

#Find the indexes of missing values
na_index <- which(is.na(as.character(data$steps)))
complete_data <- data

#Impute missing values using the mean for that 5-minute interval
complete_data[NA_index, ]$steps<-unlist(lapply(na_index, FUN=function(na_index){
  steps_per_interval[data[na_index,]$interval==steps_per_interval$interval,]$average_steps
}))

#Create new data frame with steps taken for each day
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)

#Add column names to data frame
colnames(steps_each_day_complete) <- c("date", "steps")

#Make histogram of the total number of steps taken each day
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "green", xlab = "Number of Steps", main= "Histogram of the Total Number of Steps Taken Each Day")

#Output mean total number of steps taken per day
mean(steps_each_day_complete$steps)

#Output median total number of steps taken per day
median(steps_each_day_complete$steps)

#Create factor variable (day) to store day of the week
complete_data$day <- as.factor(weekdays(complete_data$date))

#Create logical variable (is_weekday) (weekday = TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE) 


#Calculate average number of steps for weekdays
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)


#Calculate average number of steps for weekends
weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

#Add column names
colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

#Add a column that will indicate the day
steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

#Merge weekday and weekend data together
week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)

#Convert day variable to a factor
week_data$day <- as.factor(week_data$day)

#Create the plot (type "l")
xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps", xlab="Interval")
