# RepData_PeerAssessment1

## Loading and preprocessing the data

1. Load the data

```
data <- read.csv("activity.csv", header = TRUE, sep = ",", na.strings = "NA")
```

2. Process/transform the data (if necessary) into a format suitable for your analysis

```
data$date <- as.Date(data$date, format = "%Y-%m-%d")
data$interval <- factor(data$interval)

na_index <- is.na(as.character(data$steps))
data_no_na <- data[!na_index,]

colnames(steps_each_day) <- c("date", "steps")
```

## What is mean total number of steps taken per day?

1. Calculate the total number of steps taken per day

```
steps_each_day <- aggregate(steps ~ date, data = data_no_na, sum)
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

```
hist(as.numeric(steps_each_day$steps), breaks = 20, col = "green", xlab = "Number of Steps", main= "Histogram of the Total Number of Steps Taken Each Day")
```

3. Calculate and report the mean and median of the total number of steps taken per day

```
#Output mean of the total number of steps taken per day
mean(steps_each_day$steps)

#Output median of the total number of steps taken per day
median(steps_each_day$steps)
```

## What is the average daily activity pattern?

```
steps_per_interval <- aggregate(data_no_na$steps, by=list(interval=data_no_na$interval), FUN=mean)

colnames(steps_per_interval) <- c("interval", "average_steps")
```

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```
#Plot average daily activity pattern using type "l"
plot(as.integer(levels(steps_per_interval$interval)), steps_per_interval$average_steps, type="l",
     xlab = "Interval", ylab = "Average Number of Steps", main = "Average Daily Activity Pattern",  col ="blue")
```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```
max_steps <- max(steps_per_interval$average_steps)
max_steps

interval_max_steps<-steps_per_interval[which.max(steps_per_interval$average_steps),]$interval
interval_max_steps
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```
sum(is.na(as.character(data$steps)))
sum(is.na(as.character(data$date)))
sum(is.na(as.character(data$interval)))

na_index <- which(is.na(as.character(data$steps)))
complete_data <- data
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```
complete_data[NA_index, ]$steps<-unlist(lapply(na_index, FUN=function(na_index){
  steps_per_interval[data[na_index,]$interval==steps_per_interval$interval,]$average_steps
}))
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```
steps_each_day_complete <- aggregate(steps ~ date, data = complete_data, sum)

colnames(steps_each_day_complete) <- c("date", "steps")
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```
hist(as.numeric(steps_each_day_complete$steps), breaks = 20, col = "green", xlab = "Number of Steps", main= "Histogram of the Total Number of Steps Taken Each Day")

mean(steps_each_day_complete$steps)

median(steps_each_day_complete$steps)
```

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```
#Create factor variable (day) to store day of the week
complete_data$day <- as.factor(weekdays(complete_data$date))

#Create logical variable (is_weekday) (weekday = TRUE, weekend = FALE) :
complete_data$is_weekday <- ifelse(!(complete_data$day %in% c("Saturday","Sunday")), TRUE, FALSE)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.

```
weekdays_data <- complete_data[complete_data$is_weekday,]
steps_per_interval_weekdays <- aggregate(weekdays_data$steps, by=list(interval=weekdays_data$interval), FUN=mean)

weekends_data <- complete_data[!complete_data$is_weekday,]
steps_per_interval_weekends <- aggregate(weekends_data$steps, by=list(interval=weekends_data$interval), FUN=mean)

colnames(steps_per_interval_weekdays) <- c("interval", "average_steps")
colnames(steps_per_interval_weekends) <- c("interval", "average_steps")

steps_per_interval_weekdays$day <- "Weekday"
steps_per_interval_weekends$day <- "Weekend"

week_data <- rbind(steps_per_interval_weekends, steps_per_interval_weekdays)

week_data$day <- as.factor(week_data$day)

xyplot(average_steps ~  interval | day, data = week_data, layout = c(1,2), type ="l", ylab="Number of Steps", xlab="Interval")
```
