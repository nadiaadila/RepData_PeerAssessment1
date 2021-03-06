setwd("C:/Users/saya/Desktop/DS/real kuiz/Reproducible Research/Week 2/Project")

rawData <- read.csv("activity.csv")
rawData$date <- as.POSIXct(rawData$date, format="%Y-%m-%d")
head(rawData)

##What is mean total number of steps taken per day?
#1.Calculate the total number of steps taken per day
#you can ignore the missing values in the dataset.
sum_data <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum, na.rm=TRUE)

# Rename the attributes
names(sum_data) <- c("date", "total")
head(sum_data)

#2. Make a histogram of the total number of steps taken each day.  

hist(sum_data$total, 
     main="Total Step Taken Each Day",
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40))

#3.Calculate and report the mean and median of the total number of steps taken per day
mean(sum_data$total)
median(sum_data$total)

##What is the average daily activity pattern?
#1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) 
#  and the average number of steps taken, averaged across all days (y-axis)
meanData <- aggregate(rawData$steps, by=list(rawData$interval), FUN=mean, na.rm=TRUE)

# Rename the attributes
names(meanData) <- c("interval", "mean")
head(meanData)

plot(meanData$interval, 
     meanData$mean,
     type = "l", 
     col="green", 
     lwd = 2,
     xlab="Intervel [minutes]", 
     ylab="Average num of Steps", 
     main="Time-series of the average number of steps per intervals")

#2. Which 5-minute interval, on average across all the days in the dataset, 
#   contains the maximum number of steps?
# We find the position of the maximum mean
maxPosition <- which(meanData$mean == max(meanData$mean))
# We lookup the value of interval at this position
maxInterval <- meanData[maxPosition, 1]

##Imputing missing values
#1. Calculate and report the total number of missing values in the dataset 
#   (i.e. the total number of rows with NAs)
NA_count <- sum(is.na(rawData$steps))

#2. Devise a strategy for filling in all of the missing values in the dataset.
# Find the NA positions
NApos <- which(is.na(rawData$steps))
head(NApos)

# Create a vector of means
meanVec <- rep(mean(rawData$steps, na.rm=TRUE), times=length(NApos))
head(meanVec)

#3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
rawData[NApos, "steps"] <- meanVec #replace NA with mean value

#4. Make a histogram of the total number of steps taken each day
#   Calculate and report the mean and median total number of steps taken per day
sumData <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum)
names(sumData) <- c("date", "total") #rename x and y

hist(sumData$total,
     breaks=seq(from=0, to=25000, by=2500),
     xlab="Total Steps", 
     border="blue", 
     col="green",
     ylim=c(0,40),
     main="Total number of steps taken each day")

mean(sumData$total)
median(sumData$total)

## Are there differences in activity patterns between weekdays and weekends?
#1.Create a new factor variable in the dataset with two levels - "weekday" and "weekend" 
#  indicating whether a given date is a weekday or weekend day.

weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}

rawData$date <- as.Date(rawData$date)
rawData$day <- sapply(rawData$date, FUN=weekday.or.weekend)


#2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval 
#  (x-axis) and the average number of steps taken, averaged across 
# all weekday days or weekend days (y-axis).

library(ggplot2)

averages <- aggregate(steps ~ interval + day, data=rawData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("Interval") + ylab("Number of steps") 

