Reproducible Research: Peer Assessment 1
========================================

Setting and getting the data
----------------------------

    setwd("C:/Users/saya/Desktop/DS/real kuiz/Reproducible Research/Week 2/Project")

    rawData <- read.csv("activity.csv")

What is mean total number of steps taken per day?
-------------------------------------------------

1.Calculate the total number of steps taken per day you can ignore the
missing values in the dataset.

    sum_data <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum, na.rm=TRUE)
    # Rename the attributes
    names(sum_data) <- c("date", "total")

1.  Make a histogram of the total number of steps taken each day.

<!-- -->

    hist(sum_data$total, 
         main="Total Step Taken Each Day",
         breaks=seq(from=0, to=25000, by=2500),
         xlab="Total Steps", 
         border="blue", 
         col="green",
         ylim=c(0,40))

![](PA1_template_files/figure/unnamed-chunk-2-1.png)
[plot of chunk unnamed-chunk-1](figure/unnamed-chunk-2-1.png)


3.Calculate and report the mean and median of the total number of steps
taken per day

    mean(sum_data$total)

    ## [1] 9354.23

    median(sum_data$total)

    ## [1] 10395

What is the average daily activity pattern?
-------------------------------------------

1.Make a time series plot (i.e. type = "l") of the 5-minute interval
(x-axis) and the average number of steps taken, averaged across all days
(y-axis)

    meanData <- aggregate(rawData$steps, by=list(rawData$interval), FUN=mean, na.rm=TRUE)

    # Rename the attributes
    names(meanData) <- c("interval", "mean")

    plot(meanData$interval, 
         meanData$mean,
         type = "l", 
         col="green", 
         lwd = 2,
         xlab="Intervel [minutes]", 
         ylab="Average num of Steps", 
         main="Time-series of the average number of steps per intervals")

![](PA1_template_files/figure/unnamed-chunk-4-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    #We find the position of the maximum mean
    maxPosition <- which(meanData$mean == max(meanData$mean))
    # We lookup the value of interval at this position
    maxInterval <- meanData[maxPosition, 1]

Imputing missing values
-----------------------

1.  Calculate and report the total number of missing values in the
    dataset (i.e. the total number of rows with NAs)

<!-- -->

    NA_count <- sum(is.na(rawData$steps))

1.  Devise a strategy for filling in all of the missing values in the
    dataset.

<!-- -->

    # Find the NA positions
    NApos <- which(is.na(rawData$steps))
    head(NApos)

    ## [1] 1 2 3 4 5 6

    # Create a vector of means
    meanVec <- rep(mean(rawData$steps, na.rm=TRUE), times=length(NApos))
    head(meanVec)

    ## [1] 37.3826 37.3826 37.3826 37.3826 37.3826 37.3826

1.  Create a new dataset that is equal to the original dataset but with
    the missing data filled in.

<!-- -->

    rawData[NApos, "steps"] <- meanVec #replace NA with mean value

1.  Make a histogram of the total number of steps taken each day

<!-- -->

    # Calculate and report the mean and median total number of steps taken per day
    sumData <- aggregate(rawData$steps, by=list(rawData$date), FUN=sum)
    names(sumData) <- c("date", "total") #rename x and y

    hist(sumData$total,
         breaks=seq(from=0, to=25000, by=2500),
         xlab="Total Steps", 
         border="blue", 
         col="green",
         ylim=c(0,40),
         main="Total number of steps taken each day")

![](PA1_template_files/figure/unnamed-chunk-9-1.png)

    mean(sumData$total)

    ## [1] 10766.19

    median(sumData$total)

    ## [1] 10766.19

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

1.Create a new factor variable in the dataset with two levels -
"weekday" and "weekend"

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

1.  Make a panel plot containing a time series plot (i.e. type = "l") of
    the 5-minute interval (x-axis) and the average number of steps
    taken, averaged across all weekday days or weekend days (y-axis).

<!-- -->

    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 3.2.5

    averages <- aggregate(steps ~ interval + day, data=rawData, mean)
    ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
      xlab("Interval") + ylab("Number of steps") 

![](PA1_template_files/figure/unnamed-chunk-11-1.png)
