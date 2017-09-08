# Reproducible Research: Peer Assessment 1



## Loading and preprocessing the data

Show any code that is needed to:

1. Load the data:


```r
if(!file.exists("activity.csv")){
  unzip("activity.zip")
}
activity <- read.csv("activity.csv")
head(activity)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

```r
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

2. Process/transform the data (if necessary) into a format suitable for your analysis:


```r
activity$date <- as.Date(activity$date)
str(activity)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

## What is mean total number of steps taken per day?

1. (Load all necessary packages and) Calculate the total number of steps taken per day:


```r
library(dplyr, warn.conflicts = FALSE)
library(ggplot2)
library(knitr)
activity2 <- na.omit(activity)
total_step <- aggregate(steps ~ date, data = activity2, sum, na.rm = TRUE)
by_day <- group_by(total_step, date)
steps_by_day <- summarise(by_day, total = sum(steps))
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day:


```r
hist(total_step$steps, breaks = 5, col= 2, main="Histogram of total number of steps per day", xlab="Total number of steps per day")
```

![](PA1_template_files/figure-html/histogram-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day:


```r
mean(total_step$steps)
```

```
## [1] 10766.19
```

```r
median(total_step$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
avg_step <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)
plot(avg_step$interval, avg_step$steps, type = "l", col = "red",
     main = "Time Series: Average number of steps per day",
     xlab = "5-minute interval", ylab = "Average number of steps")
axis(1)
axis(2, las = 3)
```

![](PA1_template_files/figure-html/timeseries-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
max_avg_step <- which.max(avg_step$steps)
avg_step [max_avg_step, ]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs):


```r
sum(is.na(activity))
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.:

I use the mean of 5-minute interval to fill in the values of the missing values.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in:


```r
new_set <- activity
for (i in avg_step$interval) {
    new_set[new_set$interval == i & is.na(new_set$steps), ]$steps <- 
        avg_step$steps[avg_step$interval == i]
}
sum(is.na(new_set))
```

```
## [1] 0
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
total_step_new_set <- aggregate(steps ~ date, data = new_set, sum, na.rm = TRUE)
hist(total_step_new_set$steps, breaks = 5, 
     main = "Total number of steps per day (imputed)",
     col = "blue", xlab = "Steps")
```

![](PA1_template_files/figure-html/hist2-1.png)<!-- -->


```r
mean(total_step_new_set$steps)
```

```
## [1] 10766.19
```

```r
median(total_step_new_set$steps)
```

```
## [1] 10766.19
```

The mean is the same as the mean from the first part of the assignment, but the median is not. Imputing missing data using the average of the 5-minute interval results in more data points equal to the mean. Since many data points now have the same values as the mean, the median and the mean are equal.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day:


```r
new_set$dayType<- ifelse(as.POSIXlt(new_set$date)$wday %in% c(0,6), "weekends","weekdays")
aggregate_data <- aggregate(steps ~ interval + dayType, data=new_set, mean)
```

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis):


```r
ggplot(aggregate_data, aes(interval, steps)) + 
    geom_line() + 
    facet_grid(dayType ~ .) +
    xlab("5-minute interval") + 
    ylab("Avarage number of steps")
```

![](PA1_template_files/figure-html/WW2-1.png)<!-- -->
