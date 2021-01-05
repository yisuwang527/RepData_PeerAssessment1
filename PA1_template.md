---
title: "Reproducible Research: Peer Assessment 1"
author: "Yisu Wang"
date: "Thursday, April 16, 2015"
output: 
  html_document:
    keep_md: true
---

## Loading and preprocessing the data
1. Load the data


```r
if(!file.exists("/repdata-data-activity")) 
  {
  fileURL <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileURL, destfile = "./repdata-data-activity.zip")
  file.name <- "./repdata-data-activity.zip"
  unzip(file.name)
  unlink(file.name)
}
```

2. Process/transform the data

```r
activity <- read.csv("./activity.csv", colClasses = c("numeric", "Date", "numeric"))
activity$day <- weekdays(activity$date)
```
## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day

```r
library(ggplot2)
activityData <- aggregate(steps ~ date, activity, sum, na.rm = TRUE)
qplot(steps, data = activityData, binwidth = 1400) 
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
2. Calculate and report the mean and median total number of steps taken per day.
mean steps per day is

```r
mean(activityData$steps)
```

```
## [1] 10766.19
```
median steps per day is

```r
median(activityData$steps)
```

```
## [1] 10765
```

## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
timeSeries <- aggregate(steps ~ interval, activity, mean, na.rm = TRUE)
ggplot(timeSeries, aes(x=interval,y=steps)) + 
geom_line(color="black",size=2) +  
labs(x="Interval",y="Average Number of Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
timeSeries[which.max(timeSeries$steps),]$interval
```

```
## [1] 835
```

## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Missing step values (NA) can be replaced by the mean number of steps taken for the corresponding time interval and weekday.


3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
avgStep <- aggregate(steps ~ interval + day, activity, mean, na.rm = TRUE)
imputeData <- merge(activity, avgStep, by=c("interval", "day"))
imputeData <- transform(imputeData, steps.x = ifelse(is.na(steps.x),steps.y,steps.x))
imputeData <- data.frame(imputeData[,1:4])
names(imputeData) <- c("interval", "day","steps", "date")
imputeData$steps <- round(imputeData$steps, digits = 0)
imputeData <- imputeData[order(imputeData$date, imputeData$interval),]
```
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
imputeDataAggregate <- aggregate(steps ~ date, imputeData, sum, na.rm = TRUE)
qplot(steps, data = imputeDataAggregate, binwidth = 1400) 
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

mean steps per day is

```r
mean(imputeDataAggregate$steps)
```

```
## [1] 10821.1
```
median steps per day is

```r
median(imputeDataAggregate$steps)
```

```
## [1] 11015
```


## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
imputeData$daytype <- ifelse(imputeData$day %in% c("Saturday", "Sunday"),"Weekend", "Weekday")
imputeDataDayAgg <- aggregate(steps ~ interval + daytype, imputeData, mean)
```
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:


```r
ggplot(imputeDataDayAgg, aes(x=interval,y=steps)) + 
geom_line(color="black",size=2) + 
facet_wrap(~daytype, nrow=2, ncol=1) + 
labs(x="Interval",y="Number of Steps") 
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
