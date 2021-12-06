---
title: "Reproducible Research. Course project1"
author: "Nadine.solov"
date: "04 12 2021"
output: 
  html_document: 
    keep_md: yes
---

## Loading and preprocessing the data

```r
data <- read.csv(unz("repdata_data_activity (3).zip","activity.csv"),
                 header=TRUE,
                 na.strings="NA",
                 colClasses=c("numeric","character","numeric"))
data$DateTime <- strptime(paste(data$date, sprintf("%04d",data$interval), sep=" "),
                          format="%Y-%m-%d %H%M")
```


## What is mean total number of steps taken per day?

The histogram shown below is the number of steps taken daily from the dataset

```r
stepsPerDay <- aggregate(steps~date,data,FUN=sum)
hist(stepsPerDay$steps, xlab="Steps Per Day", breaks=15, main="Histogram of Steps Per Day")
```

![](PA1_template_files/figure-html/Histogram-1.png)<!-- -->

```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
The plot shown below is the average daily activity of the person, this isin 5 minute intervals

```r
IntSteps <- aggregate(steps~interval,data,FUN=mean)
plot(IntSteps$interval,IntSteps$steps,
     type="l",
     xlab="Interval",
     ylab="Mean number of steps")
```

![](PA1_template_files/figure-html/Daily_average-1.png)<!-- -->
Find the interval with the maximum number of average steps

```r
IntSteps[IntSteps$steps == max(IntSteps$steps),]$interval
```

```
## [1] 835
```


## Imputing missing values
To imput missing values in the dataset, "NA" values for the number of steps within a 5-minute interval were replaced with the average number of steps.

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
We want to replace the NA values with the mean for that interval, which we calculated above

```r
noData <- data[is.na(data$steps),]
replacementData <- IntSteps[IntSteps$interval == noData$interval,]
data$steps <- replace(data$steps,
                      is.na(data$steps), 
                      replacementData$steps)
```
The revised dataset containing the imputed values was then used to calculate daily totals.  The histogram below shows the frequency of various values of total daily steps across the revised dataset.

```r
newStepsPerDay <- aggregate(steps~date,data,FUN=sum)
hist(newStepsPerDay$steps, xlab="Steps Per Day", 
     breaks=15, main="Histogram of Steps Per Day with Imputed Values")
```

![](PA1_template_files/figure-html/New_Data-1.png)<!-- -->

```r
mean(newStepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(newStepsPerDay$steps)
```

```
## [1] 10765.59
```
## Are there differences in activity patterns between weekdays and weekends?
A column was added to determine weather the specific data was on a weekday, or a weekend.
The two plots show that there are many differences between the activity of the user on a weekday and a weekend.
For example, as you can see on the plots, this person is usually active earlier on weekdays, then on weekends.

```r
library(lattice)
data$DayOfWeek = factor(ifelse(weekdays(data$DateTime)=="Saturday" |
                                 weekdays(data$DateTime)=="Sunday",
                               "weekend",
                               "weekday"))
weekendData <- data[data$DayOfWeek=="weekend",]
weekdayData <- data[data$DayOfWeek=="weekday",]
IntStepsWeekend <- aggregate(steps~interval,weekendData,FUN=mean)
IntStepsWeekend$DayOfWeek <- "weekend"
IntStepsWeekday <- aggregate(steps~interval,weekdayData,FUN=mean)
IntStepsWeekday$DayOfWeek <- "weekday"
IntStepsTotal <- rbind(IntStepsWeekend,IntStepsWeekday)
xyplot(steps~interval | DayOfWeek, 
       data=IntStepsTotal, 
       ylab="Number of steps", 
       xlab="Interval",
       layout=c(1,2), 
       type="l")
```

![](PA1_template_files/figure-html/Weekend_Weekday-1.png)<!-- -->

