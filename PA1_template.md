---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
assignurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download.file(assignurl, destfile = "activity.zip", method = "curl")
unzip("activity.zip")
assigndata <- read.csv("activity.csv")
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
library(ggplot2)
```

## What is mean total number of steps taken per day?
Calculating the total number of steps taken per day

```r
data_per_date <- assigndata %>% select(date, steps) %>% group_by(date) %>% summarize(totalsteps = sum(steps)) %>% na.omit()
head(data_per_date)
```

```
## # A tibble: 6 x 2
##   date       totalsteps
##   <chr>           <int>
## 1 2012-10-02        126
## 2 2012-10-03      11352
## 3 2012-10-04      12116
## 4 2012-10-05      13294
## 5 2012-10-06      15420
## 6 2012-10-07      11015
```

Making a histogram of the total number of steps taken per day

```r
hist(data_per_date$totalsteps, xlab = "Total Daily Steps", ylab = "Frequency", main = "Histogram of Total Steps Taken per Day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

Calculating the mean and median of the total number of steps taken per day

```r
mean(data_per_date$totalsteps)
```

```
## [1] 10766.19
```

```r
median(data_per_date$totalsteps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
Making a time series plot of interval and average steps taken across all days

```r
data_per_interval <- assigndata %>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(meansteps = mean(steps))

ggplot(data_per_interval, aes(x = interval, y = meansteps)) + geom_line() + labs ( x = "Interval of Steps Taken") + labs (y = "Average Steps Taken") + labs (title = "Time-series plot of Interval and Average Steps Taken across all days")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Identifying the interval with maximum number of steps

```r
data_per_interval[which(data_per_interval$meansteps == max(data_per_interval$meansteps)),]
```

```
## # A tibble: 1 x 2
##   interval meansteps
##      <int>     <dbl>
## 1      835      206.
```


## Imputing missing values
Calculating the total number of missing values

```r
sapply(X = assigndata, FUN = function(x) sum(is.na(x)))
```

```
##    steps     date interval 
##     2304        0        0
```

Replacing all missing values with the mean of that interval

```r
replace_with_mean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
```

Creating new dataset with replaced missing values

```r
replaced_data <- assigndata %>% group_by(interval) %>% mutate(steps = replace_with_mean(steps))
```

Making a histogram of total number of steps taken each day with new replaced data

```r
replaced_data_per_date <- replaced_data %>% select(date, steps) %>% group_by(date) %>% summarize(totalsteps = sum(steps))
```

```
## Adding missing grouping variables: `interval`
```

```r
hist(replaced_data_per_date$totalsteps, xlab = "Total Daily Steps", ylab = "Frequency", main = "Histogram of Total Steps Taken per Day", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

Calculating mean and median of total steps taken each day of replaced data

```r
mean(replaced_data_per_date$totalsteps)
```

```
## [1] 10766.19
```

```r
median(replaced_data_per_date$totalsteps)
```

```
## [1] 10766.19
```

Finding the diffeence in mean and median between original and replaced data

```r
mean(replaced_data_per_date$totalsteps) - mean(data_per_date$totalsteps)
```

```
## [1] 0
```

```r
median(replaced_data_per_date$totalsteps) - median(data_per_date$totalsteps)
```

```
## [1] 1.188679
```


## Are there differences in activity patterns between weekdays and weekends?
Creating a factor variable to categorize date into weekday and weekend

```r
replaced_data$weekday_weekend <- ifelse(weekdays(as.Date(replaced_data$date)) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"), "Weekday", "Weekend")
```

Making a panel plot to compare interval and average steps taken by weekdays

```r
replaced_data_per_interval <- replaced_data %>% group_by(interval, weekday_weekend) %>% summarize(meansteps = mean(steps))
ggplot(replaced_data_per_interval, aes(x = interval, y = meansteps, color = weekday_weekend)) + geom_line() + facet_grid(weekday_weekend~.) + labs ( x = "Interval of Steps Taken") + labs (y = "Average Steps Taken") + labs (title = "Comparison of Intervals and Average Steps Taken by weekdays")
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png)<!-- -->
