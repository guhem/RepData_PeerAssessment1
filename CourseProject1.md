---
title: "CourseProject1"
date: "10 april 2019"
output: 
  html_document: 
    keep_md: yes
keep_md: true 
---



### Loading and Preprocessing the data


[Download data here](https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip) and save in your working directory.  

Read in datafile:


```r
unzip("repdata_data_activity.zip")
activity_pre <- read.csv("activity.csv")
```


```r
dim(activity_pre)
```

```
## [1] 17568     3
```
Remove NA in the data:

```r
activity<-activity_pre[with(activity_pre,{!(is.na(steps))}),]
```

And inspect file: 

```r
head(activity,10)
```

```
##     steps       date interval
## 289     0 2012-10-02        0
## 290     0 2012-10-02        5
## 291     0 2012-10-02       10
## 292     0 2012-10-02       15
## 293     0 2012-10-02       20
## 294     0 2012-10-02       25
## 295     0 2012-10-02       30
## 296     0 2012-10-02       35
## 297     0 2012-10-02       40
## 298     0 2012-10-02       45
```

```r
dim(activity)
```

```
## [1] 15264     3
```

```r
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-02:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-03:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-04:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-05:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-06:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-07:  288   Max.   :2355.0  
##                   (Other)   :13536
```

Load packages:

```r
library(ggplot2)
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

### What is the mean totalt number of steps taken per day?
Calculate the total number of steps taken per day:

```r
day <-group_by(activity,date)
steps_day<-summarise(day,total=sum(steps))
head(steps_day)
```

```
## # A tibble: 6 x 2
##   date       total
##   <fct>      <int>
## 1 2012-10-02   126
## 2 2012-10-03 11352
## 3 2012-10-04 12116
## 4 2012-10-05 13294
## 5 2012-10-06 15420
## 6 2012-10-07 11015
```

Make a histogram of the total number of steps taken each day:

```r
hist(steps_day$total, main="Total number of steps taken each day", xlab="Number of steps per day")
```

![](CourseProject1_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Calcualte and report the mean and median of the totalt number of steps taken per day:

```r
mean(steps_day$total)
```

```
## [1] 10766.19
```

```r
median(steps_day$total)
```

```
## [1] 10765
```

### What is the average daily activity pattern?

Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):

```r
steps_interval <- aggregate(steps ~ interval, activity, mean)
plot(steps_interval$interval, steps_interval$steps, type = "l", 
     main="Average number of steps taken across all days", xlab="5 minute interval",
     ylab="Average number og steps")
```

![](CourseProject1_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps:

```r
max_interval<-which.max(steps_interval$steps)
steps_interval[max_interval,]
```

```
##     interval    steps
## 104      835 206.1698
```
### Imputing missing values

Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity_pre))
```

```
## [1] 2304
```

Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

Create a new dataset that is equal to the original dataset but with the missing data filled in.

**Here I chose to use the mean for the 5 minute interval and create a new dataset called activity_imputed**

```r
activity_imputed <- activity_pre
for (i in 1:nrow(activity_imputed)) {
    if(is.na(activity_imputed$steps[i])) {
        value <- steps_interval$steps[which(steps_interval$interval == activity_imputed$interval[i])]
        activity_imputed$steps[i] <- value 
    }
}
```

View imputed file:

```r
dim(activity_imputed)
```

```
## [1] 17568     3
```

```r
head(activity_imputed)
```

```
##       steps       date interval
## 1 1.7169811 2012-10-01        0
## 2 0.3396226 2012-10-01        5
## 3 0.1320755 2012-10-01       10
## 4 0.1509434 2012-10-01       15
## 5 0.0754717 2012-10-01       20
## 6 2.0943396 2012-10-01       25
```

Calculate steps per day:

```r
imputed_steps_day <- aggregate(steps ~ date, activity_imputed, sum)
head(imputed_steps_day)
```

```
##         date    steps
## 1 2012-10-01 10766.19
## 2 2012-10-02   126.00
## 3 2012-10-03 11352.00
## 4 2012-10-04 12116.00
## 5 2012-10-05 13294.00
## 6 2012-10-06 15420.00
```

Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day.

```r
hist(imputed_steps_day$steps, main="Total number of steps taken each day (Imputed)", xlab="Number of steps per day")
```

![](CourseProject1_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

Mean and median:

```r
mean(imputed_steps_day$steps)
```

```
## [1] 10766.19
```

```r
median(imputed_steps_day$steps)
```

```
## [1] 10766.19
```
Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

**The mean is the same, but the meadian has slightly changed**


### Are there differences in activity patterns between weekdays and weekends?

Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.


```r
day <- weekdays(as.Date(activity_imputed$date))
daylevel <- vector()
for (i in 1:nrow(activity_imputed)) {
    if (day[i] == "Saturday") {
        daylevel[i] <- "Weekend"
    } else if (day[i] == "Sunday") {
        daylevel[i] <- "Weekend"
    } else {
        daylevel[i] <- "Weekday"
    }
}
activity_imputed$day_type  <- daylevel
head(activity_imputed,10)
```

```
##        steps       date interval day_type
## 1  1.7169811 2012-10-01        0  Weekday
## 2  0.3396226 2012-10-01        5  Weekday
## 3  0.1320755 2012-10-01       10  Weekday
## 4  0.1509434 2012-10-01       15  Weekday
## 5  0.0754717 2012-10-01       20  Weekday
## 6  2.0943396 2012-10-01       25  Weekday
## 7  0.5283019 2012-10-01       30  Weekday
## 8  0.8679245 2012-10-01       35  Weekday
## 9  0.0000000 2012-10-01       40  Weekday
## 10 1.4716981 2012-10-01       45  Weekday
```

Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
steps_day_type_impute <- aggregate(steps ~ interval + day_type, activity_imputed, mean)

plt <- ggplot(steps_day_type_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Interval", y=expression("No of Steps")) +
    ggtitle("Steps per Interval by day type")
print(plt)
```

![](CourseProject1_files/figure-html/unnamed-chunk-18-1.png)<!-- -->
