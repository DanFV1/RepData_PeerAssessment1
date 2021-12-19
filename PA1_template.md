---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


# Libraries

```r
library(dplyr)  
```

```
## Warning: package 'dplyr' was built under R version 4.0.5
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
library(lubridate)  
```

```
## Warning: package 'lubridate' was built under R version 4.0.5
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following objects are masked from 'package:base':
## 
##     date, intersect, setdiff, union
```

```r
library(ggplot2)
```

# Loading and preprocessing the data

```r
unzip("activity.zip")  
data <- read.csv("activity.csv")  
```

### Exploring Data

```r
dim(data)  
```

```
## [1] 17568     3
```

```r
names(data)  
```

```
## [1] "steps"    "date"     "interval"
```

```r
head(data)  
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
str(data)  
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

### Transforming the date column (using lubridate)

```r
data$date<-ymd(data$date)  
length(unique(data$date))  
```

```
## [1] 61
```

# Histogram of the total number of steps taken each day

```r
histData <- with(data,
                aggregate(steps, by = list(date),
                FUN = sum,
                na.rm = TRUE)
                )  
names(histData) <- c("date", "steps")  

hist(histData$steps,
        main = "Total number of steps taken by day",
        xlab = "Total steps taken per day",
        ylim = c(0,20),
        col = "blue",
        breaks = seq(0,25000, by=2500)
        )  
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

# What is mean total number of steps taken per day?
### Mean  

```r
mean(histData$steps)  
```

```
## [1] 9354.23
```

### Median  

```r
median(histData$steps)  
```

```
## [1] 10395
```

# What is the average daily activity pattern?  

```r
data_mean_dailyactivity <- aggregate(data$steps,
                                by=list(data$interval),
                                FUN=mean,
                                na.rm=TRUE)  
                                
names(data_mean_dailyactivity) <- c("interval", "mean")  

plot(data_mean_dailyactivity$interval,
        data_mean_dailyactivity$mean,
        type = "l",
        col="blue",
        lwd = 2,
        xlab="Interval",
        ylab="Average number of steps",
        main="Average number of steps per intervals")  
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

## Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  

```r
data_mean_dailyactivity[which.max(data_mean_dailyactivity$mean), ]$interval  
```

```
## [1] 835
```

# Imputing missing values  
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

```r
cleaned_steps <- data_mean_dailyactivity$mean[match(data$interval, data_mean_dailyactivity$interval)]
```

### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
data_cleaned <- transform(data,
                        steps = ifelse(is.na(data$steps),
                                yes = cleaned_steps, no = data$steps)
                                )
data_updated <- aggregate(steps ~ date, data_cleaned, sum)
names(data_updated) <- c("date", "daily_steps")
```

### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
hist(data_updated$daily_steps,
        main = "Total number of steps taken by day",
        xlab = "Total steps taken per day",
        ylim = c(0,20),
        col = "blue",
        breaks = seq(0,25000, by=2500)
        )
```

![](PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

#### Mean

```r
mean(data_updated$daily_steps)
```

```
## [1] 10766.19
```

#### Median

```r
median(data_updated$daily_steps)
```

```
## [1] 10766.19
```

### Are there differences in activity patterns between weekdays and weekends?
#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
data$datetype <- sapply(data$date, function(x) {
        if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
                {y <- "Weekend"} else 
                {y <- "Weekday"}
                y
        })
```

#### Make a panel plot containing a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
data_by_date <- aggregate(steps~interval + datetype, data, mean, na.rm = TRUE)
plot<- ggplot(data_by_date, aes(x = interval , y = steps, color = datetype)) +
       geom_line() +
       labs(title = "Average daily steps by Weekend/Weekday",
                x = "Interval",
                y = "Avg number of steps") +
       facet_wrap(~datetype, ncol = 1, nrow=2)
print(plot)
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png)<!-- -->
