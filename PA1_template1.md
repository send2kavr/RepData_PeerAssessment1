---
title: "Course Project 1"
output: 
        html_document:
                keep_md: true
---



## Reproducible Research - Course Project 1

This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

The variables included in this dataset are:

1. steps: Number of steps taking in a 5-minute interval (missing values are coded as 𝙽𝙰)
2. date: The date on which the measurement was taken in YYYY-MM-DD format
3. interval: Identifier for the 5-minute interval in which measurement was taken

The dataset is stored in a comma-separated-value (CSV) file and there are a total of 17,568 observations in this dataset.

### Q1: Code for reading in the dataset and/or processing the data

Make sure the data file is available in your current working directory.
Let's load the dataset and see the structure of the data. Here is the code snippet for the same:


```r
activity_data <- read.csv("activity.csv",na.strings = "NA",stringsAsFactors = FALSE)
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : chr  "2012-10-01" "2012-10-01" "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Looking at the structure we can see that the date column is of class character which needs to be transformed to date class. 
Using the lubridate package lets transform the date column to the correct format.


```r
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

```r
activity_data$date <- ymd(activity_data$date)
str(activity_data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

Now the data looks good with the required formats.

### Q2: Histogram of the total number of steps taken each day

Using the dplyr package lets group the steps by date and calculate the total number of steps taken each day.
Plot the Histogram for the Total steps taken per day.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:lubridate':
## 
##     intersect, setdiff, union
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
total_steps_data <- activity_data %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(total_steps_data$total_steps,main="Total steps taken per day",xlab = "Total Steps", col="green")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

### Q3: Mean and median number of steps taken each day

Using the dplyr package lets group the steps by date and calculate the mean and median of the number of steps taken each day. For this calculation we can omit the NA values. Also median is calculated taking into account only the steps > 0 as in few days we have more 0 values and median was getting calculated as 0 always.


```r
steps_by_date <- activity_data %>% group_by(date) %>% na.omit %>% summarise(mean_per_day = mean(steps),median_per_day = median(steps[steps>0]))
steps_by_date
```

```
## # A tibble: 53 x 3
##          date mean_per_day median_per_day
##        <date>        <dbl>          <dbl>
##  1 2012-10-02      0.43750           63.0
##  2 2012-10-03     39.41667           61.0
##  3 2012-10-04     42.06944           56.5
##  4 2012-10-05     46.15972           66.0
##  5 2012-10-06     53.54167           67.0
##  6 2012-10-07     38.24653           52.5
##  7 2012-10-09     44.48264           48.0
##  8 2012-10-10     34.37500           56.5
##  9 2012-10-11     35.77778           35.0
## 10 2012-10-12     60.35417           46.0
## # ... with 43 more rows
```

### Q4: Time series plot of the average number of steps taken

Assuming that we can omit NA values for this plot, using the dplyr package lets group the activity data by time interval and calculate the average steps.Then we can do a time series plot of the average number of steps taken.


```r
steps_by_interval <- activity_data %>% na.omit %>% group_by(interval) %>% summarise(average_steps = mean(steps))
plot(steps_by_interval$interval,steps_by_interval$average_steps,xlab="Time Interval",ylab="Average Steps",main = "Average Daily Activity Pattern",type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

### Q5: The 5-minute interval that, on average, contains the maximum number of steps

As seen from the plot above we have a maximum value of 200 between 500 and 1000 interval. Lets use the which.max function to calculate the same.


```r
steps_by_interval[which.max(steps_by_interval$average_steps),]
```

```
## # A tibble: 1 x 2
##   interval average_steps
##      <int>         <dbl>
## 1      835      206.1698
```

As we can see the maximum average steps is 206.1698 and the time interval is 835.

### Q6: Code to describe and show a strategy for imputing missing data

Lets find out the count of the missing values from the activity dataset.


```r
sum(is.na(activity_data$steps))
```

```
## [1] 2304
```

As we can see there is a total of 2304 missing values.
We will use the strategy of replacing missing values for an interval with the mean of that 5-minute interval. The mean of the 5-minute interval was already calculated in Question 5.
Lets first merge the activity data set with the steps_by_interval dataset. Then we can replace any missing values in the steps column with the corresponding average steps column.


```r
new_df <- merge(activity_data,steps_by_interval,by="interval")
for(i in 1:nrow(new_df)) { if(is.na(new_df[i,2])) {new_df[i,2] <- new_df[i,4]} }
sum(is.na(new_df))
```

```
## [1] 0
```

As you can see we do not have any missing values in the new dataframe created in the above step.

### Q7: Histogram of the total number of steps taken each day after missing values are imputed

Lets plot the total steps histogram again with the new dataset.


```r
total_steps_with_newdf <- new_df %>% group_by(date) %>% summarise(total_steps = sum(steps))
hist(total_steps_with_newdf$total_steps,main="Total steps taken per day",xlab = "Total Steps", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

If we take a look at the summary of the total steps taken before and after imputing missing values we can see slight variations in the numbers. But since the total NA values was less this didnt had much impact in the calculation.


```r
summary(total_steps_with_newdf)
```

```
##       date             total_steps   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 9819  
##  Median :2012-10-31   Median :10766  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:12811  
##  Max.   :2012-11-30   Max.   :21194
```

```r
summary(total_steps_data)
```

```
##       date             total_steps   
##  Min.   :2012-10-01   Min.   :   41  
##  1st Qu.:2012-10-16   1st Qu.: 8841  
##  Median :2012-10-31   Median :10765  
##  Mean   :2012-10-31   Mean   :10766  
##  3rd Qu.:2012-11-15   3rd Qu.:13294  
##  Max.   :2012-11-30   Max.   :21194  
##                       NA's   :8
```

### Q8: Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

Lets create a factor variable with 2 values "weekday" and "weekend". Add a new column in the new dataset created removing the missing values which classifies as weekday or weekend for that day.


```r
weekdays <- c("weekday","weekend")
weekday <- factor(weekdays)
new_df <- new_df %>% mutate(weekday = weekday[1])
for(i in 1:nrow(new_df)) { if((weekdays.Date(new_df[i,3]) == "Saturday") || (weekdays.Date(new_df[i,3]) == "Sunday")) {new_df[i,5] <- weekday[2]} }
str(new_df)
```

```
## 'data.frame':	17568 obs. of  5 variables:
##  $ interval     : int  0 0 0 0 0 0 0 0 0 0 ...
##  $ steps        : num  1.72 0 0 0 0 ...
##  $ date         : Date, format: "2012-10-01" "2012-11-23" ...
##  $ average_steps: num  1.72 1.72 1.72 1.72 1.72 ...
##  $ weekday      : Factor w/ 2 levels "weekday","weekend": 1 1 2 1 2 1 2 1 1 2 ...
```

Lets calculate the average steps for weekend and weekdays and plot the average steps against the 5-minute interval.


```r
meanSteps <- aggregate(new_df$steps, list(as.numeric(new_df$interval),new_df$weekday), FUN = "mean")
names(meanSteps) <- c("interval","dayOfWeek","avgSteps")
library(lattice)
xyplot(meanSteps$avgSteps ~ meanSteps$interval | meanSteps$dayOfWeek,layout = c(1, 2), type = "l", xlab = "Interval", ylab = "Number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
