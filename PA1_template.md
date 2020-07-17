---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data
First, we load the necessary packages.  
*tidyverse* contains dplyr and ggplot2 to help us clean the data.  
*lubridate* will help us parse date and time variables.  

```r
library(tidyverse)
library(lubridate)
```
  
  
Now, we load the data from the zip file "activity.zip".  
We also clean the data:  
* Convert *date* from character class to date class  
* Derive the variable, *tod*, to identify the time of day from the interval variable  
* Derive a date-time variable, *dt*, for each unique interval per day  

```r
activity <- as_tibble(read.csv(unz("activity.zip", "activity.csv"))) %>%
    mutate(date = as.Date(date),
           tod = parse_time(str_pad(interval, 4, "left", "0"), format = "%H%M"),
           dt = as_datetime(date) + tod,
           tod = as.POSIXct(tod))
```
  
  
## What is mean total number of steps taken per day?
We calculate the total number of steps taken per day and save this to "daily".

```r
daily <- activity %>%
    group_by(date) %>%
    summarize(steps = sum(steps, na.rm = TRUE))
```
  
  
The distribution and summary of the total steps per day is as follows:

```r
qplot(x = steps, data = daily, binwidth = 2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
ggsave("1 histogram1.png")
```

```
## Saving 7 x 5 in image
```

```r
summary(daily$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
  
  
The mean daily total is 9354.23 steps.  
The median daily total is 10395 steps.  
  
  
## What is the average daily activity pattern?
We would like to find out which 5-minute interval of the day has the highest average number of steps taken.  
To calculate this, we group our data by the 5-minute interval and take the mean across all days.  

```r
interval <- activity %>%
    group_by(tod) %>%
    summarize(steps = mean(steps, na.rm = TRUE))

intsum <- interval %>%
    mutate(tod = strftime(tod, format = "%I:%M %p", tz = "GMT")) %>%
    arrange(desc(steps))
```
  
  
The following time series plot shows the average daily pattern:

```r
ggplot(interval, aes(tod, steps)) +
    geom_path(size = 1.2) +
    scale_x_datetime(breaks = "4 hours", date_labels = "%I:%M %p")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
ggsave("2 time.png")
```

```
## Saving 7 x 5 in image
```
  
  
The 5-minute interval 08:35 AM has the highest average
daily total step count of 206.17.  
  
  
## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA).  
The presence of missing days may introduce bias into some calculations or summaries of the data.  

```r
activity %>%
    mutate(missingvalue = is.na(steps)) %>%
    summarize(missing_values = sum(missingvalue)) %>%
    print()
```

```
## # A tibble: 1 x 1
##   missing_values
##            <int>
## 1           2304
```
  
  
To fill in all of the missing values in the dataset, we use the mean for respective
5-minute interval across all days.  
We add these imputed values to the original dataset.  
  

```r
imputed <- activity %>%
    group_by(interval) %>%
    mutate(interval_mean = as.integer(round(mean(steps, na.rm = TRUE), 0)),
           imputed_values = if_else(is.na(steps), interval_mean, steps))

imputeddaily <- imputed %>%
    group_by(date) %>%
    summarize(imputed_steps = sum(imputed_values, na.rm = TRUE))
```
  
  
The distribution and summary of the imputed total steps per day is as follows:

```r
qplot(x = imputed_steps, data = imputeddaily, binwidth = 2000)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
ggsave("3 histogram2.png")
```

```
## Saving 7 x 5 in image
```

```r
summary(daily$steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##       0    6778   10395    9354   12811   21194
```
  
  
The re-calculated mean daily total is 10765.64 steps.  
The re-calculated median daily total is 10762 steps.  
  
We found that both of the re-calculated mean and median statistics are higher than
our initial estimates. This is arguably expected, as missing values could only
be filled with non-negative observations, so the actual daily totals would be at 
least equal to, or greater than what we had initially estimated.  
  
  
## Are there differences in activity patterns between weekdays and weekends?
We create a new factor variable, "wkday", in the dataset to indicate whether a given
date is a weekday or weekend day.


```r
byweekday <- imputed %>%
    mutate(wkday = if_else(weekdays(date) %in% c("Saturday", "Sunday"),
                             "Weekend", "Weekday"),
           wkday = as.factor(wkday)) %>%
    group_by(tod, wkday) %>%
    summarize(imputed_steps = mean(imputed_values, na.rm = TRUE))
```
  
We find that the most significant difference between weekday and weekend activities
is in the early morning.

```r
ggplot(byweekday, aes(x = tod, y = imputed_steps)) +
    geom_path(size = 1) +
    facet_wrap(wkday ~ ., nrow = 2) +
    scale_x_datetime(breaks = "4 hours", date_labels = "%I:%M %p")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

```r
ggsave("4 weekday.png")
```

```
## Saving 7 x 5 in image
```
